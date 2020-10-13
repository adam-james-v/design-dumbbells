(ns ^:figwheel-hooks dumbbells.design
  (:require [reagent.dom :as rdom]
            [reagent.core :as r]
            [reagent.ratom :refer [reaction]]
            [stylo.shape :as shape]
            [stylo.svg :as svg]
            [stylo.parser :as parser]
            [baton.core :as b]
            [forge.proto :as f]))

(def state
  (r/atom
   {:scale 25
    :view-rotation 0

    :plate-radius 2.5
    :plate-thickness 0.115
    :plate-n 4

    :handle-radius (/ 1.315 2)
    :handle-thickness 0.083
    :grip-width 6.5

    :material-density 0.285
    :desired-mass 5}))

(defn hex-plate
  [or ir t]
  (let [sk (f/union
            (f/circle ir)
            (f/polygon
             (f/regular-polygon-pts or 6)))]
    (-> sk
        (f/extrude t)
        (f/rotate [0 0 30])
        (f/translate [0 0 0]))))

(defn tube-rnd
  [r t h]
  (-> (f/union (f/circle r) 
               (f/circle (- r t)))
      (f/extrude h)))

(defn plate
  [state]
  (let [or (r/cursor state [:plate-radius])
        ir (r/cursor state [:handle-radius])
        t (r/cursor state [:plate-thickness])]
    (hex-plate @or @ir @t)))

(defn handle
  [state]
  (let [r (r/cursor state [:handle-radius])
        t (r/cursor state [:handle-thickness])
        l1 (r/cursor state [:grip-width])
        pn (r/cursor state [:plate-n])
        pt (r/cursor state [:plate-thickness])
        l2 (* @pt @pn)]
    (tube-rnd @r @t (+ @l1 l2))))

(declare update-plate-n!)
(defn assembly
  [state]
  (let [plate-r (r/cursor state [:plate-radius])
        plate-t (r/cursor state [:plate-thickness])
        grip-w (r/cursor state [:grip-width])

        plate-n (update-plate-n! state 60) #_(r/cursor state [:plate-n])
        handle-l (+ @grip-w (* plate-n @plate-t))

        grip (-> (handle state)
                 (f/translate [0 0 (/ handle-l -2.0)])
                 (f/rotate [22.5 90 0]))
        hex (-> (plate state)
                (f/rotate [0 90 0]))
        
        rstack (reduce 
                f/union
                (for [n (range (/ plate-n 2))]
                  (f/translate 
                   hex 
                   [(+ (- (/ handle-l 2)) (* n @plate-t)) 0 0])))
        lstack (f/rotate rstack [0 180 0])]
    
    (reduce f/union [lstack grip rstack])))

(defn plate-volume
  [plate]
  (let [[bvmax bvmin] (f/bounding-box-corners plate)
        height (Math/abs (- (last bvmax) (last bvmin)))
        pts (last (first (:history (last (first (:history plate))))))
        a1 (f/polygon-area pts)
        ir (last (first (:history (second (first (:history plate))))))
        a2 (* Math/PI (f/sq ir))]
    (* (- a1 a2) height)))

(defn tube-volume
  [tube]
  (let [[bvmax bvmin] (f/bounding-box-corners tube)
        height (Math/abs (- (last bvmax) (last bvmin)))
        or (last (first (:history (second (first (:history tube))))))
        a1 (* Math/PI (f/sq or))
        ir (last (first (:history (last (first (:history tube))))))
        a2 (* Math/PI (f/sq ir))]
    (* (- a1 a2) height)))

(defn calculate-plate-n
  [state max]
  (let [desired-mass (r/cursor state [:desired-mass])
        density (r/cursor state [:material-density])
        
        handle-r (r/cursor state [:handle-radius])
        handle-t (r/cursor state [:handle-thickness])
        plate-t (r/cursor state [:plate-thickness])
        grip-w (r/cursor state [:grip-width])

        plate-m (* (plate-volume (plate state)) 
                   @density)
        grip-m (* (tube-volume (tube-rnd @handle-r @handle-t @grip-w)) 
                  @density)
        handle-slice-m (* (tube-volume (tube-rnd @handle-r @handle-t @plate-t)) 
                          @density)
        
        ns (for [n (range 2 max 2)]
             (let [total-m (+ grip-m (* n (+ plate-m handle-slice-m)))]
               [(- @desired-mass total-m) n]))]
    (second (first (sort-by first (filter #(pos? (first %)) ns))))))

(defn update-plate-n!
  [state max]
  (let [new-n (calculate-plate-n state max)
        n (r/cursor state [:plate-n])]
    (reset! n new-n)
    new-n))

#_(defn calculate-hex-r
  [min-r max-r t mass density]
  (let [f #(* (plate-volume (plate %)) density)
        rs (for [r (range min-r max-r 0.01)]
             [(Math/abs (- (f r) mass)) r])]
    (second (first (sort-by first rs)))))

(defn total-mass
  [state]
  (let [density (r/cursor state [:material-density])
        pn (calculate-plate-n state 60)
        plate  (* (plate-volume (plate state)) @density)
        handle (* (tube-volume (handle state)) @density)]
    (+ (* pn plate) handle)))

(defn handle-front
  [state]
  (let [zoom (r/cursor state [:scale])
        handle-r (r/cursor state [:handle-radius])]
    [:<>
     (svg/fig
      1 "Front View of Handle"
      (svg/dwg-2d
       [200 100 1]
       (svg/scale
        @zoom
        (svg/g
         (svg/style-element
           {:stroke "blue"
            :fill "blue"}
           (svg/g
            (->> (svg/text "r") 
                 (svg/translate [(+ -4.125 @handle-r) 4.65])
                 (svg/scale 0.05))
            (svg/line [0 0] [@handle-r 0])))
        
         (shape/render-curves 
          (handle state)
          shape/top-xf
          "black")))))]))

(defn handle-right
  [state]
  (let [zoom (r/cursor state [:scale])
        grip-w (r/cursor state [:grip-width])]
    [:<>
     (svg/fig
      2 "Right View of Handle"
      (svg/dwg-2d
       [250 100 1]
       (svg/scale
        @zoom
        (svg/translate 
         [-100 0]
         (svg/g
          
          (svg/style-element
           {:stroke "blue"
            :fill "blue"}
           (svg/g
            (->> (svg/text "L") 
                 (svg/translate [(+ -4.375 (/ @grip-w 2)) 3.375])
                 (svg/scale 0.05))
            (svg/line [0 -1] [@grip-w -1])))
  
          (svg/style-element
           {:stroke "red"
            :fill "red"}
           (svg/g
            (->> (svg/text "GRIP-WIDTH") 
                 (svg/translate [(+ (* 10 -4.375) -1.75 (/ @grip-w 2)) 6.25])
                 (svg/scale 0.05))
            (svg/line [0 1] [@grip-w 1])))
         
          (shape/render-curves 
           (-> (handle state)
               (f/translate [0 0 0])
               (f/rotate [0 90 0]))
           shape/right-xf
           "black"))))))]))

(defn plate-front
  [state]
  (let [zoom (r/cursor state [:scale])
        plate-r (r/cursor state [:plate-radius])]
    [:<>
     (svg/fig
      3 "Front View of Hex Plate"
      (svg/dwg-2d
       [250 250 1]
       (svg/scale
        @zoom
        (svg/g
         
         (svg/style-element
          {:stroke "blue"
           :fill "blue"}
          (svg/g
           (->> (svg/text "R") 
                (svg/translate [(+ -4.5 (/ @plate-r 2)) 4.375])
                (svg/scale 0.05))
           (svg/line [0 0] [@plate-r 0])))

         (shape/render-curves
          (-> (plate state)
              (f/rotate [0 0 30]))
          shape/top-xf
          "black")))))]))

(defn plate-right
  [state]
  (let [zoom (r/cursor state [:scale])
        plate-r (r/cursor state [:plate-radius])]
    [:<>
     (svg/fig
      4 "Right View of Hex Plate"
      (svg/dwg-2d
       [250 250 1]
       (svg/scale
        @zoom
        (svg/g
         
         (svg/style-element
          {:stroke "blue"
           :fill "blue"}
          (svg/g
           (->> (svg/text "T") 
                (svg/translate [(+ -4.5 (/ @plate-r 2)) 4.375])
                (svg/scale 0.05))
           (svg/line [0 0] [@plate-r 0])))

         (shape/render-curves
          (-> (plate state)
              (f/rotate [0 90 30]))
          shape/right-xf
          "black")))))]))

(defn assembly-iso
  [state]
  (let [zoom (r/cursor state [:scale])
        rotation (r/cursor state [:view-rotation])]
    [:<>
     (svg/fig
      2 "Isometric View of Assembly"
      (svg/dwg-2d
       [300 250 1]
       (shape/axes-iso)
       (svg/scale 
        @zoom
        (svg/g
         (shape/render-curves 
          (-> (assembly state) 
              (f/rotate [0 0 @rotation]))
          shape/isometric-xf
          "black")))))]))

(defn view-controls [state]
  (let [zoom   (r/cursor state [:scale])
        rot    (r/cursor state [:view-rotation])]
    [:div
     [:div 
      [b/slider zoom 1 100 1]
      [:span "  zoom: " @zoom]]
     [:div 
      [b/slider rot 0 360 1]
      [:span "rotate: " @rot]]])) 

(defn handle-parameters [state]
  (let [grip-w (r/cursor state [:grip-width])
        handle-r (r/cursor state [:handle-radius])
        handle-t (r/cursor state [:handle-thickness])]
    [:div
     [:div 
      [b/slider grip-w 4.0 12.0 0.25]
      [:span "grip width: " @grip-w]]
     [:div
      [b/slider handle-r 1.0 2.5 0.15]
      [:span "handle r: " @handle-r]]
     [:div
      [b/slider handle-t 0.065 0.25 0.005]
      [:span "handle thickness: " @handle-t]]]))

(defn plate-parameters [state]
  (let [plate-r (r/cursor state [:plate-radius])
        plate-t (r/cursor state [:plate-thickness])]
    [:div
     [:div
      [b/slider plate-r 2.0 16.0 0.125]
      [:span "plate R: " @plate-r]]
     [:div
      [b/slider plate-t 0.065 0.25 0.005]
      [:span "plate thickness: " @plate-t]]]))
  
(defn parameters [state]
  (let [zoom (r/cursor state [:scale])
        rot (r/cursor state [:view-rotation])
        plate-t (r/cursor state [:plate-thickness])
        plate-r (r/cursor state [:plate-radius])
        desired-mass (r/cursor state [:desired-mass])
        plate-n (r/cursor state [:plate-n])]
    [:div
     [:div 
      [b/slider plate-t 0.085 0.75 0.005]
      [:span "plate t: " @plate-t]]
     [:div
      [b/slider plate-r 1.5 5.0 0.25]
      [:span "plate R: " @plate-r]]
     [:div
      [b/slider desired-mass 5 100 2.5]
      [:span "Desired Mass: " @desired-mass " lbs"]]
     [:div
      [:span "Number of plates : " @plate-n]]]))

(def title "## Dumbbells Parametric Design Doc.")
(def intro (str 
"This design is parametric in the desired mass. Given a plate thickness, maximum radius of hex shapes, and the desired mass of the finished dumbbell, this app will calculate the design for you.

You can use this in your browser, just try adjusting some settings!
"))

(defn markdown
  [& strings]
  (mapcat (comp parser/->hiccup parser/doc-parse) strings))

(def intro-component
  (markdown title intro))

(defn doc []
  [:<>
   intro-component
   
   [:section
    (markdown
     "### Handle Configuration"
     
     "Set the parameters according to the tube size you will be using for the handle part.
")

    [handle-parameters state]
    [:div {:style {:display "flex"}}
     [handle-front state]
     [handle-right state]]]

   [:section
    (markdown
     "### Hex Plate Configuration"
     
     "Set the parameters according to the flat sheet you will be using to make the plates.
")

    [plate-parameters state]
    [:div {:style {:display "flex"}}
     [plate-front state]
     [plate-right state]]]
    
   
   [parameters state]
   
   
   [assembly-iso state]
   [view-controls state]])

(b/mount doc)
(defn ^:after-load re-render [] (b/mount doc))
(defonce go (do (b/mount doc) true))
