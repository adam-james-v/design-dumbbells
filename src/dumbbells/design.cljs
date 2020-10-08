(ns ^:figwheel-hooks dumbbells.design
  (:require [reagent.dom :as rdom]
            [reagent.core :as r]
            [reagent.ratom :refer [reaction]]
            [stylo.shape :as shape]
            [stylo.svg :as svg]
            [stylo.parser :as parser]
            [baton.core :as b]
            [forge.proto :as f]))

(def g-sc (r/atom 25))
(def g-plate-r (r/atom 2.5))
(def g-plate-t (r/atom 0.115))
(def g-density (r/atom 0.285))
(def g-view-rot (r/atom 0))
(def g-desired-mass (r/atom 5))

(def state
  (r/atom
   {:scale 25
    :r @g-plate-r}))

(declare calculate-plate-n)
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
  [r]
  (hex-plate r (/ 1.315 2) @g-plate-t))

(defn handle
  [n]
  (let [h (+ 5.5 (* n @g-plate-t))] 
    (tube-rnd (/ 1.315 2) 0.1 h)))

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

(defn total-mass
  []
  (let [pn (calculate-plate-n @g-plate-r @g-plate-t @g-desired-mass 0.285)
        plate (* (plate-volume (plate @g-plate-r)) @g-density)
        handle (* (tube-volume (handle pn)) @g-density)]
    (+ (* pn plate) handle)))

(defn plate-iso
  []
  [:<>
   (svg/fig
    1 "Isometric View of Hex Plate"
    (svg/dwg-2d
     [250 250 1]
     (shape/axes-iso)
     (svg/scale 
      @g-sc
      (svg/g
       (shape/render-curves 
        (-> (plate @g-plate-r)
            (f/rotate [0 90 0]))
        shape/isometric-xf 
        "black")))))])

(defn plate-front
  [state]
  [:<>
   (svg/fig
    1 "Front View of Hex Plate"
    (svg/dwg-2d
     [250 250 1]
     (svg/scale
      @g-sc #_(:scale state)
      (svg/g

       (->> (svg/text (str @g-plate-r "in"))
            (svg/translate [-2 5])
            (svg/scale 0.125))

       (shape/render-curves 
        (-> (plate @g-plate-r #_(:r state))
            (f/rotate [0 0 30]))
        shape/front-xf 
        "black")))))])

(defn handle-iso
  []
  (let [pn (calculate-plate-n @g-plate-r @g-plate-t @g-desired-mass 0.285)]
    [:<>
     (svg/fig
      2 "Isometric View of Handle"
      (svg/dwg-2d
       [250 250 1]
       (shape/axes-iso)
       (svg/scale 
        @g-sc
        (svg/g
         (shape/render-curves 
          (-> (handle pn)
              (f/translate [0 0 (/ (+ 5 (* pn @g-plate-t)) -2.0)])
              (f/rotate [22.5 90 0]))
          shape/isometric-xf
          "black")))))]))

(defn view-controls []
  [:div
   [:div 
    [b/slider g-sc 1 100 1]
    [:span "zoom: " @g-sc]]
   [:div 
    [b/slider g-view-rot 0 360 1]
    [:span "rotate: " @g-view-rot]]]) 

(defn parameters []
  [:div
   [:div 
    [b/slider g-plate-t 0.085 0.75 0.005]
    [:span "plate t: " @g-plate-t]]
   [:div 
    [b/slider g-plate-r 1.5 5.0 0.25]
    [:span "plate R: " @g-plate-r]]
   [:div
    [b/slider g-desired-mass 5 100 2.5]
    [:span "Desired Mass: " @g-desired-mass " lbs"]]])

(defn calculate-plate-n
  [r t mass density]
  (let [p (plate r)
        h (tube-rnd (/ 1.315 2) 0.1 t)
        um (* (+ (tube-volume h) (plate-volume p)) density)
        ns (for [n (range 2 60 2)]
             (let [hm (* (tube-volume (handle n)) @g-density)]
               [(- (- mass hm) (* um n)) n]))]
    (second (first (sort-by first (filter #(pos? (first %)) ns))))))

(defn calculate-hex-r
  [min-r max-r t mass density]
  (let [f #(* (plate-volume (plate %)) density)
        rs (for [r (range min-r max-r 0.01)]
             [(Math/abs (- (f r) mass)) r])]
    (second (first (sort-by first rs)))))

(defn design
  []
  (let [pn (calculate-plate-n @g-plate-r @g-plate-t @g-desired-mass 0.285)
        length (+ 5.5 (* pn @g-plate-t))
        actual-mass (total-mass)
        handle (-> (handle pn)
                   (f/translate [0 0 (/ length -2.0)])
                   (f/rotate [22.5 90 0]))
        plate (-> (plate @g-plate-r)
                  (f/rotate [0 90 0]))
        r-mass (/ (- @g-desired-mass actual-mass) 2)
        small-plate-r (calculate-hex-r 0.75 @g-plate-r @g-plate-t r-mass @g-density) 
        small-plate (-> (plate small-plate-r)
                        (f/rotate [0 90 0]))
        rstack (reduce 
                f/union
                (for [n (range (/ pn 2))]
                  (f/translate 
                   plate 
                   [(+ (- (/ length 2)) (* n @g-plate-t)) 0 0])))
        lstack (f/rotate rstack [0 180 0])]
    (reduce f/union [small-plate lstack handle rstack])))

(defn design-iso
  []
  [:<>
   (svg/fig
    2 "Isometric View of Handle"
    (svg/dwg-2d
     [300 250 1]
     (shape/axes-iso)
     (svg/scale 
      @g-sc
      (svg/g
       (shape/render-curves 
        (-> (design) (f/rotate [0 0 @g-view-rot]))
        shape/isometric-xf
        "black")))))])

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
  (let [pn (calculate-plate-n @g-plate-r @g-plate-t @g-desired-mass 0.285)]
    [:<>
     intro-component
     [plate-front @state]
     [parameters]
     #_[design-iso]
     [view-controls]
     [:p "N: " pn]
     [:p "Actual Mass: " (f/round (total-mass) 3)]
     [:p "------------"]
     [:p "Single Plate Mass: " (f/round (*
                                         (plate-volume (plate @g-plate-r))
                                         @g-density)
                                        3)]
     [:p "All Plates Mass: " (f/round (* 
                                       (plate-volume (plate @g-plate-r))
                                       (calculate-plate-n @g-plate-r @g-plate-t @g-desired-mass 0.285)
                                       @g-density)
                                      3)]
   [:p "Handle Mass: " (f/round (*
                                 (tube-volume (handle pn))
                                 @g-density)
                                3)]]))

(b/mount doc)
(defn ^:after-load re-render [] (b/mount doc))
(defonce go (do (b/mount doc) true))
