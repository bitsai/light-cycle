(ns engine
  (:import (java.awt Color)))

(def width 75)
(def height 50)
(def point-size 10)
(def turn-millis 75)

(def black (Color. 0 0 0))
(def blue (Color. 0 0 255))
(def orange (Color. 255 165 0))
(def yellow (Color. 255 255 0))

(defn add-points [& pts]
  (vec (apply map + pts)))

(defn point-to-screen-rect [[x y]]
  (map #(* point-size %) [x y 1 1]))

(defn create-cycle [pt dir color]
  {:trail [pt]
   :dir dir
   :color color})

(defn create-p1 []
  (create-cycle [37 50] [0 -1] yellow))

(defn create-p2 []
  (create-cycle [37 0] [0 1] blue))

(defn move [{:keys [trail dir] :as cycle}]
  (let [new-pt (add-points dir (first trail))]
    (assoc cycle :trail (cons new-pt trail))))

(defn get-winner [p1 p2]
  (cond
   (and (nil? p1) (nil? p2)) "Tie!"
   (nil? p1) "P2 wins!"
   (nil? p2) "P1 wins!"))

(defn hit-wall? [{[[x y] & _] :trail}]
  (or
   (neg? x)
   (neg? y)
   (> x width)
   (> y height)))

(defn hit-trails? [{[pt & trail] :trail} other-cycle]
  (let [trails (concat trail (:trail other-cycle))]
    (some #(= pt %) trails)))

(defn lose? [cycle other-cycle]
  (or
   (nil? cycle)
   (hit-wall? cycle)
   (hit-trails? cycle other-cycle)))

(defn turn [{[cur-pt prev-pt & _] :trail :as cycle} new-dir]
  (let [new-pt (add-points new-dir cur-pt)]
    (if (= new-pt prev-pt) cycle
	(assoc cycle :dir new-dir))))

;; mutable state ahead
(defn reset-game [p1 p2]
  (dosync (ref-set p1 (create-p1))
	  (ref-set p2 (create-p2)))
  nil)

(defn update-direction [cycle new-dir]
  (when new-dir
    (dosync (alter cycle turn new-dir))))

(defn update-positions [p1 p2]
  (dosync
   (let [p1-lose (lose? @p1 @p2)
	 p2-lose (lose? @p2 @p1)]
     (if p1-lose (ref-set p1 nil)
	 (alter p1 move))
     (if p2-lose (ref-set p2 nil)
	 (alter p2 move))))
  nil)
