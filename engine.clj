(ns engine
  (:import (java.awt Color)))

(def width 75)
(def height 50)
(def point-size 10)
(def turn-millis 75)

(def black (Color. 0 0 0))
(def blue (Color. 0 0 255))
(def yellow (Color. 255 255 0))
(def white (Color. 255 255 255))

(defn add-points [& pts]
  (vec (apply map + pts)))

(defn create-cycle [name pt dir color]
  {:name name
   :trail [pt]
   :dir dir
   :color color})

(defn create-cycles []
  [(create-cycle "Player 1" [37 50] [0 -1] yellow)
   (create-cycle "Player 2" [37 0] [0 1] blue)])

(defn move [{:keys [trail dir] :as cycle}]
  (let [new-pt (add-points dir (first trail))
	new-trail (cons new-pt trail)]
    (assoc cycle :trail new-trail)))

(defn hit-wall? [{[[x y] & _] :trail}]
  (or
   (neg? x)
   (neg? y)
   (> x width)
   (> y height)))

(defn hit-trail? [{[pt & trail] :trail :as cycle} cycles]
  (let [other-cycles (disj (set cycles) cycle)
	other-trails (mapcat :trail other-cycles)
	trails (concat trail other-trails)]
    (some #{pt} trails)))

(defn dead? [cycle cycles]
  (or
   (hit-wall? cycle)
   (hit-trail? cycle cycles)))

(defn turn [{[cur-pt prev-pt & _] :trail :as cycle} new-dir]
  (let [new-pt (add-points new-dir cur-pt)]
    (if (= new-pt prev-pt) cycle
	(assoc cycle :dir new-dir))))

;; mutable state ahead
(defn reset-game [cycles-ref]
  (dosync (ref-set cycles-ref (create-cycles))))

(defn update-direction [cycles-ref name new-dir]
  (let [old-cycle (first (filter #(= name (:name %)) @cycles-ref))
	new-cycle (turn old-cycle new-dir)
	replace-cycle (partial replace {old-cycle new-cycle})]
    (dosync (alter cycles-ref replace-cycle))))

(defn update-positions [cycles-ref]
  (let [move-all (partial map move)]
    (dosync (alter cycles-ref move-all))))

(defn clear-dead [cycles-ref]
  (let [cycles @cycles-ref
	remove-dead (partial remove #(dead? % cycles))]
    (dosync (alter cycles-ref remove-dead))))
