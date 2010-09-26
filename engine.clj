(ns engine
  (:import (java.awt Color)))

(def width 75)
(def height 50)
(def point-size 10)
(def turn-millis 75)

(def black (Color. 0 0 0))
(def blue (Color. 0 0 255))
(def red (Color. 255 0 0))
(def yellow (Color. 255 255 0))
(def white (Color. 255 255 255))

(defn add-points [& pts]
  (vec (apply map + pts)))

(defn create-cycle [name pt dir color move-fn]
  {:name name
   :trail [pt]
   :dir dir
   :color color
   :move-fn move-fn})

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

(defn move [{:keys [trail dir] :as cycle} cycles]
  (let [new-pt (add-points dir (first trail))
	new-trail (cons new-pt trail)]
    (assoc cycle :trail new-trail)))

;; AI bot move function
(defn move-random [cycle cycles]
  (let [dirs [[0 -1] [-1 0] [0 1] [1 0]]
	biased-dirs (concat dirs (repeat 8 (:dir cycle)))
	make-move #(move (turn cycle %) cycles)
	next-moves (map make-move biased-dirs)
	valid-moves (remove #(dead? % cycles) next-moves)]
    (if-not (empty? valid-moves) (rand-nth valid-moves)
	    (move cycle cycles))))

(defn create-cycles []
  [(create-cycle "Player 1" [37 50] [0 -1] yellow move)
   (create-cycle "Player 2" [37 0] [0 1] blue move)
   (create-cycle "Rand 1" [0 24] [1 0] red move-random)
   (create-cycle "Rand 2" [75 24] [-1 0] red move-random)])

;; mutable state ahead
(defn reset-game [cycles-ref]
  (dosync (ref-set cycles-ref (create-cycles))))

(defn update-direction [cycles-ref name new-dir]
  (let [old-cycle (first (filter #(= name (:name %)) @cycles-ref))
	new-cycle (turn old-cycle new-dir)
	replace-cycle (partial replace {old-cycle new-cycle})]
    (dosync (alter cycles-ref replace-cycle))))

(defn update-positions [cycles-ref]
  (let [cycles @cycles-ref
	apply-move-fns (partial map #((:move-fn %) % cycles))]
    (dosync (alter cycles-ref apply-move-fns))))

(defn clear-dead [cycles-ref]
  (let [cycles @cycles-ref
	remove-dead (partial remove #(dead? % cycles))]
    (dosync (alter cycles-ref remove-dead))))
