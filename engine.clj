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

(defn create-p1 []
  (create-cycle "Player 1" [37 50] [0 -1] yellow))

(defn create-p2 []
  (create-cycle "Player 2" [37 0] [0 1] blue))

(defn move [{:keys [trail dir] :as cycle}]
  (let [new-pt (add-points dir (first trail))]
    (assoc cycle :trail (cons new-pt trail))))

(defn hit-wall? [{[[x y] & _] :trail}]
  (or
   (neg? x)
   (neg? y)
   (> x width)
   (> y height)))

(defn hit-cycle? [{[pt & trail] :trail} other-cycles]
  (let [other-trails (mapcat :trail other-cycles)
	trails (concat trail other-trails)]
    (some #(= pt %) trails)))

(defn dead? [cycle other-cycles]
  (or
   (nil? cycle)
   (hit-wall? cycle)
   (hit-cycle? cycle other-cycles)))

(defn turn [{[cur-pt prev-pt & _] :trail :as cycle} new-dir]
  (let [new-pt (add-points new-dir cur-pt)]
    (if (= new-pt prev-pt) cycle
	(assoc cycle :dir new-dir))))

;; mutable state ahead
(defn reset-game [p1 p2]
  (dosync
   (ref-set p1 (create-p1))
   (ref-set p2 (create-p2))))

(defn update-direction [cycle new-dir]
  (dosync (alter cycle turn new-dir)))

(defn update-positions [cycle-refs]
  (dosync
   (doseq [cycle-ref cycle-refs]
     (if @cycle-ref (alter cycle-ref move)))))

(defn dead-ref? [cycle-ref other-cycle-refs]
  (dead? @cycle-ref (map deref other-cycle-refs)))

(defn remove-dead [cycle-refs]
  (dosync
   (let [get-other-refs #(disj (set cycle-refs) %)
	 pair-fn #(list % (dead-ref? % (get-other-refs %)))
	 pairs (map pair-fn cycle-refs)]
     (doseq [[cycle-ref dead] pairs]
       (if dead (ref-set cycle-ref nil))))))
