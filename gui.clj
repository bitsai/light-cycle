(ns gui
  (:use [engine])
  (:import (java.awt Dimension)
	   (java.awt.event ActionListener KeyListener)
	   (javax.swing JFrame JOptionPane JPanel Timer))
  (:use clojure.contrib.import-static))

(import-static java.awt.event.KeyEvent
	       VK_W VK_A VK_S VK_D
	       VK_UP VK_LEFT VK_DOWN VK_RIGHT)

(def p1-dirs {VK_W [ 0 -1]
	      VK_A [-1  0]
	      VK_S [ 0  1]
	      VK_D [ 1  0]})
(def p2-dirs {VK_UP    [ 0 -1]
	      VK_LEFT  [-1  0]
	      VK_DOWN  [ 0  1]
	      VK_RIGHT [ 1  0]})

(defn point-to-screen-rect [[x y]]
  (map #(* point-size %) [x y 1 1]))

(defn fill-point [g pt color]
  (let [[x y width height] (point-to-screen-rect pt)]
    (.setColor g color)
    (.fillRect g x y width height)))

(defn paint-cycles [g cycles]
  (doseq [{:keys [trail color]} cycles]
    (doseq [point trail]
      (fill-point g point color))))

(defn paint-hits [g cycles]
  (doseq [{[pt & _] :trail :as cycle} cycles]
    (let [other-cycles (disj (set cycles) cycle)]
      (if (hit-cycle? cycle other-cycles) (fill-point g pt white)))))

(defn game-panel [frame win-fn p1 p2]
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]
		    (proxy-super paintComponent g)
		    (paint-cycles g [@p1 @p2])
		    (paint-hits g [@p1 @p2]))
    (actionPerformed [e]
		     (remove-dead [p1 p2])
		     (update-positions [p1 p2])
		     (let [win-msg (win-fn [@p1 @p2])]
		       (when win-msg
			 (reset-game p1 p2)
			 (JOptionPane/showMessageDialog frame win-msg)))
		     (.repaint this))
    (keyPressed [e]
		(let [p1-dir (p1-dirs (.getKeyCode e))
		      p2-dir (p2-dirs (.getKeyCode e))]
		  (cond
		   (and @p1 p1-dir) (update-direction p1 p1-dir)
		   (and @p2 p2-dir) (update-direction p2 p2-dir))))
    (getPreferredSize []
		      (Dimension. (* (inc width) point-size)
				  (* (inc height) point-size)))
    (keyReleased [e])
    (keyTyped [e])))

(defn game [win-fn]
  (let [p1 (ref (create-p1))
	p2 (ref (create-p2))
	frame (JFrame. "Light Cycle")
	panel (game-panel frame win-fn p1 p2)
	timer (Timer. turn-millis panel)]
    (doto panel
      (.addKeyListener panel)
      (.setBackground black)
      (.setFocusable true))
    (doto frame
      (.add panel)
      (.pack)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setVisible true))
    (.start timer)
    [p1 p2 timer]))
