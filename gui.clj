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

(defn fill-point [g pt color]
  (let [[x y width height] (point-to-screen-rect pt)]
    (.setColor g color)
    (.fillRect g x y width height)))

(defn paint-cycle [g {:keys [trail color]}]
  (doseq [point trail]
    (fill-point g point color)))

(defn paint-collisions [g p1 p2]
  (let [p1-pt (first (:trail p1))
	p2-pt (first (:trail p2))]
    (if (hit-trails? p1 p2) (fill-point g p1-pt orange))
    (if (hit-trails? p2 p1) (fill-point g p2-pt orange))))

(defn game-panel [frame p1 p2]
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]
		    (proxy-super paintComponent g)
		    (paint-cycle g @p1)
		    (paint-cycle g @p2)
		    (paint-collisions g @p1 @p2))
    (actionPerformed [e]
		     (update-positions p1 p2)
		     (let [winner (get-winner @p1 @p2)]
		       (when winner
			 (reset-game p1 p2)
			 (JOptionPane/showMessageDialog frame winner)))
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

(defn game []
  (let [p1 (ref (create-p1))
	p2 (ref (create-p2))
	frame (JFrame. "Light Cycle")
	panel (game-panel frame p1 p2)
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
