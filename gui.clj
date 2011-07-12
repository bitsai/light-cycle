(ns gui
  (:use [engine])
  (:import (java.awt Dimension)
	   (java.awt.event ActionListener KeyEvent KeyListener)
	   (javax.swing JFrame JOptionPane JPanel Timer)))

(def p1-dirs {KeyEvent/VK_W [ 0 -1]
	      KeyEvent/VK_A [-1  0]
	      KeyEvent/VK_S [ 0  1]
	      KeyEvent/VK_D [ 1  0]})

(def p2-dirs {KeyEvent/VK_UP    [ 0 -1]
	      KeyEvent/VK_LEFT  [-1  0]
	      KeyEvent/VK_DOWN  [ 0  1]
	      KeyEvent/VK_RIGHT [ 1  0]})

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
    (if (hit-trail? cycle cycles) (fill-point g pt white))))

(defn game-panel [frame win-fn cycles-ref]
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]
		    (proxy-super paintComponent g)
		    (paint-cycles g @cycles-ref)
		    (paint-hits g @cycles-ref))
    (actionPerformed [e]
		     (clear-dead cycles-ref)
		     (update-positions cycles-ref)
		     (let [win-msg (win-fn @cycles-ref)]
		       (when win-msg
			 (reset-game cycles-ref)
			 (JOptionPane/showMessageDialog frame win-msg)))
		     (.repaint this))
    (keyPressed [e]
		(let [p1-dir (p1-dirs (.getKeyCode e))
		      p2-dir (p2-dirs (.getKeyCode e))]
		  (cond
		   p1-dir (update-direction cycles-ref "Player 1" p1-dir)
		   p2-dir (update-direction cycles-ref "Player 2" p2-dir))))
    (getPreferredSize []
		      (Dimension. (* (inc width) point-size)
				  (* (inc height) point-size)))
    (keyReleased [e])
    (keyTyped [e])))

(defn game [win-fn]
  (let [cycles-ref (ref (create-cycles))
	frame (JFrame. "Light Cycle")
	panel (game-panel frame win-fn cycles-ref)
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
    [cycles-ref timer]))
