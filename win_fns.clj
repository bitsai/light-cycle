(ns win-fns)

(defn ffa-win-fn [cycles]
  (let [live-cycles (filter #(not (nil? %)) cycles)]
    (case (count live-cycles)
	  1 (str (:name (first live-cycles)) " wins!")
	  0 "Tie!"
	  nil)))
