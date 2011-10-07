(ns win-fns)

(defn ffa-win-fn [cycles]
  (case (count cycles)
	1 (str (:name (first cycles)) " wins!")
	0 "Tie!"
	nil))
