(ns utils)

(defn ^:export remove-nth [n coll]
  (concat (take n coll)
          (drop (inc n) coll)))

(defn ^:export without-one [pred coll]
  (->> (range (count coll))
       (map #(remove-nth % coll))
       (some pred)))

(defn ^:export input->grid [input]
  (loop [x 0, y 0, [c & rest] input, nodes {}]
    (cond
      (nil? c) nodes
      (= c \newline) (recur 0 (inc y) rest nodes)
      :else (recur (inc x) y rest (assoc nodes [x y] c)))))


(defn ^:export neighbors [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (or (not= dx 0)
                  (not= dy 0))]
    [(+ x dx) (+ y dy)]))
