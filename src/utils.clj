(ns utils)

(defn ^:export remove-nth [n coll]
  (concat (take n coll)
          (drop (inc n) coll)))

(defn ^:export without-one [pred coll]
  (->> (range (count coll))
       (map #(remove-nth % coll))
       (some pred)))

(defn ^:export input->grid [input]
  (loop [x 0, y 0, [c & rest] input, heights {}]
    (cond
      (nil? c) heights
      (= c \newline) (recur 0 (inc y) rest heights)
      :else (recur (inc x) y rest (assoc heights [x y] c)))))
