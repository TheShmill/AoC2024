(ns day4
  (:require [clojure.string :as str]))

(def input (slurp "input/day4.txt"))

(defn cols [input]
  (->> input
       (apply (partial map vector))
       (map #(apply str %))))

(defn rows [input]
  input)

(defn diags [input]
  (->> input
       vec
       (mapv vec)
       (#(for [x (range (count (% 0)))
               y (range (count %))]
           [(- x y) ((% y) x)]))
       (reduce (fn [m [k v]] (update m k #(str % v))) {})
       vals))

(defn backwards [input]
  (->> input
       (map reverse)
       (map #(apply str %))))

(defn all-ways [input]
  (concat (cols input)
          (backwards (cols input))
          (rows input)
          (backwards (rows input))
          (diags input)
          (backwards (diags input))
          (diags (backwards input))
          (backwards (diags (backwards input)))))

(defn part1 [input]
  (->> input
       str/split-lines
       all-ways
       (map #(count (re-seq #"XMAS" %)))
       (reduce +)))

(defn windows [input]
  (for [y (range (- (count input) 2))
        x (range (- (count (input 0)) 2))]
    (->> input
         (#(subvec % y (+ y 3)))
         (map #(subs % x (+ x 3)))
         (str/join "\n"))))

(defn is-xmas [window]
  (let [[_ c1 c2 c3 c4] (re-find #"(M|S).(M|S)\n.A.\n(M|S).(M|S)" window)]
    (and (not= c1 c4)
         (not= c2 c3))))

(defn part2 [input]
  (->> input
       str/split-lines
       windows
       (filter is-xmas)
       count))
