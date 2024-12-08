(ns day7
  (:require [clojure.string :as str]))

(def input (slurp "input/day7.txt"))

(defn | [num1 num2]
  (parse-long (str num1 num2)))

(defn parse-line [line]
  (let [[target nums] (str/split line #": ")]
    {:target (parse-long target),
     :nums (rseq (mapv parse-long (str/split nums #" ")))}))

(defn get-results [[val & rest] ops]
  (if (empty? rest)
    [val]
    (let [rest (get-results rest ops)]
      (for [v rest
            op ops]
        (op v val)))))

(defn handle-line [line ops]
  (let [{:keys [target nums]} (parse-line line)]
    (->> (get-results nums ops)
         (filter #(= target %))
         first)))

(defn part1 [input]
  (->> input
       str/split-lines
       (mapv #(handle-line % [+ *]))
       (remove nil?)
       (reduce +)))

(defn part2 [input]
  (->> input
       str/split-lines
       (mapv #(handle-line % [+ * |]))
       (remove nil?)
       (reduce +)))
