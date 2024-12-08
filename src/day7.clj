(ns day7
  (:require [clojure.string :as str]
            [clojure.math :as m]))

(def input (slurp "input/day7.txt"))

(defn | [num1 num2]
  (+ (* (int (m/pow 10 (inc (int (m/log10 num2)))))
        num1)
     num2))

(defn parse-line [line]
  (let [[target nums] (str/split line #": ")]
    {:target (parse-long target),
     :nums (rseq (mapv parse-long (str/split nums #" ")))}))

(defn get-results [[val & rest] ops max]
  (if (empty? rest)
    [val]
    (let [rest (get-results rest ops max)]
      (for [v rest
            op ops
            :let [result (op v val)]
            :when (<= result max)]
        result))))

(defn handle-line [line ops]
  (let [{:keys [target nums]} (parse-line line)]
    (->> (get-results nums ops target)
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
       (pmap #(handle-line % [+ * |]))
       (remove nil?)
       (reduce +)))
