(ns day9
  (:require [clojure.string :as str]))

(def input (str/trim (slurp "input/day9.txt")))

(defn parse-blocks [input]
  (->> input
       (map (comp parse-long str))
       (map #(repeat %2 %1)
            (interpose nil (range)))
       flatten
       vec))

(defn compact-blocks [original-blocks]
  (loop [[block & blocks] original-blocks
         rblocks (remove nil? (reverse original-blocks))
         final []
         countdown (count (remove nil? original-blocks))]
    (cond (zero? countdown) final
          (nil? block) (recur blocks
                              (rest rblocks)
                              (conj final (first rblocks))
                              (dec countdown))
          :else (recur blocks rblocks (conj final block) (dec countdown)))))

(defn check-sum [blocks]
  (reduce + (map-indexed * blocks)))

(defn part1 [input]
  (->> input
       parse-blocks
       compact-blocks
       check-sum))
