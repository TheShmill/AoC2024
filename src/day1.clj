(ns day1
  (:require [clojure.string :as str]))

(def input (->> (slurp "input/day1.txt")
                str/split-lines
                (map #(str/split % #" +"))))
(def l1 (->> input
             (map first)
             (map parse-long)
             sort))
(def l2 (->> input
             (map second)
             (map parse-long)
             sort))
(def part1
  (->> (map #(abs (- %1 %2)) l1 l2)
       (reduce +)))

(def part2
  (->> (for [x l1, y l2, :when (= x y)] x)
       (reduce +)))
