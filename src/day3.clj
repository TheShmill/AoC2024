(ns day3
  (:require [clojure.string :as str]))

(def input (slurp "input/day3.txt"))

(defn parse-mul [[_ a b]]
  (* (parse-long a)
     (parse-long b)))

(defn part1 [input]
  (->> input
       (re-seq #"mul\((\d\d?\d?),(\d\d?\d?)\)")
       (map parse-mul)
       (reduce +)))

(defn part2 [input]
  (->> input
       (re-seq #"(?:do\(\)|^)(.|\n)*?(?:don't\(\)|$)")
       (map first)
       (map part1)
       (reduce +)))
