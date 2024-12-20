(ns day8
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def input (slurp "input/day8.txt"))

(defn parse-input [input]
  (loop [x 0, y 0, [c & rest] input, antennas {}]
    (case c
      nil antennas
      \. (recur (inc x) y rest antennas)
      \newline (recur 0 (inc y) rest antennas)
      (recur (inc x) y rest 
             (if (antennas c)
               (update antennas c #(conj % [x y]))
               (conj antennas [c [[x y]]]))))))

(defn antinodes [[x1 y1] [x2 y2]]
  [[(- x1 (- x2 x1)) (- y1 (- y2 y1))]
   [(- x2 (- x1 x2)) (- y2 (- y1 y2))]])

(defn part1 [input]
  (let [height (count (str/split-lines input))
        width (str/index-of input \newline)]
    (as-> input $
      (parse-input $)
      (update-vals $ #(combo/combinations % 2))
      (apply concat (vals $))      ; flatten into a seq of antenna pairs
      (mapv (fn [[p1 p2]] (antinodes p1 p2)) $)
      (apply concat $) ; flatten again
      (into #{} $) ; into a set to remove duplicates
      (remove (fn [[x y]] (or (< x 0) (< y 0) (<= width x) (<= height y))) $) ; remove outside the grid
      (count $))))
