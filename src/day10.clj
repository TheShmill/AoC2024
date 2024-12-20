(ns day10
  (:require [clojure.string :as str]
            [utils :as u]))

(def input (str/trim (slurp "input/day10.txt")))

(defn parse-input [input]
  (into {}
        (for [[pos v] (u/input->grid input)]
          [pos (parse-long (str v))])))

(defn get-trailheads [grid]
  (->> grid
       (filter #(= 0 (second %)))
       (map first)))

(defn neighbors [[x y]]
  (for [[dx dy] [[0 1], [0 -1], [1 0], [-1 0]]]
    [(+ dx x) (+ dy y)]))

(defn count-trails [trailhead grid type]
  (loop [nodes (type trailhead)
         n 1]
    (if (= n 10)
      (count nodes)
      (recur (->> nodes
                  (mapcat neighbors)
                  (filter #(= (grid %) n))
                  (into (type)))
             (inc n)))))

(defn part1 [input]
  (let [grid (parse-input input)
        trailheads (get-trailheads grid)]
    (->> trailheads
         (map #(count-trails % grid hash-set))
         (reduce +))))

(defn part2 [input]
  (let [grid (parse-input input)
        trailheads (get-trailheads grid)]
    (->> trailheads
         (map #(count-trails % grid vector))
         (reduce +))))
