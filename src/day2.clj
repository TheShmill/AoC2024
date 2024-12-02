(ns day2
  (:require [clojure.string :as str]
            [utils :as u]))

(def input (->> (slurp "input/day2.txt")
                str/split-lines
                (map (fn [line]
                       (->> line
                            (#(str/split % #" "))
                            (map parse-long))))))

(defn valid? [line]
  (let [diffs (map #(apply - %)
                   (partition 2 1 line))]
    (and (every? #(<= 1 (abs %) 3) diffs)
         (or (every? pos? diffs)
             (every? neg? diffs)))))

(defn part1 []
  (count (filter valid? input)))

(defn part2 []
  (count (filter #(u/without-one valid? %)
                 input)))
