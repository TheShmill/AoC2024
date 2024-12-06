(ns day5
  (:require [clojure.string :as str]))

(def input (slurp "input/day5.txt"))

(defn parse-rule [rule]
  (mapv parse-long (str/split rule #"\|")))

(defn parse-update [update]
  (mapv parse-long (str/split update #"\,")))

(defn valid-update [rules [val & rest]]
  (println rules val rest)
  (cond
    (nil? val) true
    (empty? rules) true
    (some #(= (% 1) val) rules) false
    :else (recur (remove #(= (% 0) val) rules) rest)))

(defn part1 [input]
  (let [[rules updates] (str/split input #"\n\n")
        rules (mapv parse-rule (str/split-lines rules))
        updates (mapv parse-update (str/split-lines updates))]
    (valid-update rules (updates 0))))
