(ns day11
  (:require [clojure.string :as str]
            [clojure.math :as m]))

(def input (str/trim (slurp "input/day11.txt")))

(defn parse-input [input]
  (->> (str/split input #"\s")
       (mapv parse-long)))

(defn count-digits [n]
  (inc (int (m/log10 n))))

(defn split-num [n]
  (let [digits (count-digits n)
        shift-count (/ digits 2)
        divisor (m/pow 10 shift-count)]
    [(int (/ n divisor))
     (int (mod n divisor))]))

(defn stone-count [n steps]
  (cond (= steps 0) 1
        (= n 0) (stone-count 1 (dec steps))
        (even? (count-digits n)) (reduce + (mapv #(stone-count % (dec steps)) (split-num n)))
        :else (stone-count (* n 2024) (dec steps))))
(def stone-count (memoize stone-count))

(defn solve [input steps]
  (->> (parse-input input)
       (map #(stone-count % steps))
       (reduce +)))
