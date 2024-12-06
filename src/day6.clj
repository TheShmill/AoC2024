(ns day6
  (:require [clojure.string :as str]))

(def input (slurp "input/day6.txt"))

(defn parse-input [input]
  (loop [x 0 y 0
         [c & rest] input
         guard nil
         squares #{}]
    (case c
      nil
      {:squares squares
       :guard guard
       :max-x (dec x)
       :max-y y
       :moved #{}}
      \newline (recur 0 (inc y) rest guard squares)
      \. (recur (inc x) y rest guard squares)
      \# (recur (inc x) y rest guard (conj squares [x y]))
      \^ (recur (inc x) y rest [x y :up] squares))))

(def rot {:up :right, :right :down, :down :left, :left :up})
(def rot->move {:up [0 -1], :down [0 1], :right [1 0], :left [-1 0]})

(defn next-pos [[x y dir]]
  (let [[dx dy] (rot->move dir)]
    [(+ dx x) (+ dy y)]))

(defn tick-loop [{:keys [squares guard max-x max-y moved]}]
  (let [[x y dir] guard]
    (loop [[x y dir :as pos] guard
           visited moved]
      (let [[new-x new-y] (next-pos pos)]
        (cond (squares [new-x new-y]) (recur [x y (rot dir)] visited)
              (neg? new-x) (conj visited [x y])
              (neg? new-y) (conj visited [x y])
              (> new-x max-x) (conj visited [x y])
              (> new-y max-y) (conj visited [x y])
              :else (recur [new-x new-y dir] (conj visited [x y])))))))

(defn part1 [input]
  (->> input
       str/trim
       parse-input
       tick-loop
       count))
