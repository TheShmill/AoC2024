(ns day6
  (:require [clojure.string :as str]
            [clojure.core.reducers :as r]))

(def input (str/trim (slurp "input/day6.txt")))

(defn parse-input [input]
  (loop [x 0 y 0
         [c & rest] input
         guard nil
         squares #{}]
    (case c
      nil {:squares squares
           :guard guard
           :max-x (dec x)
           :max-y y}
      \newline (recur 0 (inc y) rest guard squares)
      \. (recur (inc x) y rest guard squares)
      \# (recur (inc x) y rest guard (conj squares [x y]))
      \^ (recur (inc x) y rest [x y :up] squares))))

(def rot {:up :right, :right :down, :down :left, :left :up})
(def rot->move {:up [0 -1], :down [0 1], :right [1 0], :left [-1 0]})

(defn next-pos [[x y dir]]
  (let [[dx dy] (rot->move dir)]
    [(+ dx x) (+ dy y)]))

(defn tick-loop [{:keys [squares guard max-x max-y]}]
  (let [[x y dir] guard]
    (loop [[x y dir :as pos] guard
           visited #{}]
      (let [[new-x new-y] (next-pos pos)]
        (cond (squares [new-x new-y]) (recur [x y (rot dir)] visited)
                (neg? new-x) (conj visited [x y])
                (neg? new-y) (conj visited [x y])
                (> new-x max-x) (conj visited [x y])
                (> new-y max-y) (conj visited [x y])
                :else (recur [new-x new-y dir] (conj visited [x y])))))))

(defn part1 [input]
  (->> input
       parse-input
       tick-loop
       count))

(defn cycle? [{:keys [squares guard max-x max-y]}]
  (let [[x y dir] guard]
    (loop [[x y dir :as pos] guard
           corners (transient #{})]
      (let [[new-x new-y] (next-pos pos)]
        (cond (corners pos) 1
              (squares [new-x new-y]) (recur [x y (rot dir)] (conj! corners pos))
              (neg? new-x) 0
              (neg? new-y) 0
              (> new-x max-x) 0
              (> new-y max-y) 0
              :else (recur [new-x new-y dir] corners))))))

(defn part2 [input]
  (let [parsed (parse-input input)]
    (->> parsed
         tick-loop
         (into [])
         (r/map #(update-in parsed [:squares] conj %))
         (r/map cycle?)
         (r/fold 8 + +))))
