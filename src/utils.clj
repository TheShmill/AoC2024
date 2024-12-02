(ns utils)

(defn ^:export remove-nth [n coll]
  (concat (take n coll)
          (drop (inc n) coll)))

(defn ^:export without-one [pred coll]
  (->> (range (count coll))
       (map #(remove-nth % coll))
       (some pred)))
