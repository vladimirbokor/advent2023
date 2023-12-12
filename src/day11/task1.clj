(ns day11.task1
  (:require [clojure.set :as set]))

(defn indexes-of-galaxies [s]
  (keep-indexed #(if (= %2 \#) %1) s))

(defn make-galaxy-pairs [idx, line]
  (let [galaxy-indexes (indexes-of-galaxies line)]
    (for [element galaxy-indexes] [idx element])))

(defn get-galaxies-maps [lines]
  (->>
   (map-indexed make-galaxy-pairs lines)
   (apply concat)))


(defn unique-pairs [coords]
  (let [n (count coords)]
    (for [i (range 0 n)
          j (range (inc i) n)]
      [(nth coords i) (nth coords j)])))

(defn get-difference [end, non-empty]
  (set/difference (set (range 0 end)) (set non-empty)))

(defn count-expands [start, end, empty-set]
  (let [range-seq (if (<= start end) (range start end) (range end start))]
    (count (set/intersection empty-set (set range-seq)))))

(defn count-distance [pair, empty-rows, empty-columns]
  (let [[[r1, c1], [r2, c2]] pair
        count-expanded-rows (count-expands r1 r2 empty-rows)
        count-expanded-columns (count-expands c1 c2 empty-columns)] 
    (reduce + [(abs (- r2 r1)) (abs (- c2 c1)) (* 999999 count-expanded-rows) (* 999999 count-expanded-columns)])))

(with-open [rdr (clojure.java.io/reader "assets/day11/real_data.txt")]
  (let [lines (vec (line-seq rdr))
        galaxies-cord (get-galaxies-maps lines)
        pairs (unique-pairs galaxies-cord)
        empty-rows (get-difference (count lines) (map first galaxies-cord))
        empty-columns (get-difference (count (first lines)) (map second galaxies-cord))]
    (->>
     (map #(count-distance % empty-rows empty-columns) pairs)
     (reduce +))))