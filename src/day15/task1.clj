(ns day15.task1
  (:require [clojure.string :as str]))

(defn calc-new-hash [acc symbol]
  (->>
   (int symbol)
   (+ acc)
   (* 17)
   (#(rem % 256))))


(defn make-steps [rdr]
  (->>
   (line-seq rdr)
   (first)
   (#(str/split % #","))))

(defn calc-hash [step]
  (reduce calc-new-hash 0 step))

(with-open [rdr (clojure.java.io/reader "assets/day15/real_data.txt")]
  (->>
   (make-steps rdr)
   (map calc-hash)
   (reduce +)))