(ns day09.task1
  (:require [clojure.string :as str]))

(defn calc-differences [seq]
  (map - (rest seq) seq))

(defn calc-last-values [seq, new-seq]
  (let [
        current-diff-seq (calc-differences seq)
        current-new-seq (conj new-seq (last current-diff-seq))
        ]
    (if (every? #(= 0 %) seq)
      new-seq
      (calc-last-values current-diff-seq current-new-seq))
    ) 
  )

(defn parse-seq [initial-seq]
  (->>
   (str/split initial-seq #" ")
   (map parse-long)))

(defn get-last-element [initial-seq]
  (let [parsed-seq (parse-seq initial-seq)]
    (->>
     (calc-last-values parsed-seq [(last parsed-seq)])
     (reduce +))))

(with-open [rdr (clojure.java.io/reader "assets/day09/real_data.txt")]
  (->>
   (line-seq rdr)
   (map get-last-element)
   (reduce +)))