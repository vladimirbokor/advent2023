(ns day09.task2
  (:require [clojure.string :as str]))

(defn calc-differences [seq]
  (map - (rest seq) seq))

(defn calc-first-values [seq, new-seq]
  (let [
        current-diff-seq (calc-differences seq)
        current-new-seq (conj new-seq (first current-diff-seq))
        ]
    (if (every? #(= 0 %) seq)
      new-seq
      (calc-first-values current-diff-seq current-new-seq))
    ) 
  )

(defn calculate-value [seq]
  (let [reversed (reverse seq)]
    (reduce (fn [acc x] (- x acc)) (first reversed) (rest reversed))))
  

(defn parse-seq [initial-seq]
  (->>
   (str/split initial-seq #" ")
   (map parse-long)))

(defn get-first-element [initial-seq]
  (let [parsed-seq (parse-seq initial-seq)]
    (->>
     (calc-first-values parsed-seq [(first parsed-seq)])
     (calculate-value))))

(with-open [rdr (clojure.java.io/reader "assets/day09/real_data.txt")]
  (->>
   (line-seq rdr)
   (map get-first-element)
   (reduce +)))