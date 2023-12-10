(ns day03.task1
  (:require
   [clojure.spec.alpha :as alpha]))

(defn req-symbol? [sym]
  (and (not= sym \.) (not (Character/isDigit sym))))

(defn find-symbol-positions [s]
  (keep-indexed #(if (req-symbol? %2) %1) s))


(defn get-symbol-maps [lines]
  (->>
   (map-indexed (fn [idx, line] {idx, (find-symbol-positions line)}) lines)
   (apply merge)))

(defn extract-number-pairs [s]
  (let [matcher (re-matcher #"\d+" s)]
    (loop [matches []]
      (if (.find matcher)
        (let [start (.start matcher)
              match (.group matcher)
              end (+ start (count match))]
          (recur (conj matches (vector (- start 1) end (parse-long match)))))
        matches))))


(defn connected-num? [idx, num-pair, symbol-maps]
  (let [[start, end, _] num-pair
        arround-indexes [(- idx 1) idx (+ idx 1)]]
    (->>
     (mapcat (fn [index] (map #(alpha/int-in-range? start (+ end 1) %) (symbol-maps index))) arround-indexes)
     (some true?))) 
  )

(defn get-suitable-num [idx, line, symbol-maps]
  (let [number-pairs, (extract-number-pairs line)]
    (if (empty? number-pairs)
      0
      (->>
       (filter #(connected-num? idx % symbol-maps) number-pairs)
       (map last) 
       (reduce +)))))

(with-open [rdr (clojure.java.io/reader "assets/day03/real_data.txt")]
  (let [lines (line-seq rdr)
        symbol-maps (get-symbol-maps lines)]

    (->>
     (map-indexed (fn [idx, value] (get-suitable-num idx value symbol-maps)) lines)
     (reduce +))
     )
  )
    
    