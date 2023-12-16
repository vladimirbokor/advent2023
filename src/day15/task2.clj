(ns day15.task2
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

(defn split-step [step]
  (->>
   step
   (re-matches #"^(.*?)([&=|-])(.*)$")
   (rest)))

(defn remove-step-from-result [label, current-pairs]
  (remove #(= label (first %)) current-pairs))


(defn replace-or-add-pair [current-pairs label new-value]
  (let [pair-exists? (some #(= label (first %)) current-pairs)]
    (if pair-exists?
      (map #(if (= label (first %)) [label new-value] %) current-pairs)
      (conj current-pairs [label new-value]))))


(defn add-to-hash [current-map, step]
  (let [[label, action, value] (split-step step)
        label-hash (calc-hash label)
        current-pairs (current-map label-hash)
        new-pairs (if (= action "-") (remove-step-from-result label current-pairs) (replace-or-add-pair current-pairs label value))]

    (assoc current-map label-hash new-pairs)))

(defn calc-box-focal-length [box, pairs]
  (->>
   (reverse pairs)
   (map-indexed (fn [idx [_, value]] (reduce * [(inc box) (inc idx) (parse-long value)])))
   (reduce +)))


(with-open [rdr (clojure.java.io/reader "assets/day15/real_data.txt")]
  (->>
   (make-steps rdr)
   (reduce add-to-hash {})
   (map (fn [[box value]] (calc-box-focal-length box value)))
   (reduce +)))