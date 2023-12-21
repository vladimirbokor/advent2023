(ns day19.task1
  (:require [clojure.string :as str]))

(defn split-workflow [s]
  (->>
   (re-find #"(.*?)\{(.*?)}" s)
   (rest)
   vec))

(defn make-worklow-map [workflows]
  (->>
   (map split-workflow workflows)
   (into {})))

(defn make-input [rdr]
  (->>
   (line-seq rdr)
   (partition-by empty?)
   (remove #(= 1 (count %)))))

(defn make-rating-map [rating]
  (->>
   (str/replace rating #"\{|\}" "")
   (#(str/split % #","))
   (map #(str/split % #"="))
   (map (fn [[name, value]] [name, (parse-long value)]))
   (into {})))

(defn prepare-rules [workflow]
  (->>
   (str/split workflow #",")
   (map #(str/split % #":"))))

(defn handle-compare [rule, rating-map]
  (let [[category, value] (str/split (first rule) #"<|>")
        parsed-value (parse-long value)]
    (if (str/includes? rule "<")
      (if (< (rating-map category) parsed-value) (second rule) false)
      (if (> (rating-map category) parsed-value) (second rule) false))))

(defn handle-rule [rule, rating-map]
  (let [has-compare (or (str/includes? rule "<") (str/includes? rule ">"))]
    (if has-compare (handle-compare rule rating-map) (first rule))))

(defn go-over-workflow [workflow, rating-map]
  (->>
   (prepare-rules workflow)
   (map #(handle-rule % rating-map))
   (some identity)))

(defn go-over-map [workflow-map, cur-workflow, rating-map]
  (if (or (= "A" cur-workflow) (= "R" cur-workflow)) cur-workflow
      (go-over-map workflow-map (go-over-workflow (workflow-map cur-workflow) rating-map) rating-map)))

(defn calc-rating [rating-maps, idx]
  (->>
   (get rating-maps idx)
   (vals)
   (reduce +)))

(with-open [rdr (clojure.java.io/reader "assets/day19/real_data.txt")]
  (let [[workflows, ratings] (make-input rdr)
        workflow-map (make-worklow-map workflows)
        rating-maps (mapv make-rating-map ratings)]
    (->>
     (map #(go-over-map workflow-map "in" %) rating-maps)
     (keep-indexed (fn [idx, val] (if (= "A" val) idx)))
     (map #(calc-rating rating-maps %))
     (reduce +))))
    