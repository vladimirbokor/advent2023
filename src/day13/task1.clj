(ns day13.task1)


(defn find-reflection-indexes [matrix]
  (->>
   (map vector (range) matrix (rest matrix))
   (filter (fn [[_ el1 el2]] (= el1 el2)))
   (map first)))


(defn equal? [pair, matrix]
  (let [[x, y] pair]
    (= (get matrix x) (get matrix y))))

(defn get-count-above [matrix, reflection-index]
  (if (->>
       (map vector (range (dec reflection-index) -1 -1) (range (+ reflection-index 2) (count matrix)))
       (every? #(equal? % (vec matrix))))
    (inc reflection-index)
    0))


(defn get-counts [matrix]
  (->>
   (find-reflection-indexes matrix)
   (map #(get-count-above matrix %))
   (reduce +)))

(defn transpose [matrix]
  (apply map vector matrix))

(defn make-matrixes [lines]
  (->>
   (partition-by empty? lines)
   (remove #(= 1 (count %)))))

(with-open [rdr (clojure.java.io/reader "assets/day13/real_data.txt")]
  (let [lines (line-seq rdr)
        matrixes (make-matrixes lines)
        horizontal-reflections (map get-counts matrixes)
        vertical-reflections (map get-counts (map transpose matrixes))]

    (+
     (reduce + (map #(* 100 %) horizontal-reflections))
     (reduce + vertical-reflections))))



