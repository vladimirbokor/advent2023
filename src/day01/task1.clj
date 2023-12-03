(ns day01.task1)

(defn get_digit_value [x] 
  (let [first_char (first x)]
    (cond
      (Character/isDigit first_char) first_char
      :else (get_digit_value (rest x)))) 
  )

(defn get_value [input]
  (let [reversed_input (reverse input)]
    (str (get_digit_value (seq input)) (get_digit_value (seq reversed_input)))) 
    )

(with-open [rdr (clojure.java.io/reader "assets/day01/real_data.txt")]
(->>
 (line-seq rdr)
 (map get_value)
 (map parse-long)
 (reduce +)
 ))