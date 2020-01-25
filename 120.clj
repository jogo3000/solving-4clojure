;; Sum of square of digits

;; Difficulty:	Easy
;; Topics:	math

;; Write a function which takes a collection of integers as an argument. Return the count of how many elements are smaller than the sum of their squared component digits. For example: 10 is larger than 1 squared plus 0 squared; whereas 15 is smaller than 1 squared plus 5 squared.

(defn digits [n]
  (when (pos? n)
    (cons (rem n 10) (digits (quot n 10)))))

(def __
  (fn [coll]
    (letfn [(digits [n]
              (if (zero? n) '(0)
                  (when (pos? n)
                    (cons (rem n 10) (digits (quot n 10))))))]
      (->> (filter (fn [n]
                     (< n (->> (digits n)
                               (map #(* % %))
                               (reduce +)))) coll)
           count))))

(= 8 (__ (range 10)))

(= 19 (__ (range 30)))

(= 50 (__ (range 100)))

(= 50 (__ (range 1000)))
