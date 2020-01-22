;; Perfect Numbers

;; Difficulty:	Medium
;; Topics:


;; A number is "perfect" if the sum of its divisors equal the number itself. 6 is a perfect number because 1+2+3=6. Write a function which returns true for perfect numbers and false otherwise.

(def __
  (fn [n]
    (->>
     (filter #(zero? (rem n %)) (range 1 n))
     (reduce +)
     (= n))))

(= (__ 6) true)
(= (__ 7) false)
(= (__ 496) true)
(= (__ 500) false)
(= (__ 8128) true)
