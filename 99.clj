;; Product Digits
;; Difficulty:	Easy
;; Topics:	math seqs

;; Write a function which multiplies two numbers and returns the result as a sequence of its digits.


(def __
  (fn [a b]
    (ffirst
     (drop-while
      (comp pos? second)
      (iterate (fn [[digits x]] [(cons (rem x 10) digits) (quot x 10)]) [[] (* a b)])))))

(rem 891 10)
(quot 891 1000)

(= (__ 1 1) [1])
(= (__ 99 9) [8 9 1])
(= (__ 999 99) [9 8 9 0 1])
