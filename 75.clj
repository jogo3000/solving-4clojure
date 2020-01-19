;; Euler's Totient Function

;; Difficulty:	Medium
;; Topics:

;; Two numbers are coprime if their greatest common divisor equals 1. Euler's totient function f(x) is defined as the number of positive integers less than x which are coprime to x. The special case f(1) equals 1. Write a function which calculates Euler's totient function.

;; (= (__ 1) 1)
;; (= (__ 10) (count '(1 3 7 9)) 4)
;; (= (__ 40) 16)
;; (= (__ 99) 60)

(def totient
  (fn [n]
    (if (= n 1) 1
        (->> (range 1 n)
             (filter (fn [x]
                       (= 1
                          ((fn [a b]
                             (if (zero? b) a
                                 (recur b (mod a b))))
                           x n))))
             count))))

(totient 1)
(totient 10)
