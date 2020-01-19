;; Greatest Common Divisor
;; Difficulty:	Easy
;; Topics:
;;
;; Given two integers, write a function which returns the greatest common divisor.
;;
;; (= (__ 2 4) 2)
;; (= (__ 10 5) 5)
;; (= (__ 5 7) 1)
;; (= (__ 1023 858) 33)

(def gcd
  "Euclid's algorithm, simplified

  https://en.wikipedia.org/wiki/Greatest_common_divisor"
  (fn [a b]
    (if (zero? b) a
        (recur b (mod a b)))))

(assert (= (gcd 2 4) 2))
