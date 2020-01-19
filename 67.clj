;; Prime Numbers
;; Difficulty:	Medium
;; Topics:	primes

;; Write a function which returns the first x number of prime numbers.

;; (= (__ 2) [2 3])
;; (= (__ 5) [2 3 5 7 11])
;; (= (last (__ 100)) 541)

(def primes
  (fn [n]
    (take n (filter #(every?
                      pos?
                      (map (fn [m] (mod % m))
                           (range 2 %)))
                    (range 2 1000)))))
