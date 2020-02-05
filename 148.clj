;; The Big Divide

;; Difficulty:	Medium
;; Topics:	math


;; Write a function which calculates the sum of all natural numbers under n (first argument) which are evenly divisible by at least one of a and b (second and third argument). Numbers a and b are guaranteed to be coprimes.

;; Note: Some test cases have a very large n, so the most obvious solution will exceed the time limit.

(comment "I had to try the naive solution. It obviously was too slow.")

(def __naive
  (fn [n a b]
    (reduce (fn [v x] (if (or (zero? (rem x a))
                              (zero? (rem x b)))
                        (+ v x)
                        v)) 0 (range n))))

(comment "Then I tried another approach with some laziness. It was even slower.")

(def __definitely_too_slow
  (fn [n a b]
    (reduce + (mapcat (fn [x] (take-while #(< % n) (iterate #(+ x %) x))) [a b]))))


(comment "Final approach. I calculate 1+2+3+4+...+n where n is quotient of n and a. Then I count the same for quotient of n and b.
I sum them. For multiples of a*b the values are twice counted for, so finally I remove 1+2+3+4+...+n times a*b")

(def __
  (fn [n a b]
    (letfn [(n+ [n]
              (/ (*' n (+ n 1)) 2))]
      (- (+ (*' a (n+ (quot (dec n) a)))
            (*' b (n+ (quot (dec n) b))))
         (*' (*' a b) (n+ (quot (dec n) (*' a b))))))))


(= 0 (__ 3 17 11))

(= 23 (__ 10 3 5))

(= 233168 (__ 1000 3 5))

(= "2333333316666668" (str (__ 100000000 3 5)))

(= "110389610389889610389610"
  (str (__ (* 10000 10000 10000) 7 11)))

(= "1277732511922987429116"
  (str (__ (* 10000 10000 10000) 757 809)))

(= "4530161696788274281"
  (str (__ (* 10000 10000 1000) 1597 3571)))
