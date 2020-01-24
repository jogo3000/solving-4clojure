;; Prime Sandwich

;; Difficulty:	Medium
;; Topics:	math


;; A balanced prime is a prime number which is also the mean of the primes directly before and after it in the sequence of valid primes. Create a function which takes an integer n, and returns true iff it is a balanced prime.

(comment "This went pretty bad quickly. I needed to optimize calculating primes as the final test needs to calculate a LOT of primes. It doesn't support a chance to share results between tests either. So I implemented Erasthotenes' sieve once again. It doesn't look too great this time. Then I figured out I can find the distance between the prime and the last prime and find out what would be the number which would make n be the mean. Then I could check if that is a prime or not. Too bad I got bogged down for a while because I didn't realize there might be _other_ primes between n and this upper limit. ")

(def __
  (fn [n]
    (cond (> 2 n) false
          (= 2 n) false
          :else
          (let [sieve (set (cons 2 (range 3 (inc n) 2)))
                primes (reduce (fn [sieve x] (apply disj sieve
                                                    (take-while
                                                     (partial >= n)
                                                     (iterate (partial + x) (* 2 x))))) sieve sieve)]
            (if-not
                (primes n) false
                (let [last-prime (->> (sort primes)
                                      butlast
                                      last)
                      distance (- n last-prime)
                      upper-limit (+ n distance)
                      muh (set (filter (fn [maybe-prime] (not-any? (comp zero? (partial rem maybe-prime)) (range 2 maybe-prime))) (range (inc n) (inc upper-limit))))]
                  (and (muh upper-limit)
                       (not-any? (partial > upper-limit) muh))))))))


(__ 97)
((comp pos? (partial rem 5)) 5)

(set (cons 2 (range 3 15 2)))

(__ 1)
(__ 2)

(__ 37)

(= false (__ 4))
(= true (__ 563))

;; 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97
;; 5, 53, 157, 173, 211, 257, 263, 373, 563, 593, 607, 653, 733, 947, 977, 1103

(/ (+ 31 43) 2)

(take 15 (filter __ (range)))

(= 1103 (nth (filter __ (range)) 15))

(#{2 3} 0)
