;; Least Common Multiple

;; Difficulty:	Easy
;; Topics:	math


;; Write a function which calculates the least common multiple. Your function should accept a variable number of positive integers or ratios.

(comment
  "All examples I could find either calculated lcm for two numbers or expected the numbers to be integers.

So I decided to find the smallest gcd of the every pair of integers in the collection and brute force by using multiples of that smallest gcd"
  )

(defn gcd [a b]
  (if (zero? b) a
      (gcd b (rem a b))))

(def __
  (fn [& xs]
    (letfn [(gcd [a b]
              (if (zero? b) a
                  (gcd b (rem a b))))]
      (let [scd (->> (mapcat #(map (partial conj [%]) xs) xs)
                     (map (partial apply gcd))
                     (apply min))]
        (first
         (drop-while #(some pos? (map (fn [x] (rem % x)) xs)) (map (partial * scd) (iterate inc 1))))))))

(== (__ 2 3) 6)

(== (__ 5 3 7) 105)

(== (__ 1/3 2/5) 2)

(== (__ 3/4 1/6) 3/2)

(== (__ 7 5/7 2 3/5) 210)
