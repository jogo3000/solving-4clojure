;; Happy numbers

;; Difficulty:	Medium
;; Topics:	math


;; Happy numbers are positive integers that follow a particular formula: take each individual digit, square it, and then sum the squares to get a new number. Repeat with the new number and eventually, you might get to a number whose squared sum is 1. This is a happy number. An unhappy number (or sad number) is one that loops endlessly. Write a function that determines if a number is happy or not.

(def __
  "Unhappy numbers end up in a cycle which does not contain 1"
  (fn [n]
    ((fn step [found n]
       (let [digits (ffirst
                     (drop-while
                      (comp pos? second)
                      (iterate (fn [[digits x]] [(cons (rem x 10) digits) (quot x 10)]) [[] n])))
             next (->> (map #(* % %) digits)
                       (reduce +))]
         (if-not (found next)
           (recur (conj found next) next)
           (= 1 next)))) #{}  n)))


(= (__ 7) true)



(= (__ 986543210) true)



(= (__ 2) false)



(= (__ 3) false)
