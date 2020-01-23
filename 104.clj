;; Write Roman Numerals
;; Difficulty:	Medium
;; Topics:	strings math

;; This is the inverse of Problem 92, but much easier. Given an integer smaller than 4000, return the corresponding roman numeral in uppercase, adhering to the subtractive principle.

(comment "It says here this is easier than problem 92. I don't know. It took me longer to solve this.")

(def __
  (fn [n]
    (let [values {1000 ["" "M" "MM" "MMM" "MMMM"]
                  100  ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
                  10   ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
                  1    ["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]}]
      (letfn [(step [n N]
                (when (pos? n)
                  (str
                   (get-in values [N (quot n N)])
                   (step (rem n N) (/ N 10)))))]
        (step n 1000)))))

(= "I" (__ 1))

(= "XXX" (__ 30))

(= "IV" (__ 4))

(= "CXL" (__ 140))

(= "DCCCXXVII" (__ 827))

(= "MMMCMXCIX" (__ 3999))

(= "XLVIII" (__ 48))
