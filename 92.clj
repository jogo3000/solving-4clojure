;; Read Roman numerals

;; Difficulty:	Hard
;; Topics:	strings math


;; Roman numerals are easy to recognize, but not everyone knows all the rules necessary to work with them. Write a function to parse a Roman-numeral string and return the number it represents.

;; You can assume that the input will be well-formed, in upper-case, and follow the subtractive principle. You don't need to handle any numbers greater than MMMCMXCIX (3999), the largest number representable with ordinary letters.

(def __
  (fn [s]
    (let [values {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (->> (map values s)
           (reduce (fn [{:keys [total previous] :as result} v]
                     (let [maybe-corrected (if (and previous (< previous v)) (- v previous previous) v)]
                       (assoc result :total (+ total maybe-corrected) :previous v))) {:total 0 :previous nil})
           (:total)))))

(= 14 (__ "XIV"))
(= 827 (__ "DCCCXXVII"))
(= 3999 (__ "MMMCMXCIX"))
(= 48 (__ "XLVIII"))
