;; The Balance of N

;; Difficulty:	Medium
;; Topics:	math


;; A balanced number is one whose component digits have the same sum on the left and right halves of the number. Write a function which accepts an integer n, and returns true iff n is balanced.

(def __
  (fn [n]
    (letfn [(digits [n]
              (when (pos? n)
                (cons (rem n 10) (digits (quot n 10)))))]
      (let [digits (digits n)
            length (count digits)
            halves (split-at (/ length 2) digits)
            first-half (-> (first halves) ((if (odd? length) butlast identity)))
            second-half (second halves)]
        (= (apply + first-half)
           (apply + second-half))))))

(split-at (/ 7 2) '(1 2 1 2 1 2 1))

(= true (__ 11))

(= true (__ 121))

(= false (__ 123))

(= true (__ 0))

(= false (__ 88099))

(= true (__ 89098))

(= true (__ 89089))

(= (take 20 (filter __ (range)))

   [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101])
