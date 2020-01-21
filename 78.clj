;; Reimplement Trampoline
;; Difficulty:	Medium
;; Topics:	core-functions

;; Reimplement the function described in "Intro to Trampoline".

(def __
  (fn [f & args]
    (loop [f? (apply f args)]
      (if (fn? f?)
        (recur (f?))
        f?))))

(= (letfn [(triple [x] #(sub-two (* 3 x)))
          (sub-two [x] #(stop?(- x 2)))
          (stop? [x] (if (> x 50) x #(triple x)))]
    (__ triple 2))
  82)

(= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
          (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
    (map (partial __ my-even?) (range 6)))
  [true false true false true false])

;; Special Restrictions
;; trampoline
