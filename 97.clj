;; Pascal's Triangle

;; Difficulty:	Easy
;; Topics:


;; Pascal's triangle is a triangle of numbers computed using the following rules:

;; - The first row is 1.
;; - Each successive row is computed by adding together adjacent numbers in the row above, and adding a 1 to the beginning and end of the row.

;; Write a function which returns the nth row of Pascal's Triangle.

(defn spy [val]
  (println "spy" val) val)

(def __
  (fn [n]
    (letfn [(pad [coll]
              (cond (empty? coll) [1]
                    (= coll [1]) [1 1]
                    :else (-> coll (conj 1) vec (conj 1))))
            (step [coll]
              (let [nextval (if (= coll [1]) [1 1]
                              (->> (partition 2 1 coll)
                                   (map (partial apply +))
                                   pad))]
                (cons nextval (lazy-seq (step nextval)))))]
      (nth (step '()) (dec n)))))

(__ 1)
(__ 2)
(__ 3)

(= (__ 1) [1])
(= (map __ (range 1 6))
   [     [1]
        [1 1]
       [1 2 1]
      [1 3 3 1]
    [1 4 6 4 1]])

(= (__ 11)
   [1 10 45 120 210 252 210 120 45 10 1])
