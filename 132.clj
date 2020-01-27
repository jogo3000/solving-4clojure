;; Insert between two items

;; Difficulty:	Medium
;; Topics:	seqs core-functions


;; Write a function that takes a two-argument predicate, a value, and a collection; and returns a new collection where the value is inserted between every two items that satisfy the predicate.

(defn spy [id x]
  (println id x) x)

(def __
  (fn [pred v coll]
    (letfn [(step [[c & cs]]
              (if-not (seq cs) (list c)
                      (if (pred c (first cs))
                        (cons c (lazy-seq (cons v (step cs))))
                        (cons c (lazy-seq (step cs))))))]
      (if (empty? coll) coll (step coll)))))

(= '(1 :less 6 :less 7 4 3) (__ < :less [1 6 7 4 3]))

(= '(2) (__ > :more [2]))

(= [0 1 :x 2 :x 3 :x 4]  (__ #(and (pos? %) (< % %2)) :x (range 5)))

(empty? (__ > :more ()))

(= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
   (take 12 (->> [0 1]
                 (iterate (fn [[a b]] [b (+ a b)]))
                 (map first) ; fibonacci numbers
                 (__ (fn [a b] ; both even or both odd
                       (= (mod a 2) (mod b 2)))
                     :same))))
