;; Re-implement Map
;; Difficulty:	Easy
;; Topics:	core-seqs
;;
;; Map is one of the core elements of a functional programming language. Given a function f and an input sequence s, return a lazy sequence of (f x) for each element x in s.

(def __
  (fn my-map [f coll]
    (when (seq coll)
      (cons (f (first coll)) (lazy-seq (my-map f (rest coll)))))))


(= [3 4 5 6 7]
   (__ inc [2 3 4 5 6]))

(= (repeat 10 nil)
   (__ (fn [_] nil) (range 10)))

(= [1000000 1000001]
   (->> (__ inc (range))
        (drop (dec 1000000))
        (take 2)))

;; Special Restrictions
;; map
;; map-indexed
;; mapcat
;; for
