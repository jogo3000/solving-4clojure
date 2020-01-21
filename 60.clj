;; Sequence Reductions
;; Difficulty:	Medium
;; Topics:	seqs core-functions

;; Write a function which behaves like reduce, but returns each intermediate value of the reduction. Your function must accept either two or three arguments, and the return sequence must be lazy.

(def __
  (fn my-reduce
    ([f coll]
     (my-reduce f (first coll) (rest coll)))
    ([f v coll]
     ((fn step [v coll]
        (if-not (seq coll)
          (list v)
          (let [x (f v (first coll))]
            (cons v (lazy-seq (step x (rest coll))))))) v coll))))

(= (take 5 (__ + (range))) [0 1 3 6 10])
(= (__ conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
(= (last (__ * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)

;; Special Restrictions
;; reductions
