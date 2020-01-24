;; Sequs Horribilis

;; Difficulty:	Medium
;; Topics:	seqs


;; Create a function which takes an integer and a nested collection of integers as arguments. Analyze the elements of the input collection and return a sequence which maintains the nested structure, and which includes all elements starting from the head whose sum is less than or equal to the input integer.


(comment "A few false starts on this one, as I didn't read the problem description carefully. I tried to make a immutable algorithm,
but it ended up being so complicated I just retorted to an atom. I had some problems with having the sum update in nested structures too")

(def __
  (fn [n coll]
    (let [sum (atom 0)]
      (letfn [(step [coll]
                (when (seq coll)
                  (let [[c & cs] coll]
                    (if (coll? c)
                      (cons (step c) (step cs))
                      (if (<= (+ @sum c) n)
                        (do
                          (swap! sum (fn [n] (+ n c)))
                          (cons c (step cs)))
                        '())))))]
        (step coll)))
    ))



(=  (__ 10 [1 2 [3 [4 5] 6] 7])
   '(1 2 (3 (4))))

(=  (__ 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])
   '(1 2 (3 (4 (5 (6 (7)))))))

(=  (__ 9 (range))
    '(0 1 2 3))

(=  (__ 1 [[[[[1]]]]])
   '(((((1))))))

(=  (__ 0 [1 2 [3 [4 5] 6] 7])
   '())

(=  (__ 0 [0 0 [0 [0]]])
   '(0 0 (0 (0))))

(=  (__ 1 [-10 [1 [2 3 [4 5 [6 7 [8]]]]]])
   '(-10 (1 (2 3 (4)))))
