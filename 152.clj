;; Latin Square Slicing

;; Difficulty:	Hard
;; Topics:	data-analysis math

;; A Latin square of order n is an n x n array that contains n different elements, each occurring exactly once in each row, and exactly once in each column. For example, among the following arrays only the first one forms a Latin square:

;; A B C    A B C    A B C
;; B C A    B C A    B D A
;; C A B    C A C    C A B

;; Let V be a vector of such vectors1 that they may differ in length2. We will say that an arrangement of vectors of V in consecutive rows is an alignment (of vectors) of V if the following conditions are satisfied:
;;     All vectors of V are used.
;;     Each row contains just one vector.
;;     The order of V is preserved.
;;     All vectors of maximal length are horizontally aligned each other.
;;     If a vector is not of maximal length then all its elements are aligned with elements of some subvector of a vector of maximal length.
;; Let L denote a Latin square of order 2 or greater. We will say that L is included in V or that V includes L iff there exists an alignment of V such that contains a subsquare that is equal to L.
;; For example, if V equals [[1 2 3][2 3 1 2 1][3 1 2]] then there are nine alignments of V (brackets omitted):

;;         1              2              3

;;       1 2 3          1 2 3          1 2 3
;;   A   2 3 1 2 1    2 3 1 2 1    2 3 1 2 1
;;       3 1 2        3 1 2        3 1 2

;;       1 2 3          1 2 3          1 2 3
;;   B   2 3 1 2 1    2 3 1 2 1    2 3 1 2 1
;;         3 1 2        3 1 2        3 1 2

;;       1 2 3          1 2 3          1 2 3
;;   C   2 3 1 2 1    2 3 1 2 1    2 3 1 2 1
;;           3 1 2        3 1 2        3 1 2

;; Alignment A1 contains Latin square [[1 2 3][2 3 1][3 1 2]], alignments A2, A3, B1, B2, B3 contain no Latin squares, and alignments C1, C2, C3 contain [[2 1][1 2]]. Thus in this case V includes one Latin square of order 3 and one of order 2 which is included three times.
;; Our aim is to implement a function which accepts a vector of vectors V as an argument, and returns a map which keys and values are integers. Each key should be the order of a Latin square included in V, and its value a count of different Latin squares of that order included in V. If V does not include any Latin squares an empty map should be returned. In the previous example the correct output of such a function is {3 1, 2 1} and not {3 1, 2 3}.
;; 1 Of course, we can consider sequences instead of vectors.
;; 2 Length of a vector is the number of elements in the vector.

(require '[clojure.test :refer [deftest is testing run-tests]])

(comment

  "list? doesn't return true for a seq!"

  "So i was fighting for a very long time in producing all of the possible alignments. After that it was pretty much trivial
to get the rest, even though I made some simple mistakes in determining correct ranges. Unfortunately this solution is too slow now.
I need to make it faster."

  "Latin squares seems to be too slow. Either we want to avoid solving it or we want to speed it up even further"

  "I guess the next thing to try would be to use a single vector to describe the whole latin square. This would avoid nested get-in
calls. You'd have to navigate the square knowing that for example in a 10x10 square the coordinate 11 is the first cell in the second row,
or (1, 0) in terms of (y, x).

My theory is that I've got only a small gap to reach before it's quick enough."
  )

(defn spy [id x]
  (println id x) x)

(def __
  (fn [V]
    (let [height (count V)
          ^Integer width (->> (map count V) (apply max))]
      (letfn [(positions [v]
                (let [^Integer diff (- width (count v))]
                  (for [i (range (inc diff))]
                    (vec (concat (repeat i nil) v (repeat (- diff i) nil))))))
              (alignments [[v & vs]]
                (if vs
                  (let [alignments-vs (alignments vs)]
                    (if (seq? (ffirst alignments-vs))
                      (map (fn [configurations]
                             (for [conf configurations
                                   option v]
                               (cons option conf))) alignments-vs)
                      (map (fn [configurations]
                             (for [conf configurations
                                   option v]
                               (cons option (list conf)))) alignments-vs)))
                  (list v)))
              (latin-squares [processed-squares A ^Integer n]
                (let [^Integer dec-n (dec n)
                      ^Integer max-w (- width n)
                      ^Integer max-h (- height n)
                      starting-points (for [y (range (inc max-h))
                                            x (range (inc max-w))
                                            :when (let [+ydecn (+ y dec-n)
                                                        +xdecn (+ x dec-n)
                                                        +yrow (get A +ydecn)]
                                                    (and (get +yrow x)
                                                         (get +yrow +xdecn)
                                                         (let [yrow (get A y)]
                                                           (get yrow +xdecn)
                                                           (get yrow x))))] [y x])]
                  (reduce
                   (fn [[latin not-latin :as processed-squares] [^Integer y ^Integer x]]
                     (let [S (->> (subvec A y (+ n y))
                                  (mapv #(subvec % x (+ n x))))]
                       (cond (not-latin S) processed-squares
                             (latin S) processed-squares
                             :else (if (when (every? (fn [row]
                                                       (loop [found (transient #{})
                                                              [r & rs] row]
                                                         (if-not r false
                                                                 (if (found r) false
                                                                     (if-not (seq rs) true
                                                                             (recur (conj! found r) rs)))))) S)
                                         (when (loop [y 0
                                                      x 0
                                                      found (transient #{})]
                                                 (if (= dec-n x y) true
                                                     (let [v (get-in S [y x])]
                                                       (if (found v) false
                                                           (let [y' (inc y)]
                                                             (if (= y n)
                                                               (recur 0 (inc x) (transient #{}))
                                                               (recur (inc y) x (conj! found v))))))))
                                           (let [all-vals (mapcat concat S)
                                                 freqs (frequencies all-vals)]
                                             (->> (keys freqs) count (= n)))))
                                     (list (conj latin S) not-latin)
                                     (list latin (conj not-latin S))))))
                   processed-squares starting-points)))]
        (let [^Integer width (->> (map count V) (apply max))]
          (->> (map positions V)
               (alignments)
               first ;; There's an extra list wrapping at this level
               (map vec)
               (reduce (fn [processed alignment]
                         (->> (range 2 (inc width))
                              (reduce (fn [processed ^Integer width]
                                        (latin-squares processed alignment width)) processed))) '(#{} #{}))
               ;; Collect
               first
               (map count)
               frequencies))))))


(comment "Timing the solution"
         (time
          (dotimes [_ 100] (__ [[8 6 7 3 2 5 1 4]
                                [6 8 3 7]
                                [7 3 8 6]
                                [3 7 6 8 1 4 5 2]
                                [1 8 5 2 4]
                                [8 1 2 4 5]]))))

;;"Elapsed time: 5139.755102 msecs"
;;"Elapsed time: 5071.759706 msecs" <- reused range calls, I don't think it had an effect
;; "Elapsed time: 399.252876 msecs" <- give up if square contains nils. This is probably enough!
;; 4clojure doesn't think so. It must be a pretty slow machine.
;; "Elapsed time: 631.49424 msecs" <- give up when rows are not ok. Not improving
;; "Elapsed time: 461.786602 msecs" <- give up when cols are not ok. Not improving!
;; "Elapsed time: 460.712164 msecs" <- reuse already gathered all-vals. Not improving!
;; "Elapsed time: 230.110953 msecs" <- starting points: do not start where you can't fit a square of requested size
;; Not enough! Wow. Clojure has become much faster from the day
;; "Elapsed time: 256.084199 msecs" <- intead of sets use distinct
;; "Elapsed time: 207.271753 msecs" <- Looks like sets are faster
;; "Elapsed time: 234.568465 msecs" <- avoid iffing inside a for loop
;; "Elapsed time: 214.715225 msecs" <- avoid iffing inside a for loop 2
;; "Elapsed time: 211.766895 msecs" <- avoid redefining latinsquares
;; starting to use rounds of 100
;; "Elapsed time: 13955.080994 msecs"
;; "Elapsed time: 14065.124266 msecs" <- skip cols when possible
;; "Elapsed time: 13390.073665 msecs" <- removed one seq call
;; "Elapsed time: 12844.722847 msecs" <- type hinting on functions
;; "Elapsed time: 12940.255705 msecs" <- even more type hints
;; "Elapsed time: 11299.149679 msecs" <- further pare down starting points by looking into all corners of the square
;; "Elapsed time: 10056.425205 msecs" <- using subvec instead of nested for loops to gather rows
;; "Elapsed time: 9777.816135 msecs" <- further use of subvec
;; Getting closer! It gets at least some results now. Two larger cases are missing results.
;; "Elapsed time: 9658.719282 msecs" <- give up the moment a duplicate is found on a row
;; "Elapsed time: 9283.02525 msecs" <- give up the moment a duplicate is found on a column
;; Just one more case time outs.
;; "Elapsed time: 5042.315988 msecs" <- basically memoize the results of checking if square is latin.
;; Still final case time outs!
;; "Elapsed time: 5262.872382 msecs" <- check not latin ones first
;; "Elapsed time: 4988.893582 msecs" <- give up the moment we find a duplicate in a column
;; "Elapsed time: 4931.172183 msecs" <- remove unnecessary lets
;; "Elapsed time: 4918.844575 msecs" <- inlined collect-squares
;; "Elapsed time: 4716.071694 msecs" <- don't count width many times
;; "Elapsed time: 4722.601861 msecs" <- avoid counting height many times
;; Whoops, `reduced` was added in 1.5
;; "Elapsed time: 4929.021578 msecs" <- avoid counting max width and height many times
;; "Elapsed time: 4911.853644 msecs" <- type-hint new integers
;; "Elapsed time: 4353.290933 msecs" <- avoid needless checks if going over range by creating correct range to begin with
;; "Elapsed time: 4047.895034 msecs" <- more strategic check to pare down squares before thorough check
;; "Elapsed time: 3992.484906 msecs" <- combine nil check with unique row value check
;; "Elapsed time: 3962.03017 msecs" <- avoid finding all values before it's needed
;; "Elapsed time: 3946.890454 msecs" <- avoid extra recur when iterating columns
;; "Elapsed time: 4042.456866 msecs" <- avoid recounting size again
;; "Elapsed time: 3980.162678 msecs" <- even more strategic check to pare down squares
;; "Elapsed time: 3943.343205 msecs" <- avoid calculating (+ x dec-n) many times
;; "Elapsed time: 3909.525471 msecs" <- remove unnecessary check
;; "Elapsed time: 2962.141123 msecs" <- avoid double get-in to the same Y coordinate
;; "Elapsed time: 2999.648337 msecs" <- inlined square function

(deftest one-dimensional-squares
  (let [width 4]
    (letfn [(positions [v]
              (let [diff (- width (count v))]
                (for [i (range (inc diff))]
                  (vec (concat (repeat i nil) v (repeat (- diff i) nil))))))
            (alignments [[v & vs]]
              (if vs
                (let [alignments-vs (alignments vs)]
                  (if (seq? (ffirst alignments-vs))
                    (map (fn [configurations]
                           (for [conf configurations
                                 option v]
                             (cons option conf))) alignments-vs)
                    (map (fn [configurations]
                           (for [conf configurations
                                 option v]
                             (cons option (list conf)))) alignments-vs)))
                (list v)))]
      (let [V [  [2 4 6 3]
               [3 4 6 2]
               [6 2 4]  ]
            width (->> (map count V) (apply max))
            alignments (->> (map positions V)
                            (alignments))]


        (is (= '([2 4 6 3 3 4 6 2 6 2 4 nil] [2 4 6 3 3 4 6 2 nil 6 2 4]) alignments))))))


(deftest testcases-4clojure
  (testing "1st case"

    (is
     (= (__ '[[A B C D]
              [A C D B]
              [B A D C]
              [D C A B]])
        {}))



    (is

     (= (__ '[[A B C D E F]
              [B C D E F A]
              [C D E F A B]
              [D E F A B C]
              [E F A B C D]
              [F A B C D E]])
        {6 1}))

    (is
     (= (__ '[[A B C D]
              [B A D C]
              [D C B A]
              [C D A B]])
        {4 1, 2 4}))

    (is
     (= (__ '[[B D A C B]
              [D A B C A]
              [A B C A B]
              [B C A B C]
              [A D B C A]])
        {3 3}))


    (is

     (= (__ [  [2 4 6 3]
             [3 4 6 2]
             [6 2 4]  ])
        {}))

    (is



     (= (__ [[1]
             [1 2 1 2]
             [2 1 2 1]
             [1 2 1 2]
             []       ])
        {2 2}))

    (is
     (= (__ [[3 1 2]
             [1 2 3 1 3 4]
             [2 3 1 3]    ])
        {3 1, 2 2}))

    (is

     (= (__ [[8 6 7 3 2 5 1 4]
             [6 8 3 7]
             [7 3 8 6]
             [3 7 6 8 1 4 5 2]
             [1 8 5 2 4]
             [8 1 2 4 5]])
        {4 1, 3 1, 2 7}))))


(comment "Timing and quickly testing certain things"

         (time (dotimes [_ 100000000]
                 (for [i (range (inc 3))]
                   (vec (concat (repeat i nil) [1 2 3] (repeat (- 3 i) nil))))))

         (time (dotimes [_ 100000000]
                 (map #(vec (concat (repeat % nil) [1 2 3] (repeat (- 3 %) nil))) (range (inc 3)))))

         (let [vls (range 100)]
           (letfn [(step [i coll]
                     (when (< i 10)
                       (cons (partition 1 10 coll) (step (inc i) (drop 1 coll)))))]
             (step 0 vls)))

(time
 (dotimes [_ 100000000]
   (let [diff 3
         v [1 2]
         nils (repeat nil)]

     (for [i (range (inc diff))]
       (vec (concat (take i nils) v (take (- diff i) nils)))
       #_(vec (loop [result
                     (loop [vs v
                            result
                            (loop [result '()
                                   j (- diff i)]
                              (if (<= j 0) result
                                  (recur (cons nil result) (dec j))))]
                       (if-not (seq vs) result
                               (recur (rest vs) (cons (first vs) result))))
                     j i]
                (if (<= j 0) result
                    (recur (cons nil result) (dec j))))) ))))
         )
