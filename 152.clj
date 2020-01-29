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

(comment

  "list? doesn't return true for a seq!"

  ;; Useful for checking if all row and col values are distinct
  distinct
  ;; This can be used to align subvectors
  subvec
  )

(defn spy [id x]
  (println id x) x)

;; positions
;; (([2 4 6 3])
;;  ([3 4 6 2])
;;  ([6 2 4 nil] [nil 6 2 4]))
;;

(defn extract-conf [coll]
  (loop [coll coll]
    (if (list? coll) (recur (first coll)) coll)))

(defn alignments [[v & vs]]
  ;; v -> list of lists
  ;; vs -> list of list of lists
  (if (seq vs)
    (map (fn [configurations]
           (for [conf configurations
                 option v]
             (cons option (if (seq? conf) conf (list conf))))) (alignments vs))
    (list v)))

(comment
  "Tests for alignment function"
  (alignments positions)
  (alignments '(([a b c])
                ([d e nil] [nil d e])))

  )
(alignments positions)
;; => ((([2 4 6 3] ([3 4 6 2] [6 2 4 nil])) ([2 4 6 3] ([3 4 6 2] [nil 6 2 4]))))
;; => ((([2 4 6 3] ([3 4 6 2] [6 2 4 nil])) ([2 4 6 3] ([3 4 6 2] [nil 6 2 4]))))


(def __
  (fn [V]
    (let [width (->> (map count V) (apply max))]
      (letfn [(positions [v]
                (let [diff (- width (count v))]
                  (for [i (range (inc diff))]
                    (vec (concat (repeat i nil) v (repeat (- diff i) nil))))))
              (alignments [[v & vs]]
                (if (seq vs)
                  (map (fn [configurations]
                         (for [conf configurations
                               option v]
                           (cons option (if (seq? conf) conf (list conf))))) (alignments vs))
                  (list v)))
              (latin-squares [A n]
                (let [width (->> (map count A) (apply max))
                      height (count A)
                      starting-points (for [y (range (inc height))
                                            x (range (inc width))
                                            :when (and (<= x width)
                                                       (<= y height))] [y x])]
                  (letfn [(latin-square? [[y x]]
                            (let [rows (range y (+ n y))
                                  rows-ok? (->> (for [y' (range y (+ n y))]
                                                  (for [x' (range x (+ n x))] (get-in A [y' x'])))
                                                (map set)
                                                (every? #(= (count %) n)))
                                  cols-ok? (->> (for [x' (range x (+ n x))]
                                                  (for [y' (range y (+ n y))] (get-in A [y' x'])))
                                                (map set)
                                                (every? #(= (count %) n)))
                                  all-vals (for [y' (range y (+ n y))
                                                 x' (range x (+ n x))]
                                             (get-in A [y' x']))
                                  no-nils? (every? identity all-vals)
                                  freqs (frequencies all-vals)
                                  vals-ok? (and (->> (keys freqs) count (= n))
                                                (->> (vals freqs) (apply =)))]
                              (and cols-ok? rows-ok? no-nils? vals-ok?)))]
                    (->> (filter latin-square? starting-points)
                         (map (fn [[y x]]
                                (for [y' (range y (+ n y))
                                      x' (range x (+ n x))]
                                  (get-in A [y' x']))))))))]
        (let [width (->> (map count V) (apply max))]
          (->> (map positions V)
               (alignments)
               first ;; There's an extra list wrapping at this level
               (map vec)
               (mapcat (fn [alignment]
                         (->> (range 2 (inc width))
                              (map #(vector % (set (latin-squares alignment %)))))))
               (filter #(not (empty? (second %))))
               (map #(apply hash-map %))
               (apply merge-with into)
               (map (fn [[k v]]
                      [k (count v)]))
               (into {})))))))


(__ [[3 1 2]
     [1 2 3 1 3 4]
     [2 3 1 3]    ])


(time
 (__ [[8 6 7 3 2 5 1 4]
      [6 8 3 7]
      [7 3 8 6]
      [3 7 6 8 1 4 5 2]
      [1 8 5 2 4]
      [8 1 2 4 5]]))

"Elapsed time: 5139.755102 msecs"
;; => {4 1, 3 1, 2 7}
;; => {4 1, 3 1, 2 3}
;; => {4 1, 3 1, 2 3}
;; should be {4 1, 3 1, 2 7}




(latin-squares [[3 1 2]
                [1 2 3 1 3 4]
                [2 3 1 3]    ] 2)
;; => {3 1, 2 1}
; should be {3 1, 2 2}



(__ '[[A B C D E F]
      [B C D E F A]
      [C D E F A B]
      [D E F A B C]
      [E F A B C D]
      [F A B C D E]])
;; => ({6 #{(A B C D E F B C D E F A C D E F A B D E F A B C E F A B C D F A B C D E)}})
;; => ([[A B C D E F] [B C D E F A] [C D E F A B] [D E F A B C] [E F A B C D] [F A B C D E]])
;; => ((() () () () ((A B C D E F B C D E F A C D E F A B D E F A B C E F A B C D F A B C D E))))


(defn latin-squares [A n]
  (let [width (->> (map count A) (apply max))
        height (count A)
        starting-points (for [y (range (inc n))
                              x (range (inc n))
                              :when (and (<= x width)
                                         (<= y height))] [y x])]
    (letfn [(latin-square? [[y x]]
              (let [rows-ok? (->> (for [y' (range y (+ n y))]
                                    (for [x' (range x (+ n x))] (get-in A [y' x'])))
                                  (map set)
                                  (every? #(= (count %) n)))
                    cols-ok? (->> (for [x' (range x (+ n x))]
                                    (for [y' (range y (+ n y))] (get-in A [y' x'])))
                                  (map set)
                                  (every? #(= (count %) n)))
                    all-vals (for [y' (range y (+ n y))
                                   x' (range x (+ n x))]
                               (get-in A [y' x']))
                    no-nils? (every? identity all-vals)
                    freqs (frequencies all-vals)
                    vals-ok? (and (->> (keys freqs) count (= n))
                                  (->> (vals freqs) (apply =)))]
                (and cols-ok? rows-ok? no-nils? vals-ok?)))]
      (->> (filter latin-square? starting-points)
           (map (fn [[y x]]
                  (for [y' (range y (+ n y))
                        x' (range x (+ n x))]
                    (get-in A [y' x']))))))))


(latin-squares '[[A B C D]
                 [B A D C]
                 [D C B A]
                 [C D A B]] 2)
;; => ((A B B A) (C D D C) (D C C D) (B A A B))
;; => () should be 4


(__ '[[A B C D]
      [A C D B]
      [B A D C]
      [D C A B]])
;; => (([A B C D]) ([A C D B]) ([B A D C]) ([D C A B]))

(= (__ '[[A B C D]
         [A C D B]
         [B A D C]
         [D C A B]])
   {})

(= (__ '[[A B C D E F]
         [B C D E F A]
         [C D E F A B]
         [D E F A B C]
         [E F A B C D]
         [F A B C D E]])
   {6 1})

(= (__ '[[A B C D]
         [B A D C]
         [D C B A]
         [C D A B]])
   {4 1, 2 4})

(= (__ '[[B D A C B]
         [D A B C A]
         [A B C A B]
         [B C A B C]
         [A D B C A]])
   {3 3})

(= (__ [  [2 4 6 3]
        [3 4 6 2]
          [6 2 4]  ])
   {})



(= (__ [[1]
        [1 2 1 2]
        [2 1 2 1]
        [1 2 1 2]
        []       ])
   {2 2})

(= (__ [[3 1 2]
        [1 2 3 1 3 4]
        [2 3 1 3]    ])
   {3 1, 2 2})

(= (__ [[8 6 7 3 2 5 1 4]
        [6 8 3 7]
        [7 3 8 6]
        [3 7 6 8 1 4 5 2]
              [1 8 5 2 4]
              [8 1 2 4 5]])
   {4 1, 3 1, 2 7})
