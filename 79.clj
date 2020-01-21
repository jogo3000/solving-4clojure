;; Triangle Minimal Path

;; Difficulty:	Hard
;; Topics:	graph-theory


;; Write a function which calculates the sum of the minimal path through a triangle. The triangle is represented as a collection of vectors. The path should start at the top of the triangle and move to an adjacent number on the next row until the bottom of the triangle is reached.

(def __
  (fn [t]
    ((fn [v]
        ((fn step [x y]
           (let [left (get-in v [x y])]
             (if-not left 0
                     (let [right (get-in v [x (inc y)])]
                       (if-not right
                         (+ left (step (inc x) y))
                         (min
                           (+ left (step (inc x) y))
                           (+ right (step (inc x) (inc y)))))))))
         0 0)) (into [] t))))

(= 7 (__ '([1]
           [2 4]
           [5 1 4]
           [2 3 4 5])))
;; 1->2->1->3

(get-in [[1] [2 4] [5 1 4] [2 3 4 5]] [1 1])

(= 20 (__ '([3]
            [2 4]
            [1 9 3]
            [9 9 2 4]
            [4 6 6 7 8]
            [5 7 3 5 1 4])))
;; 3->4->3->2->7->1
