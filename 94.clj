;; Game of Life
;; Difficulty:	Hard
;; Topics:	game


;; The game of life is a cellular automaton devised by mathematician John Conway.

;; The 'board' consists of both live (#) and dead ( ) cells. Each cell interacts with its eight neighbours (horizontal, vertical, diagonal), and its next state is dependent on the following rules:

;; 1) Any live cell with fewer than two live neighbours dies, as if caused by under-population.
;; 2) Any live cell with two or three live neighbours lives on to the next generation.
;; 3) Any live cell with more than three live neighbours dies, as if by overcrowding.
;; 4) Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

;; Write a function that accepts a board, and returns a board representing the next generation of cells.

(def __
  (fn [board]
    (letfn [(read-cell [y x] (let [val (get-in board [y x])]
                               (if (= \# val) :live :dead)))]
      (let [height (count board)
            width (count (first board))]
        (->> (for [y (range height)
                   x (range width)]
               (let [local-cells (for [x' [-1 0 1]
                                       y' [-1 0 1]]
                                   (if (= 0 x' y') :self (read-cell (+ y y') (+ x x'))))
                     live-count (-> (filter #(= :live %) local-cells) count)
                     dead-count (-> (filter #(= :dead %) local-cells) count)
                     self (read-cell y x)]
                 (if (= self :dead)
                   (if (= live-count 3) \# \space)
                   (cond
                     (< live-count 2) \space
                     (<= 2 live-count 3) \#
                     (> live-count 3) \space))))
             (partition width)
             (map (partial apply str)))))))

(comment
  "I can read cells like this"
  (get-in ["      "
           " ##   "
           " ##   "
           "   ## "
           "   ## "
           "      "] [1 1]) ; => \#
  )


(= (__ ["      "
       " ##   "
       " ##   "
       "   ## "
       "   ## "
       "      "])
  ["      "
   " ##   "
   " #    "
   "    # "
   "   ## "
   "      "])

(= (__ ["     "
       "     "
       " ### "
       "     "
       "     "])
  ["     "
   "  #  "
   "  #  "
   "  #  "
   "     "])

(= (__ ["      "
       "      "
       "  ### "
       " ###  "
       "      "
       "      "])
  ["      "
   "   #  "
   " #  # "
   " #  # "
   "  #   "
   "      "])
