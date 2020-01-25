;; Analyze Reversi

;; Difficulty:	Hard
;; Topics:	game

;; Reversi is normally played on an 8 by 8 board. In this problem, a 4 by 4 board is represented as a two-dimensional vector with black, white, and empty pieces represented by 'b, 'w, and 'e, respectively. Create a function that accepts a game board and color as arguments, and returns a map of legal moves for that color. Each key should be the coordinates of a legal move, and its value a set of the coordinates of the pieces flipped by that move.
;; Board coordinates should be as in calls to get-in. For example, [0 1] is the topmost row, second column from the left.

(defn spy [id x]
  (println id x) x)

(def __
  (fn [board marker]
    (let [opponent ({'b 'w 'w 'b} marker)]
      (letfn [(reduce+ [f coll] (when (seq coll) (reduce f coll)))
              (piece-in [coord] (get-in board coord))
              (opponent-in? [coord] (spy (str "check" coord) (= opponent (piece-in coord))))
              (mine-in? [coord] (= marker (piece-in coord)))
              (edible-pieces [dir coord eaten]
                (let [move (dir coord)
                      piece (piece-in coord)]
                  (condp = piece
                    opponent (edible-pieces dir move (cons move eaten))
                    marker eaten
                    nil)))
              (north [[y x]]
                (let [opponents-pieces
                      (for [y' (range (dec y) -1 -1) :while (opponent-in? [y' x])] [y' x])]
                  (let [furthest (->> (map first opponents-pieces) (reduce+ min))]
                    (when-let [y' furthest]
                      (when (mine-in? [(dec y') x]) opponents-pieces)))))
              (south [[y x]]
                (let [opponents-pieces
                      (for [y' (range (inc y) 9) :while (opponent-in? [y' x])] [y' x])]
                  (let [furthest (->> (map first opponents-pieces) (reduce+ max))]
                    (when-let [y' furthest]
                      (when (mine-in? [(inc y') x]) opponents-pieces)))))
              (west [[y x]]
                (let [opponents-pieces
                      (for [x' (range (dec x) -1 -1) :while (opponent-in? [y x'])] [y x'])]
                  (let [furthest (->> (map second opponents-pieces) (reduce+ min))]
                    (when-let [x' furthest]
                      (when (mine-in? [y (dec x')]) opponents-pieces)))))
              (east [[y x]]
                (let [opponents-pieces
                      (for [x' (range (inc x) 9) :while (opponent-in? [y x'])] [y x'])]
                  (let [furthest (->> (map second opponents-pieces) (reduce+ max))]
                    (when-let [x' furthest]
                      (when (mine-in? [y (inc x')]) opponents-pieces)))))
              (flips [move]
                (set (mapcat #(% move) [north south east west])))]
        (let [possible-moves (for [y (range 9)
                                   x (range 9)
                                   :when (= 'e (piece-in [y x]))]
                               [y x])]
          (->> (map (juxt identity flips) possible-moves)
               (filter (comp seq second))
               (into {})))))))

(range 3 -1 -1)

(= {[1 3] #{[1 2]}, [0 2] #{[1 2]}, [3 1] #{[2 1]}, [2 0] #{[2 1]}}
   (__ '[[e e e e]
         [e w b e]
         [e b w e]
         [e e e e]] 'w))

(= {[3 2] #{[2 2]}, [3 0] #{[2 1]}, [1 0] #{[1 1]}}
   (__ '[[e e e e]
         [e w b e]
         [w w w e]
         [e e e e]] 'b))

(= {[0 3] #{[1 2]}, [1 3] #{[1 2]}, [3 3] #{[2 2]}, [2 3] #{[2 2]}}
   (__ '[[e e e e]
         [e w b e]
         [w w b e]
         [e e b e]] 'w))

(= {[0 3] #{[2 1] [1 2]}, [1 3] #{[1 2]}, [2 3] #{[2 1] [2 2]}}
   (__ '[[e e w e]
         [b b w e]
         [b w w e]
         [b w w w]] 'b))
