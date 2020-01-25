;; Win at Tic-Tac-Toe

;; Difficulty:	Hard
;; Topics:	game

;; As in Problem 73, a tic-tac-toe board is represented by a two dimensional vector. X is represented by :x, O is represented by :o, and empty is represented by :e. Create a function that accepts a game piece and board as arguments, and returns a set (possibly empty) of all valid board placements of the game piece which would result in an immediate win.
;; Board coordinates should be as in calls to get-in. For example, [0 1] is the topmost row, center position.

(defn spy [id coll]
  (println id coll) coll)

(def __
  (fn [player board]
    (letfn [(places-of [marker]
              (for [y (range 3)
                    x (range 3)
                    :when (= marker (get-in board [y x]))]
                [y x]))
            (win-with? [move]
              (let [[y x] move]
                (or
                 (apply = (cons player (assoc (get board y) x player)))
                 (apply = (cons player (for [y' (range 3)]
                                         (if (= y' y ) player (get-in board [y' x])))))
                 (apply = (cons player (for [xy (range 3)]
                                         (if (= xy x y) player (get-in board [xy xy])))))
                 (apply = (cons player (for [[x' y'] [[0 2] [1 1] [2 0]]]
                                         (if (= [x' y'] [x y]) player (get-in board [y' x']))))))))]
      (let [moves (places-of :e)]
        (set (filter win-with? moves))))))

(def board [[:o :e :e]
           [:o :x :o]
            [:x :x :e]])

(= (__ :x [[:o :e :e]
           [:o :x :o]
           [:x :x :e]])
   #{[2 2] [0 1] [0 2]})

(= (__ :x [[:x :o :o]
           [:x :x :e]
           [:e :o :e]])
   #{[2 2] [1 2] [2 0]})

(= (__ :x [[:x :e :x]
           [:o :x :o]
           [:e :o :e]])
   #{[2 2] [0 1] [2 0]})

(= (__ :x [[:x :x :o]
           [:e :e :e]
           [:e :e :e]])
   #{})

(= (__ :o [[:x :x :o]
           [:o :e :o]
           [:x :e :e]])
   #{[2 2] [1 1]})
