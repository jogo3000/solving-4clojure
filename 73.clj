;; Analyze a Tic-Tac-Toe Board
;; Difficulty:	Hard
;; Topics:	game

;; A tic-tac-toe board is represented by a two dimensional vector. X is represented by :x, O is represented by :o, and empty is represented by :e. A player wins by placing three Xs or three Os in a horizontal, vertical, or diagonal row. Write a function which analyzes a tic-tac-toe board and returns :x if X has won, :o if O has won, and nil if neither player has won.

;; (= nil (__ [[:e :e :e]
;;             [:e :e :e]
;;             [:e :e :e]]))
;; (= :x (__ [[:x :e :o]
;;            [:x :e :e]
;;            [:x :e :o]]))
;; (= :o (__ [[:e :x :e]
;;            [:o :o :o]
;;            [:x :e :x]]))
;; (= nil (__ [[:x :e :o]
;;             [:x :x :e]
;;             [:o :x :o]]))
;; (= :x (__ [[:x :e :e]
;;            [:o :x :e]
;;            [:o :e :x]]))
;; (= :o (__ [[:x :e :o]
;;            [:x :o :e]
;;            [:o :e :x]]))
;; (= nil (__ [[:x :o :x]
;;             [:x :o :x]
;;             [:o :x :o]]))


(def analyze
  (fn [[row1 row2 row3]]
    (#{:x :o}
     (some #(if (apply = %) (first %))
           (concat [row1 row2 row3]
                   (->> (apply interleave [row1 row2 row3]) (partition 3))
                   [[(row1 0) (row2 1) (row3 2)]
                    [(row1 2) (row2 1) (row3 0)]])))))

(analyze  [[:x :e :e]
           [:o :x :e]
           [:o :e :x]])

(analyze [[:x :x :x]
          [:e :e :e]
          [:e :e :e]])

(analyze [[:x :e :e]
          [:x :e :e]
          [:x :e :e]])

(->> (apply interleave [[:x :e :o]
                        [:x :e :o]
                        [:x :e :o]])
     (partition 3))
