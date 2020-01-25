;; For Science!
;; Difficulty:	Hard
;; Topics:	game

;; A mad scientist with tenure has created an experiment tracking mice in a maze. Several mazes have been randomly generated, and you've been tasked with writing a program to determine the mazes in which it's possible for the mouse to reach the cheesy endpoint. Write a function which accepts a maze in the form of a collection of rows, each row is a string where:

;;     spaces represent areas where the mouse can walk freely
;;     hashes (#) represent walls where the mouse can not walk
;;     M represents the mouse's starting point
;;     C represents the cheese which the mouse must reach

;; The mouse is not allowed to travel diagonally in the maze (only up/down/left/right), nor can he escape the edge of the maze. Your function must return true iff the maze is solvable by the mouse.

(def __
  "Initial thoughts:
  - It's a tree of possible moves. The Maze ain't very big. Breadth first search?
  - Iterate moves and remember where you've been. May paint yourself into a corner if you only try once.
  - Breadth / depth first search and give up if painting yourself to a corner

  Depth first version
  (find-cheese-depth-first [used-locations mouse]
    (let [possible-moves (moves used-locations mouse)]
       (if (some cheese? possible-moves) true
           (some #(find-cheese-depth-first (conj used-locations %) %) possible-moves))))

  Findings:
  - Depth first search is slow when lots of free space and cheese is in a remote inaccessible area. Lots of iterations, unless repetition can be avoided.
  - Change it into a breadth first search
  - some? wasn't available in 1.4"
  (fn [maze]
    (let [maze-height (count maze)
          maze-width (count (first maze))]
      (letfn [(look [[y x]]
                (get-in maze [y x]))
              (north [[y x]]
                [(dec y) x])
              (south [[y x]]
                [(inc y) x])
              (east [[y x]]
                [y (inc x)])
              (west [[y x]]
                [y (dec x)])
              (on-map? [location]
                (boolean (look location)))
              (floor? [location]
                (not= \# (look location)))
              (mouse? [location]
                (= \M (look location)))
              (cheese? [location]
                (= \C (look location)))
              (search-with [pred]
                (first (for [y (range maze-height)
                             x (range maze-width)
                             :when (pred [y x])]
                         [y x])))]
        (let [mouse (search-with mouse?)
              cheese (search-with cheese?)]
          (letfn [(moves [used-locations mouse]
                    (->> (map #(% mouse) [north south east west])
                         (filter (complement used-locations))
                         (filter on-map?)
                         (filter #(or (floor? %) (cheese? %)))))]
            (loop [used #{mouse}]
              (let [wavefront (into used (mapcat #(moves used %) used))]
                (cond (wavefront cheese) true
                      (= wavefront used) false
                      :else (recur wavefront))))))))))

(= true  (__ ["M   C"]))

(= false (__ ["M # C"]))

(= true  (__ ["#######"
              "#     #"
              "#  #  #"
              "#M # C#"
              "#######"]))

(= false (__ ["########"
              "#M  #  #"
              "#   #  #"
              "# # #  #"
              "#   #  #"
              "#  #   #"
              "#  # # #"
              "#  #   #"
              "#  #  C#"
              "########"]))

(= false (__ ["M     "
              "      "
              "      "
              "      "
              "    ##"
              "    #C"]))

(= true  (__ ["C######"
              " #     "
              " #   # "
              " #   #M"
              "     # "]))

(= true  (__ ["C# # # #"
              "        "
              "# # # # "
              "        "
              " # # # #"
              "        "
              "# # # #M"]))
