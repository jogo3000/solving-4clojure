;; Squares Squared

;; Difficulty:	Hard
;; Topics:	data-juggling

;; Create a function of two integer arguments: the start and end, respectively. You must create a vector of strings which renders a 45° rotated square of integers which are successive squares from the start point up to and including the end point. If a number comprises multiple digits, wrap them around the shape individually. If there are not enough digits to complete the shape, fill in the rest with asterisk characters. The direction of the drawing should be clockwise, starting from the center of the shape and working outwards, with the initial direction being down and to the right.

(defn spy [id x]
  (println id x) x)

(comment "My first instinct was to create a square and then rotate it. After all, it ended up being much simpler to just generate the final
coordinates for every cell.
 - I find the successive squares, then their digits.
 - Then I find out how many values are missing from a complete square.
 - Then I append a filler sequence of *s so I can draw a complete square
 - Then it's a matter of assigning coordinates to the digits and filler characters one by one. I check if I need to turn by looking if there's room in the right hand side of the matrix
 - Finally render the matrix")

(def __
  (fn [start end]
    (letfn [(digits [n]
              (when (pos? n)
                (cons (rem n 10) (digits (quot n 10)))))
            (fill [colls]
              (let [numbers (->> (map count colls)
                                 (apply +))
                    sizes (iterate (fn [{n :n inc :inc}]
                                     {:n (+ inc n)
                                      :inc (+ inc 2)}) {:n 1 :inc 3})
                    required (->> (drop-while #(> numbers (:n %)) sizes)
                                  first
                                  :n)
                    missing (- required numbers)]
                (concat colls (repeat missing (list \*)))))
            (right [[y x]]
              [(dec y) (inc x)])
            (down [[y x]]
              [(dec y) (dec x)])
            (left [[y x]]
              [(inc y) (dec x)])
            (up [[y x]]
              [(inc y) (inc x)])
            (advance [{coord :cursor direction :direction graph :graph}]
              (let [next-cell (case direction
                                :right (right coord)
                                :down (down coord)
                                :left (left coord)
                                :up (up coord))
                    next-dir (cond
                               (= direction :right) (if (get graph (down next-cell)) :right :down)
                               (= direction :down) (if (get graph (left next-cell)) :down :left)
                               (= direction :left) (if (get graph (up next-cell)) :left :up)
                               (= direction :up) (if (get graph (right next-cell)) :up :right))]
                {:graph graph :cursor next-cell :direction next-dir}))]
      (let [graph  (->> (iterate #(* % %) start)
                        (take-while #(<= % end))
                        (map (comp reverse digits))
                        fill
                        (reduce (fn [all ds]
                                  (reduce (fn [{:keys [cursor graph direction] :as m} d]
                                            (->> (assoc m :graph (assoc graph cursor d))
                                                 (advance))) all ds)) {:cursor [0 0]
                                                                       :direction :right
                                                                       :graph {}})
                        :graph)
            coords (keys graph)
            min-y (apply min (map first coords))
            max-y (apply max (map first coords))
            min-x (apply min (map second coords))
            max-x (apply max (map second coords))
            width (- max-x min-x)
            height (- max-y min-y)]
        (->> (for [y (range max-y (dec min-y) -1)]
               (for [x (range min-x (inc max-x))]
                 (get graph [y x] \space)))
             (mapv #(apply str %)))))))

(= (__ 2 2) ["2"])

(= (__ 2 4) [" 2 "
             "* 4"
             " * "])

(= (__ 3 81) [" 3 "
              "1 9"
              " 8 "])

(= (__ 4 20) [" 4 "
              "* 1"
              " 6 "])

(= (__ 2 256) ["  6  "
               " 5 * "
               "2 2 *"
               " 6 4 "
               "  1  "])

(= (__ 10 10000) ["   0   "
                  "  1 0  "
                  " 0 1 0 "
                  "* 0 0 0"
                  " * 1 * "
                  "  * *  "
                  "   *   "])
