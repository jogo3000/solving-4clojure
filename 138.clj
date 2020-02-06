;; Squares Squared

;; Difficulty:	Hard
;; Topics:	data-juggling

;; Create a function of two integer arguments: the start and end, respectively. You must create a vector of strings which renders a 45° rotated square of integers which are successive squares from the start point up to and including the end point. If a number comprises multiple digits, wrap them around the shape individually. If there are not enough digits to complete the shape, fill in the rest with asterisk characters. The direction of the drawing should be clockwise, starting from the center of the shape and working outwards, with the initial direction being down and to the right.

(require '[clojure.test :refer [deftest is testing]])

(defn spy [id x]
  (println id x) x)

(defn advance [{[y x] :cursor direction :direction graph :graph}]
  (let [next-cell (case direction
                    :right [y (inc x)]
                    :down [(dec y) (inc x)]
                    :left [y (dec x)]
                    :up [(inc y) x])
        next-dir (cond
                   (= direction :right) (if (get graph [(dec (first next-cell)) (second next-cell)]) :right :down)
                   (= direction :down) (if (get graph [(first next-cell) (dec (second next-cell))]) :down :left)
                   (= direction :left) (if (get graph [(inc (first next-cell)) (second next-cell)]) :left :up)
                   (= direction :up) (if (get graph [(first next-cell) (inc (second next-cell))]) :up :right))]
    {:graph graph :cursor next-cell :direction next-dir}))

(take 10 )

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
            (advance [{[y x] :cursor direction :direction graph :graph}]
              (let [next-cell (case direction
                                :right [y (inc x)]
                                :down [(dec y) x]
                                :left [y (dec x)]
                                :up [(inc y) x])
                    next-dir (cond
                               (= direction :right) (if (get graph [(dec (first next-cell)) (second next-cell)]) :right :down)
                               (= direction :down) (if (get graph [(first next-cell) (dec (second next-cell))]) :down :left)
                               (= direction :left) (if (get graph [(inc (first next-cell)) (second next-cell)]) :left :up)
                               (= direction :up) (if (get graph [(first next-cell) (inc (second next-cell))]) :up :right))]
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
        (for [y (range max-y (dec min-y) -1)]
          (for [x (range min-x (inc max-x))]
            (get graph [y x])))))))

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
