;; Love Triangle

;; Difficulty:	Hard
;; Topics:	search data-juggling

;; Everyone loves triangles, and it's easy to understand why—they're so wonderfully symmetric (except scalenes, they suck).
;; Your passion for triangles has led you to become a miner (and part-time Clojure programmer) where you work all day to chip out isosceles-shaped minerals from rocks gathered in a nearby open-pit mine. There are too many rocks coming from the mine to harvest them all so you've been tasked with writing a program to analyze the mineral patterns of each rock, and determine which rocks have the biggest minerals.
;; Someone has already written a computer-vision system for the mine. It images each rock as it comes into the processing centre and creates a cross-sectional bitmap of mineral (1) and rock (0) concentrations for each one.
;; You must now create a function which accepts a collection of integers, each integer when read in base-2 gives the bit-representation of the rock (again, 1s are mineral and 0s are worthless scalene-like rock). You must return the cross-sectional area of the largest harvestable mineral from the input rock, as follows:
;;     The minerals only have smooth faces when sheared vertically or horizontally from the rock's cross-section
;;     The mine is only concerned with harvesting isosceles triangles (such that one or two sides can be sheared)
;;     If only one face of the mineral is sheared, its opposing vertex must be a point (ie. the smooth face must be of odd length), and its two equal-length sides must intersect the shear face at 45° (ie. those sides must cut even-diagonally)
;;     The harvested mineral may not contain any traces of rock
;;     The mineral may lie in any orientation in the plane
;;     Area should be calculated as the sum of 1s that comprise the mineral
;;     Minerals must have a minimum of three measures of area to be harvested
;;     If no minerals can be harvested from the rock, your function should return nil

(comment
  "What an annoying problem statement!

Thoughts: start from every possible location and grow a triangle until growth is blocked.
- How to identify that you can have two sided triangle?
- How to identify narrowing in vertical or horizontal direction?
- I don't need to identify squares. They are only interested in triangles.

- I don't need to look into every direction, I can just rotate the 'mine' ")

(defn spy [id x]
  (println id x) x)

(def __
  (fn [xs]
    (letfn [(bits [n]
              (when (pos? n)
                (cons (rem n 2) (bits (quot n 2)))))
            (pad-to [n bs]
              (-> (count bs)
                  (as-> w (- n w))
                  (repeat 0)
                  (concat bs)))]
      (let [width (count (bits (apply max xs)))
            mine (->> (map bits xs)
                      (map #(pad-to width %)))]
        (letfn [(left [start end] [(dec start) end])
                (right [start end] [start (inc end)])
                (widen [dir start end rows]
                  (if-not (and (>= start 0) (seq rows)) 0
                          (let [r (first rows)
                                rs (rest rows)
                                cells (for [x (range start (inc end))
                                            :while (< x (count r))]
                                        (nth r x))
                                row-width (count cells)
                                [start' end'] (dir start end)]
                            (if-not (and (= row-width (inc (- end start)))
                                     (every? #(= % 1) cells))
                              0
                              (inc
                                 (widen dir start' end' rs))))))
                (size [depth]
                  (->> (iterate (fn [[n v]]
                                  (let [n' (inc n)]
                                    [n' (+ n' v)])) [0 0])
                       (drop-while #(< (first %) depth))
                       first second))
                (mirror [depth]
                  (let [area (size depth)]
                    (- (* area 2) depth)))
                (explore [start rows]
                  (let [left-side (widen left start start rows)
                        right-side (widen right start start rows)]
                    (cond
                      (< left-side right-side) (size right-side)
                      (> left-side right-side) (size left-side)
                      (= left-side right-side) (mirror left-side))))
                (starting-positions [row]
                  (for [i (range (count row))
                        :when (-> (nth row i) (= 1))] i))
                (explore-level [mine]
                  (let [found-seams (->> (starting-positions (first mine))
                                         (map #(explore % mine))
                                         (filter #(>= % 3)))]
                    (when (seq found-seams)
                      (apply max found-seams))))]
          (let [levels (take-while seq (iterate rest mine))]
            (let [seams (->> (map explore-level levels)
                             (filter boolean))]
              (when (seq seams)
                (apply max seams))))
          )))))



(= 10 (__ [15 15 15 15 15]))
; 1111      1111
; 1111      *111
; 1111  ->  **11
; 1111      ***1
; 1111      ****

(= 15 (__ [1 3 7 15 31]))
; 00001      0000*
; 00011      000**
; 00111  ->  00***
; 01111      0****
; 11111      *****

(= 3 (__ [3 3]))
; 11      *1
; 11  ->  **

(= 4 (__ [7 3]))
; 111      ***
; 011  ->  0*1

(= 6 (__ [17 22 6 14 22]))
; 10001      10001
; 10110      101*0
; 00110  ->  00**0
; 01110      0***0
; 10110      10110

(= 9 (__ [18 7 14 14 6 3]))
; 10010      10010
; 00111      001*0
; 01110      01**0
; 01110  ->  0***0
; 00110      00**0
; 00011      000*1

(= nil (__ [21 10 21 10]))
; 10101      10101
; 01010      01010
; 10101  ->  10101
; 01010      01010

(= nil (__ [0 31 0 31 0]))
; 00000      00000
; 11111      11111
; 00000  ->  00000
; 11111      11111
; 00000      00000
