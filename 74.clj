;; Filter Perfect Squares
;; Difficulty:	Medium
;; Topics:

;; Given a string of comma separated integers, write a function which returns a new comma separated string that only contains the numbers which are perfect squares.

;; (= (__ "4,5,6,7,8,9") "4,9")
;; (= (__ "15,16,25,36,37") "16,25,36")

(def squares
  (fn [s]
    (->> (clojure.string/split s #",")
         (filter (fn [s]
                   (let [n (java.lang.Integer/parseInt s)]
                     (->> (range n)
                          (some #(= (* % %) n))))))
         (clojure.string/join ","))))


(squares "4,5,6,7,8,9")
