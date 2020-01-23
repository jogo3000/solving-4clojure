;; Levenshtein Distance
;; Difficulty:	Hard
;; Topics:	seqs
;;
;; Given two sequences x and y, calculate the Levenshtein distance of x and y, i. e. the minimum number of edits needed to transform x into y. The allowed edits are:
;; - insert a single item
;; - delete a single item
;; - replace a single item with another item
;;
;; WARNING: Some of the test cases may timeout if you write an inefficient solution!

(def __
  "https://en.wikipedia.org/wiki/Levenshtein_distance#Recursive"
  (fn [c1 c2]
    (let [source (reduce (fn [m i] (assoc m [i 0] i)) {} (range (inc (count c1))))
          target (reduce (fn [m j] (assoc m [0 j] j)) source (range (inc (count c2))))
          full (reduce (fn [m [i j]]
                                     (let [cost (if (= (nth c1 (dec i)) (nth c2 (dec j))) 0 1)
                                           deletion (inc (m [(dec i) j]))
                                           insertion (inc (m [i (dec j)]))
                                           substitution (+ cost (m [(dec i) (dec j)]))]
                                       (assoc m [i j] (min deletion insertion substitution)))) target (for [j (range 1 (inc (count c1)))
                                                                                                            i (range 1 (inc (count c2)))]
                                                                                                        [j i]))]
      (full [(count c1) (count c2)]))))

(__ "sit" "bit")

(= (__ "Kitten" "sitting") 3)

(= (__ "closure" "clojure") (__ "clojure" "closure") 1)

(= (__ "xyx" "xyyyx") 2)

(= (__ "" "123456") 6)

(= (__ "Clojure" "Clojure") (__ "" "") (__ [] []) 0)

(= (__ [1 2 3 4] [0 2 3 4 5]) 2)

(= (__ '(:a :b :c :d) '(:a :d)) 2)

(= (__ "ttttattttctg" "tcaaccctaccat") 10)

(= (__ "gaattctaatctc" "caaacaaaaaattt") 9)
