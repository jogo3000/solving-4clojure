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
  "Too slow, should create an array of all the possible prefixes, check
  https://en.wikipedia.org/wiki/Levenshtein_distance#Recursive"
  (fn [c1 c2]
    (let [solved (atom {})]
      (letfn [(step [s len_s t len_t]
                (cond
                  (zero? len_s) len_t
                  (zero? len_t) len_s
                  :else
                  (get @solved [len_s len_t]
                       (let [cost (if (= (nth s (dec len_s)) (nth t (dec len_t))) 0 1)
                             dist (min (inc (step s (dec len_s) t len_t))
                                       (inc (step s len_s t (dec len_t)))
                                       (+ cost (step s (dec len_s) t (dec len_t))))]
                         (swap! solved (fn [m] (assoc m [len_s len_t] dist)))
                         dist))))]
        (step c1 (count c1) c2 (count c2))))))

(= (__ "Kitten" "sitting") 3)

(= (__ "closure" "clojure") (__ "clojure" "closure") 1)

(= (__ "xyx" "xyyyx") 2)

(= (__ "" "123456") 6)

(= (__ "Clojure" "Clojure") (__ "" "") (__ [] []) 0)

(= (__ [1 2 3 4] [0 2 3 4 5]) 2)

(= (__ '(:a :b :c :d) '(:a :d)) 2)

(= (__ "ttttattttctg" "tcaaccctaccat") 10)

(= (__ "gaattctaatctc" "caaacaaaaaattt") 9)
