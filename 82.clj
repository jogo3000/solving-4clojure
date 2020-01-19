;; Word Chains
;; Difficulty:	Hard
;; Topics:	seqs

;; A word chain consists of a set of words ordered so that each word differs by only one letter from the words directly before and after it. The one letter difference can be either an insertion, a deletion, or a substitution. Here is an example word chain:

;; cat -> cot -> coat -> oat -> hat -> hot -> hog -> dog

;; Write a function which takes a sequence of words, and returns true if they can be arranged into one continous word chain, and false if they cannot.

;; (= true (__ #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}))
;; (= false (__ #{"cot" "hot" "bat" "fat"}))
;; (= false (__ #{"to" "top" "stop" "tops" "toss"}))
;; (= true (__ #{"spout" "do" "pot" "pout" "spot" "dot"}))
;; (= true (__ #{"share" "hares" "shares" "hare" "are"}))
;; (= false (__ #{"share" "hares" "hare" "are"}))

;; Recursively check if taken the first element in a seq, if there are any chainables
;; in the rest of the seq. If chainables are found, continue to check if rest of the
;; words are chainable from that word.


(def chainable?
  (fn word-chain? [s]
    (boolean
     (let [x (first s)
           xs (rest s)
           chainables (filter (fn [suspect]
                                ((fn chainable? [differences [x & xs] [y & ys]]
                                   (cond
                                     (> differences 1) false
                                     (and (not (seq? xs)) (not (seq? ys))) (<= differences 1)
                                     (not (seq? xs)) (or (and (zero? differences) (= (count ys) 1)) (and (= differences 1) (zero? (count ys))))
                                     (not (seq? ys)) (or (and (zero? differences) (= (count xs) 1)) (and (= differences 1) (zero? (count xs))))
                                     (= x y) (chainable? differences xs ys)
                                     :else (or (chainable? (inc differences) xs (cons y ys))
                                               (chainable? (inc differences) ys (cons x xs))
                                               (chainable? (inc differences) xs ys)))) 0 x suspect)) xs)]
       (cond
         (and (pos? (count xs))
              (zero? (count chainables))) false
         (and (<= (count xs) 1)
              (pos? (count chainables))) true
         :else
         (some word-chain? (map (fn [chainable] (-> xs set (disj chainable) seq (conj chainable))) chainables)))

       ))))

(comment
  (chainable? #{"abba" "ac/dc"})

  (chainable? #{"abba" "abbat"})

  (chainable? #{"abba" "abbat" "abbath"})

  (= true (chainable? #{"spout" "do" "pot" "pout" "spot" "dot"}))

  (= false (chainable? #{"cot" "hot" "bat" "fat"}))
  )


(
 (fn chainable? [differences [x & xs] [y & ys]]
   (cond
     (> differences 1) false
     (and (not (seq? xs)) (not (seq? ys))) (<= differences 1)
     (not (seq? xs)) (or (and (zero? differences) (= (count ys) 1)) (and (= differences 1) (zero? (count ys))))
     (not (seq? ys)) (or (and (zero? differences) (= (count xs) 1)) (and (= differences 1) (zero? (count xs))))
     (= x y) (chainable? differences xs ys)
     :else (or (chainable? (inc differences) xs (cons y ys))
               (chainable? (inc differences) ys (cons x xs))
               (chainable? (inc differences) xs ys))))

 0 "abba" "abbah")

(comment
  ;; Lähdin eka tekemään testiä pelkälle substitutionille ja ajattelin, että tekisin testit lisäykselle ja poistolle
  (defn substitution? [a b]
    (and (= (count a) (count b))
         (= 1 (->> (interleave a b)
                   (partition 2)
                   (filter #(apply not= %))
                   count)))))
