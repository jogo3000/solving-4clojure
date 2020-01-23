;; Generating k-combinations
;; Difficulty:	Medium
;; Topics:	seqs combinatorics

;; Given a sequence S consisting of n elements generate all k-combinations of S, i. e. generate all possible sets consisting of k distinct elements taken from S. The number of k-combinations for a sequence is equal to the binomial coefficient.

(def __
  (fn [k coll]
    (cond (> k (count coll)) #{}
          :else
          (loop [i 1
                 K (into #{} (map hash-set coll))]
            (if (>= i k) K
                (recur (inc i) (->> (mapcat (fn [k] (map (fn [l] (conj k l)) coll)) K)
                                    (filter #(< i (count %)))
                                    set)))))))

(= (__ 1 #{4 5 6}) #{#{4} #{5} #{6}})

(= (__ 10 #{4 5 6}) #{})

(= (__ 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}})

(= (__ 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}

                         #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}})

(= (__ 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}})

(= (__ 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}

                                    #{:a "abc"} #{:a "efg"} #{"abc" "efg"}})
