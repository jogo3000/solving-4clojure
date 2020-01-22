;; Power Set

;; Difficulty:	Medium
;; Topics:	set-theory


;; Write a function which generates the power set of a given set. The power set of a set x is the set of all subsets of x, including the empty set and x itself.

(comment
  "Works, but is too slow"
  (fn [coll]
    (if-not (seq coll) #{#{}}
            (letfn [(powerset [coll]
                      (if-not (seq coll) #{}
                              (let [subsets (for [v coll]
                                              (let [subset (disj coll v)]
                                                subset))]

                                (->> (into (list coll) subsets)
                                     (into (mapcat powerset subsets))))))]
              (into #{} (powerset coll))))))

(def __
  "This won't work in 4clojure as clojure 1.4 doesn't have transducers"
  (fn [coll]
    (into #{} (map set)
          (reduce (fn [zoll v]
                    (reduce
                     (fn [woll w]
                       (cons (cons v w) woll))
                     zoll zoll)) (list (list)) coll))))




(= (__ #{1 :a})
   #{#{1 :a} #{:a} #{} #{1}})

(= (__ #{})
   #{#{}})

(= (__ #{1 2 3})
   #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})

(count (__ (into #{} (range 9))))

(= (count (__ (into #{} (range 10)))) 1024)
