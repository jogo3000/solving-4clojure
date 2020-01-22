;; Transitive Closure

;; Difficulty:	Hard
;; Topics:	set-theory


;; Write a function which generates the transitive closure of a binary relation. The relation will be represented as a set of 2 item vectors.

(def __
  (fn [coll]
    (let [relations (into {} coll)]
      (letfn [(step [relation & [start]]
                (println relation start)
                (let [k (second relation)
                      b (relations k)
                      start (if start start relation)]
                  (when b (cons [(first start) b] (step [k b] start)))))]
        (into coll (mapcat step coll))))))


(__ #{[8 4] [9 3] [4 2] [27 9]})

(let [divides #{[8 4] [9 3] [4 2] [27 9]}]
  (= (__ divides) #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]}))


(__ #{["cat" "man"] ["man" "snake"] ["spider" "cat"]})
(let [more-legs
      #{["cat" "man"] ["man" "snake"] ["spider" "cat"]}]
  (= (__ more-legs)
     #{["cat" "man"] ["cat" "snake"] ["man" "snake"]
       ["spider" "cat"] ["spider" "man"] ["spider" "snake"]}))

(let [progeny
      #{["father" "son"] ["uncle" "cousin"] ["son" "grandson"]}]
  (= (__ progeny)
     #{["father" "son"] ["father" "grandson"]
       ["uncle" "cousin"] ["son" "grandson"]}))
