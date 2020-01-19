;; Black Box Testing
;; Difficulty:	Medium
;; Topics:	seqs testing

;; Clojure has many sequence types, which act in subtly different ways. The core functions typically convert them into a uniform "sequence" type and work with them that way, but it can be important to understand the behavioral and performance differences so that you know which kind is appropriate for your application.

;; Write a function which takes a collection and returns one of :map, :set, :list, or :vector - describing the type of collection it was given.
;; You won't be allowed to inspect their class or use the built-in predicates like list? - the point is to poke at them and understand their behavior.

;; (= :map (__ {:a 1, :b 2}))
;; (= :list (__ (range (rand-int 20))))
;; (= :vector (__ [1 2 3 4 5 6]))
;; (= :set (__ #{10 (rand-int 5)}))
;; (= [:map :set :vector :list] (map __ [{} #{} [] ()]))

;; Special Restrictions
;; class
;; type
;; Class
;; vector?
;; sequential?
;; list?
;; seq?
;; map?
;; set?
;; instance?
;; getClass

(def test
  (fn [x]
    (let [a1 [:a 1]
          conj-x (conj x a1)
          cons-x (cons a1 x)]
      ;; for lists, conj and cons result in the same collection
      (if (= conj-x cons-x)
        ;; the test must be done again to rule out empty vector
        (if (= (conj conj-x :a) (cons :a conj-x))
          :list
          :vector)
        ;; when vector is conj'd to a map, the first element is considered a key
        (if (contains? conj-x :a)
          :map
          ;; for sets, contains? checks if the value is in the set
          (if (contains? conj-x [:a 1])
            :set
            ;; Everything else has been ruled out
            :vector))))))

(comment (= :map (test {:a 1, :b 2}))
         (= :list (test (range (rand-int 20))))
         (= :vector (test [1 2 3 4 5 6]))
         (= :set (test #{10 (rand-int 5)}))
         (test #{})
         (test {})
         (test '())
         (test [])
         (= [:map :set :vector :list] (map test [{} #{} [] ()]))
         )
