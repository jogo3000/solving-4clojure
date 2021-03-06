;; Set Intersection
;;
;; Difficulty:	Easy
;; Topics:	set-theory
;;
;; Write a function which returns the intersection of two sets. The intersection is the sub-set of items that each set has in common.
;;
;; (= (__ #{0 1 2 3} #{2 3 4 5}) #{2 3})
;; (= (__ #{0 1 2} #{3 4 5}) #{})
;; (= (__ #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})
;;
;; Special Restrictions
;; intersection

(def my-intersect
  (fn [a b]
    (set (filter a b))))


(= (my-intersect #{0 1 2 3} #{2 3 4 5}) #{2 3})
