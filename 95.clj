;; To Tree, or not to Tree

;; Difficulty:	Easy
;; Topics:	trees


;; Write a predicate which checks whether or not a given sequence represents a binary tree. Each node in the tree must have a value, a left child, and a right child.


(def __
  (fn [t?]
    ((fn step [t?]
       (if-not (coll? t?) (nil? t?)
               (if-not (= (count t?) 3) false
                       (let [[v l r] t?]
                         (and v (step l) (step r)))))) t?)))

(= (__ '(:a (:b nil nil) nil))
   true)

(= (__ '(:a (:b nil nil)))
   false)

(= (__ [1 nil [2 [3 nil nil] [4 nil nil]]])
   true)

(= (__ [1 [2 nil nil] [3 nil nil] [4 nil nil]])
   false)

(= (__ [1 [2 [3 [4 nil nil] nil] nil] nil])
   true)

(= (__ [1
        [2
         [3
          [4 false nil]
          nil]
         nil]
        nil])
   false)

(= (__ [1 [2 [3 [4 false nil] nil] nil] nil])
   false)

(= (__ '(:a nil ()))
   false)
