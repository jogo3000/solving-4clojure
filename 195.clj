;; Parentheses... Again

;; Difficulty:	Medium
;; Topics:	math combinatorics

;; In a family of languages like Lisp, having balanced parentheses is a defining feature of the language. Luckily, Lisp has almost no syntax, except for these "delimiters" -- and that hardly qualifies as "syntax", at least in any useful computer programming sense.
;; It is not a difficult exercise to find all the combinations of well-formed parentheses if we only have N pairs to work with. For instance, if we only have 2 pairs, we only have two possible combinations: "()()" and "(())". Any other combination of length 4 is ill-formed. Can you see why?
;; Generate all possible combinations of well-formed parentheses of length 2n (n pairs of parentheses). For this problem, we only consider '(' and ')', but the answer is similar if you work with only {} or only [].
;; There is an interesting pattern in the numbers!

(comment "Consider a stack based approach. You can either push to nest or pop and push to make a sibling. You can't pop if there's nothing in the stack.

Wait. Is it a sequence of :sibling :nest :sibling ...
")

(ns user.test-test
  (:require [clojure.test :refer [deftest is]]))

(def __
  (fn [pairs]
    (let [result (atom #{})]
      (letfn [(step [exp open closed]
                (if (= open closed pairs) (swap! result #(conj % exp))
                    (do
                      (when (< open pairs) (step (str exp "(") (inc open) closed))
                      (when (< closed open) (step (str exp ")") open (inc closed))))))]
        (step "" 0 0))
      @result)))

(__ 3)


(map count (map __ (range 10)))
;; => (0 1 2 5 14 42 132 429 1430 4862)
;; This was the amount of possible orderings from my earlier solutions. It starts going south at 4 levels already.
;; => (0 1 2 5 13 34 89 233 610 1597)
(+ 0 1 2 5 13 34 89 233 610)

(deftest foreclojure-tests

  (is (= [#{""} #{"()"} #{"()()" "(())"}] (map (fn [n] (__ n)) [0 1 2])))

  (is (= #{"((()))" "()()()" "()(())" "(())()" "(()())"} (__ 3)))

  (is (= 16796 (count (__ 10))))

  (is (= (nth (sort (filter #(.contains ^String % "(()()()())") (__ 9))) 6) "(((()()()())(())))"))

  (is (contains? (__ 9) "(((()()()())(())))"))

  (is (= (nth (sort (__ 12)) 5000) "(((((()()()()()))))(()))")))
