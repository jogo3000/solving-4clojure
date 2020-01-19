;; Intro to Destructuring 2
;; Difficulty:	Easy
;; Topics:	Destructuring

;; Sequential destructuring allows you to bind symbols to parts of sequential things (vectors, lists, seqs, etc.): (let [bindings* ] exprs*) Complete the bindings so all let-parts evaluate to 3.

;; (= 3
;;   (let [[__] [+ (range 3)]] (apply __))
;;   (let [[[__] b] [[+ 1] 2]] (__ b))
;;   (let [[__] [inc 2]] (__)))

(= 3
  (let [[f a] [+ (range 3)]] (apply f a))
  (let [[[f a] b] [[+ 1] 2]] (f a b))
  (let [[f a] [inc 2]] (f a)))
