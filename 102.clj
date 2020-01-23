;; intoCamelCase

;; Difficulty:	Medium
;; Topics:	strings


;; When working with java, you often need to create an object with fieldsLikeThis, but you'd rather work with a hashmap that has :keys-like-this until it's time to convert. Write a function which takes lower-case hyphen-separated strings and converts them to camel-case strings.

(def __
  (fn [s]
    (letfn [(step [f xs]
              (cond
                (not (seq xs)) ""
                (= \- (first xs)) (step #(-> % int (- 32) char) (rest xs))
                :else (str (f (first xs)) (step identity (rest xs)))))]
      (step identity s))))

(= (__ "something") "something")
(= (__ "multi-word-key") "multiWordKey")
(= (__ "leaveMeAlone") "leaveMeAlone")
