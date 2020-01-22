;; Through the Looking Class
;; Difficulty:	Easy
;; Topics:	fun brain-teaser

;; Enter a value which satisfies the following:

(def __
  java.lang.Class
  )


(let [x __]
  (and (= (class x) x) x))
