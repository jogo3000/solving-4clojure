;; Anagram Finder
;; Difficulty:	Medium
;; Topics:

;; Write a function which finds all the anagrams in a vector of words. A word x is an anagram of word y if all the letters in x can be rearranged in a different order to form y. Your function should return a set of sets, where each sub-set is a group of words which are anagrams of each other. Each sub-set should have at least two words. Words without any anagrams should not be included in the result.

;; (= (__ ["meat" "mat" "team" "mate" "eat"])
;;    #{#{"meat" "team" "mate"}})
;; (= (__ ["veer" "lake" "item" "kale" "mite" "ever"])
;;    #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})

(def anagrams
  (fn [xs]
    (->> (map (juxt
               (fn [s]
                 (apply merge-with + (for [c s] {c 1})))
               vector) xs)
         (map #(apply hash-map %))
         (apply merge-with concat)
         (filter (fn [[k v]] (>= (count v) 2)))
         (map val)
         (map set)
         set)))


(comment
  (anagrams ["meat" "mat" "team" "mate" "eat"])

)
