;; Best Hand

;; Difficulty:	Hard
;; Topics:	strings game

;; Following on from Recognize Playing Cards, determine the best poker hand that can be made with five cards. The hand rankings are listed below for your convenience.
;;     Straight flush: All cards in the same suit, and in sequence
;;     Four of a kind: Four of the cards have the same rank
;;     Full House: Three cards of one rank, the other two of another rank
;;     Flush: All cards in the same suit
;;     Straight: All cards in sequence (aces can be high or low, but not both at once)
;;     Three of a kind: Three of the cards have the same rank
;;     Two pair: Two pairs of cards have the same rank
;;     Pair: Two cards have the same rank
;;     High card: None of the above conditions are met

(defn spy [id x]
  (println id x) x)

(def __
  (fn [cards]
    (let [hand (sort-by (comp {\A 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \J 11 \Q 12 \K 13} second) cards)]
      (letfn [(straight-pattern [s]
                (if-not (re-find #"A" s) s
                        (let [postfix (apply str (rest s))]
                          (str "A" postfix "|" postfix "A"))))]
        (let [freqs (->> (map second hand) frequencies)
              hand-freqs (->> (map first hand) frequencies)
              full-house? (->> (vals freqs) set ((partial = #{2 3})))
              flush? (apply = (map first hand))
              pairs (->> (vals freqs) (filter #(= 2 %)) count)
              triple? (->> (vals freqs) (filter #(= 3 %)) count pos?)
              foursome? (->> (vals freqs) (filter #(= 4 %)) count pos?)
              pair? (= 1 pairs)
              two-pair? (= 2 pairs)
              straight? (re-find (->> (map second hand) (apply str) straight-pattern (re-pattern)) "A23456789TJQKA")]
          (cond
            (and straight? flush?) :straight-flush
            foursome? :four-of-a-kind
            full-house? :full-house
            flush? :flush
            straight? :straight
            triple? :three-of-a-kind
            two-pair? :two-pair
            pair? :pair
            :else :high-card))))))

(= :high-card (__ ["HA" "D2" "H3" "C9" "DJ"]))

(= :pair (__ ["HA" "HQ" "SJ" "DA" "HT"]))

(= :two-pair (__ ["HA" "DA" "HQ" "SQ" "HT"]))

(= :three-of-a-kind (__ ["HA" "DA" "CA" "HJ" "HT"]))

(= :straight (__ ["HA" "DK" "HQ" "HJ" "HT"]))

(= :straight (__ ["HA" "H2" "S3" "D4" "C5"]))

(= :flush (__ ["HA" "HK" "H2" "H4" "HT"]))

(= :full-house (__ ["HA" "DA" "CA" "HJ" "DJ"]))

(= :four-of-a-kind (__ ["HA" "DA" "CA" "SA" "DJ"]))

(= :straight-flush (__ ["HA" "HK" "HQ" "HJ" "HT"]))
