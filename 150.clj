;; Palindromic Numbers

;; Difficulty:	Medium
;; Topics:	seqs math

;; A palindromic number is a number that is the same when written forwards or backwards (e.g., 3, 99, 14341).
;; Write a function which takes an integer n, as its only argument, and returns an increasing lazy sequence of all palindromic numbers that are not less than n.
;; The most simple solution will exceed the time limit!

(comment "First instict is to generate palindromic numbers by taking single numbers and inserting them to the middle of a palindromic number
Have to take into consideration numbers with odd number of digits

'111' -> '1011' not palindromic '1101' is not either, but '1111' is
'121' is a similar problem, 1221 is palindromic though

'0' is a special case, you can't append anything to make it palindromic. Though itself is palindromic


To find the first palindromic number to return, I think a function to tell if number is palindromic is needed"


"Current problem seems to be that I can't pass from a five number 99999 to 100001. It goes to a seven number 1000001 instead. Don't exactly know why since all of the smaller ones seems to pass correctly."
         )

(def __
  (fn [n]
    (letfn [(digits [n]
              (letfn [(step [n]
                        (when (pos? n)
                          (cons (rem n 10) (step (quot n 10)))))]
                (if (zero? n) (list n) (reverse (step n)))))
            (palindrome? [n]
              (if (zero? n) true
                  (let [ds (digits n)]
                    (= ds (reverse ds)))))
            (number [digits]
              (loop [n 0
                     [d & ds] digits]
                (if-not d n
                        (recur (+ (* 10 n) d) ds))))
            (split-odd [digits digit-count]
              (let [half-point (quot digit-count 2)
                    first-half (take half-point digits)
                    middle-digit (->> (drop half-point digits) first)]
                (number (concat first-half (list middle-digit)))))
            (split-even [digits digit-count]
              (let [half-point (quot digit-count 2)
                    first-half (take half-point digits)]
                (number first-half)))
            (step [n]
              (cons n
                    (lazy-seq
                     (step
                      (let [digits-n (digits n)
                            digit-count (count digits-n)]
                        (number (cond
                                  (= 1 digit-count)
                                  (if (< n 9) (list (inc n))
                                      '(11))

                                  (odd? digit-count)
                                  (let [halved (split-odd digits-n digit-count)
                                        initial-digits (count (digits halved))
                                        next-number (->> halved
                                                         inc
                                                         digits)
                                        last-digit (last next-number)
                                        first-half (butlast next-number)
                                        new-digit-count (count next-number)]
                                    (if (> new-digit-count initial-digits)
                                      (concat first-half (reverse first-half))
                                      (concat first-half (list last-digit) (reverse first-half))))

                                  (even? digit-count)
                                  (let [next-number (->> (split-even digits-n digit-count)
                                                         inc
                                                         digits)
                                        new-digit-count (count next-number)]
                                    (if (> (* 2 new-digit-count) digit-count)
                                      (concat next-number (rest (reverse next-number)))
                                      (concat next-number (reverse next-number)))))))))))]
      (let [start (->> (iterate inc n)
                       (drop-while (complement palindrome?))
                       first)]
        (step start)))))

(take 2 (__ 99999))
(take 2 (__ 999))
(take 2 (__ 9999))


(require '[clojure.test :refer [deftest is testing run-tests]])

(deftest foreclojure-tsts

  (is (= (take 26 (__ 0))
           [0 1 2 3 4 5 6 7 8 9
            11 22 33 44 55 66 77 88 99
            101 111 121 131 141 151 161]))

  (is (= (take 16 (__ 162))
           [171 181 191 202
            212 222 232 242
            252 262 272 282
            292 303 313 323]))

  (is (= (take 6 (__ 1234550000))
           [1234554321 1234664321 1234774321
            1234884321 1234994321 1235005321]))

  (is (= (first (__ (* 111111111 111111111)))
           (* 111111111 111111111)))

  (is (= (set (take 199 (__ 0)))
           (set (map #(first (__ %)) (range 0 10000)))))

  (is (= true
           (apply < (take 6666 (__ 9999999)))))

  (is (= (nth (__ 0) 10101)
           9102019)))
