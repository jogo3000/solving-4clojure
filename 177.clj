;; Balancing Brackets

;; Difficulty:	Medium
;; Topics:	parsing


;; When parsing a snippet of code it's often a good idea to do a sanity check to see if all the brackets match up. Write a function that takes in a string and returns truthy if all square [ ] round ( ) and curly { } brackets are properly paired and legally nested, or returns falsey otherwise.

(def __
  (fn [s]
    (letfn [(safepop [stack]
              (if (empty? stack) stack
                  (pop stack)))]
      (loop [stack []
             [c & cs] s
             ok? true]
        (if-not c (and ok? (empty? stack))
                (case c
                  \( (recur (conj stack c) cs ok?)
                  \{ (recur (conj stack c) cs ok?)
                  \[ (recur (conj stack c) cs ok?)
                  \) (recur (safepop stack) cs (and ok? (= \( (last stack))))
                  \] (recur (safepop stack) cs (and ok? (= \[ (last stack))))
                  \} (recur (safepop stack) cs (and ok? (= \{ (last stack))))
                  (recur stack cs ok?))
                )))))

(__ "This string has no brackets.")

(__ "class Test {
      public static void main(String[] args) {
        System.out.println(\"Hello world.\");
      }
    }")

(not (__ "(start, end]"))

(not (__ "())"))

(not (__ "[ { ] } "))

(__ "([]([(()){()}(()(()))(([[]]({}()))())]((((()()))))))")

(not (__ "([]([(()){()}(()(()))(([[]]({}([)))())]((((()()))))))"))

(not (__ "["))
