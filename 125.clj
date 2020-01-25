;; Gus' Quinundrum
;; Difficulty:	Hard
;; Topics:	logic fun brain-teaser

;; Create a function of no arguments which returns a string that is an exact copy of the function itself.
;; Hint: read this if you get stuck (this question is harder than it first appears); but it's worth the effort to solve it independently if you can!

;;Fun fact: Gus is the name of the 4Clojure dragon.

(comment "So I immediately realized this is basically a quine. I had read of quines a short while ago and remembered some basic recipes. Only problem with most of the classic LISP recipes is that they are essentially (f 'source code of f quoted') and 4clojure problem statement doesn't allow the answer function to have parameters. At least I think so. I fought with the pproblem for a while before caving in and porting the Java example from Wikipedia.

Let the scribblings below stand as a memorial to my impotent fumblings")

(
 (fn f [] (str (list f) (quote (fn f []))))
 )


(str '(fn f [] (str f (str 'quote f))))


((fn [x] (list x (list (quote quote) x))) (quote (fn [x] (list x (list (quote quote) x)))))

(str '((fn [x] (list x (list (quote quote) x))) (quote (fn [x] (list x (list (quote quote) x))))))

(#(str (str "(#" % " '" % ")")) '(str (str "(#" % " '" % ")")))

(str '#(str (str "(#" % " '" % ")")) '(str (str "(#" % " '" % ")")))

(= (str '__) (__))
(println
 (
  (fn [] (let [q (char 34) _ (char 32) s ["(fn [] (let [q (char 34) _ (char 32) s [" "]] (str (s 0) q (s 0) q _ q (s 1) q (s 1))))"]] (str (s 0) q (s 0) q _ q (s 1) q (s 1))))
  ))

;; public class Quine
;; {
;;   public static void main(String[] args)
;;   {
;;     char q = 34;      // Quotation mark character
;;     String[] l = {    // Array of source code
;;     "public class Quine",
;;     "{",
;;     "  public static void main(String[] args)",
;;     "  {",
;;     "    char q = 34;      // Quotation mark character",
;;     "    String[] l = {    // Array of source code",
;;     "    ",
;;     "    };",
;;     "    for(int i = 0; i < 6; i++)           // Print opening code",
;;     "        System.out.println(l[i]);",
;;     "    for(int i = 0; i < l.length; i++)    // Print string array",
;;     "        System.out.println(l[6] + q + l[i] + q + ',');",
;;     "    for(int i = 7; i < l.length; i++)    // Print this code",
;;     "        System.out.println(l[i]);",
;;     "  }",
;;     "}",
;;     };
;;     for(int i = 0; i < 6; i++)           // Print opening code
;;         System.out.println(l[i]);
;;     for(int i = 0; i < l.length; i++)    // Print string array
;;         System.out.println(l[6] + q + l[i] + q + ',');
;;     for(int i = 7; i < l.length; i++)    // Print this code
;;         System.out.println(l[i]);
;;   }
;; }
