;; Tree reparenting
;; Difficulty:	Hard
;; Topics:	tree

;; Every node of a tree is connected to each of its children as well as its parent. One can imagine grabbing one node of a tree and dragging it up to the root position, leaving all connections intact. For example, below on the left is a binary tree. By pulling the "c" node up to the root, we obtain the tree on the right.
;; <image on website>

;; Note it is no longer binary as "c" had three connections total -- two children and one parent. Each node is represented as a vector, which always has at least one element giving the name of the node as a symbol. Subsequent items in the vector represent the children of the node. Because the children are ordered it's important that the tree you return keeps the children of each node in order and that the old parent node, if any, is appended on the right. Your function will be given two args -- the name of the node that should become the new root, and the tree to transform.

;; (= '(n)
;;    (__ 'n '(n)))

;; (= '(a (t (e)))
;;    (__ 'a '(t (e) (a))))

;; (= '(e (t (a)))
;;    (__ 'e '(a (t (e)))))

;; (= '(a (b (c)))
;;    (__ 'a '(c (b (a)))))

;; (= '(d
;;       (b
;;         (c)
;;         (e)
;;         (a
;;           (f
;;             (g)
;;             (h)))))

;;   (__ 'd '(a
;;             (b
;;               (c)
;;               (d)
;;               (e))
;;             (f
;;               (g)
;;               (h)))))

;; (= '(c
;;       (d)
;;       (e)
;;       (b
;;         (f
;;           (g)
;;           (h))
;;         (a
;;           (i
;;           (j
;;             (k)
;;             (l))
;;           (m
;;             (n)
;;             (o))))))

;;    (__ 'c '(a
;;              (b
;;                (c
;;                  (d)
;;                  (e))
;;                (f
;;                  (g)
;;                  (h)))
;;              (i
;;                (j
;;                  (k)
;;                  (l))
;;                (m
;;                  (n)
;;                  (o))))))


;; TODO: Code below makes child the new root but doesn't connect
;; the old parent correctly

;; Could be that this form of tree reparenting does not work properly in all cases. Think about cases where parent node needs to change into a child node, in cases when the tree is more complicated.

(def solution
  (fn [node t]
    (let [path ((fn search [path i [e & children]]
                  (if (= node e)
                    (conj path i)
                    (mapcat #(search (conj path i) % (nth children %)) (range (count children)))))[] 0 t)
          _ (println "found path" path)
          old-root ((fn pare [[e & children] [c & cs]]
                      (if-not (seq cs) (concat
                                        (list e)
                                        (take c children)
                                        (drop (inc c) children))
                              (concat (if (symbol? e)
                                        (list e) e)
                                      (take c children)
                                      (list (pare (nth children c) cs))
                                      (drop (inc c) children))))
                    t (rest path))
          _ (println "pared root" old-root)
          new-root (loop [[e & children] t
                          [c & cs] (rest path)]
                     (if-not
                         (seq cs) (nth children c)
                         (recur (nth children c) cs)))]

      (concat new-root (list old-root)))))

(comment
  (= '(a (t (e)))
     (solution 'a '(t (e) (a))))

(= '(d
      (b
        (c)
        (e)
        (a
          (f
            (g)
            (h)))))
  (solution 'd '(a
                 (b
                  (c)
                  (d)
                  (e))
                 (f
                  (g)
                  (h)))))


  )