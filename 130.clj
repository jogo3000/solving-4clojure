;; Tree reparenting
;; Difficulty:	Hard
;; Topics:	tree

;; Every node of a tree is connected to each of its children as well as its parent. One can imagine grabbing one node of a tree and dragging it up to the root position, leaving all connections intact. For example, below on the left is a binary tree. By pulling the "c" node up to the root, we obtain the tree on the right.
;; <image on website>

;; Note it is no longer binary as "c" had three connections total -- two children and one parent. Each node is represented as a vector, which always has at least one element giving the name of the node as a symbol. Subsequent items in the vector represent the children of the node. Because the children are ordered it's important that the tree you return keeps the children of each node in order and that the old parent node, if any, is appended on the right. Your function will be given two args -- the name of the node that should become the new root, and the tree to transform.

(defn spy [id t]
  (println id t ) t)

(def __
  (fn [node t]
    (letfn [(rephrase [node]
              (when (coll? node)
                (let [parent (first node)
                      children (rest node)
                      rels (vec (for [child children] [parent (first child)]))]
                  (into rels
                        (mapcat rephrase children)))))]
      (if (= node (first t)) t
          (let [relations (rephrase t)
                parent->children (->> (map (fn [[p c]] {p [c]}) relations)
                                      (apply merge-with concat))
                child->parent (->> (map (fn [[p c]] [c p]) relations)
                                   (into {}))]
            (letfn [(down [node]
                      (let [children (parent->children node)]
                        (if-not children (list node)
                                (cons node (map down children)))))
                    (not-me [parent node]
                      (filter #(if-not (coll? %) true
                                       (not= (first %) node)) parent))
                    (up [node]
                      (let [parent (child->parent node)]
                        (when parent
                          (let [parent-tree (up parent)]
                            (concat (not-me (down parent) node)
                                    (when parent-tree (list parent-tree)))))))]
              (let [my-children (down node)
                    my-parent (up node)]
                (cons node (concat (rest my-children) (list my-parent))))
              ))))))


(__ 't '(z (t (e) (a)) (u)))
;; => {t [a e], z [t]}
;; => (t (a) (e))


(__ 'a '(t (e) (a)))
;; => {e t, a t}

(__ 'c '(a
             (b
               (c
                 (d)
                 (e))
               (f
                 (g)
                 (h)))
             (i
               (j
                 (k)
                 (l))
               (m
                 (n)
                 (o)))))
;; => (c (d) (e) (b (f (g) (h)) (a (i (j (k) (l)) (m (n) (o))))))

;; => (c (d) (e) b (f (g) (h)) (a (i (j (k) (l)) (m (n) (o)))))
;; => (c ((d) (e) (b (f (g) (h)) a (i (j (k) (l)) (m (n) (o))))))
;; ;; (c (d) (e) (b (f (g) (h)) (a (i (j (k) (l)) (m (n) (o))))))

;; => (c ((d) (e) (b (f (g) (h)) a (i (j (k) (l)) (m (n) (o))))))
;; => (c (d) (e) (b (f (g) (h))))
;; => (c (d) (e) nil)


(= '(n)
   (__ 'n '(n)))

(= '(a (t (e)))
   (__ 'a '(t (e) (a))))

(= '(e (t (a)))
   (__ 'e '(a (t (e)))))

(= '(a (b (c)))
   (__ 'a '(c (b (a)))))

(= '(d
      (b
        (c)
        (e)
        (a
          (f
            (g)
            (h)))))
  (__ 'd '(a
            (b
              (c)
              (d)
              (e))
            (f
              (g)
              (h)))))

(= '(c
      (d)
      (e)
      (b
        (f
          (g)
          (h))
        (a
          (i
          (j
            (k)
            (l))
          (m
            (n)
            (o))))))
   (__ 'c '(a
             (b
               (c
                 (d)
                 (e))
               (f
                 (g)
                 (h)))
             (i
               (j
                 (k)
                 (l))
               (m
                 (n)
                 (o))))))
;; => false
