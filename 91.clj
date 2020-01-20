;; Graph Connectivity
;; Difficulty:	Hard
;; Topics:	graph-theory

;; Given a graph, determine whether the graph is connected. A connected graph is such that a path exists between any two given nodes.
;; -Your function must return true if the graph is connected and false otherwise.
;; -You will be given a set of tuples representing the edges of a graph. Each member of a tuple being a vertex/node in the graph.
;; -Each edge is undirected (can be traversed either direction).

;; (= true (__ #{[:a :a]}))

;; (= true (__ #{[:a :b]}))

;; (= false (__ #{[1 2] [2 3] [3 1]
;;                [4 5] [5 6] [6 4]}))

;; (= true (__ #{[1 2] [2 3] [3 1]
;;               [4 5] [5 6] [6 4] [3 4]}))

;; (= false (__ #{[:a :b] [:b :c] [:c :d]
;;                [:x :y] [:d :a] [:b :e]}))

;; (= true (__ #{[:a :b] [:b :c] [:c :d]
;;               [:x :y] [:d :a] [:b :e] [:x :a]}))


(def solution
  "It may not be pretty, but I figured out a really easy way to implement this. We take a random edge and assign it to
  the graph. Then we go through all of the other edges and see which of them can be fitted to the graph. Ones that can
  be added, are added to the pile of edges belonging to the graph. If the pile of edges that need to be fitted to the
  graph is getting smaller, continue to the next round. If not, we know the graph isn't connected."
  (fn [s]
    ;; Start with randomly selected edge as a seed
    (loop [found #{(first s)}
           pending (rest s)]
      (if-not (seq pending)
        ;; All edges found a place in the graph! Great!
        true
        ;; Find edges which match to any part of the graph
        (let [{matches true discards false}
              (group-by
               (fn [[a b]]
                 (boolean
                  (some (fn [[c d]] (or (= a c) (= a d) (= b c) (= b d))) found))) pending)]
          ;; If pile of discards is smaller, continue
          (if (< (count discards) (count pending))
            (recur (into found matches) discards)
            ;; It is not connected, because amount of edges is not shrinking
            false))))))


(comment

  (= true (solution #{[:a :b] [:b :c] [:c :d]
                      [:x :y] [:d :a] [:b :e] [:x :a]}))

  )
