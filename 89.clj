;; Graph Tour
;; Difficulty:	Hard
;; Topics:	graph-theory

;; Starting with a graph you must write a function that returns true if it is possible to make a tour of the graph in which every edge is visited exactly once.
;; The graph is represented by a vector of tuples, where each tuple represents a single edge.
;; The rules are:

;; - You can start at any node.
;; - You must visit each edge exactly once.
;; - All edges are undirected.

(def __
  (fn [g]
    (let [graph (into #{} (map (fn [i edge] [i (sort edge)]) (map vector (range (count g))) g))]
      (boolean
       (some
        (fn [starting-vertex]
          ((fn step [visited
                     not-visited
                     current-vertex]
             (if (= graph (set visited))
               true
               (let [moves (filter (fn [[i [a b]]]
                                     (or (= a current-vertex) (= b current-vertex))) not-visited)]
                 (boolean
                  (some #(step (conj visited %) (disj not-visited %) (let [edge (second %)
                                                                           start (first edge)
                                                                           end (second edge)]
                                                                       (if (= start current-vertex)
                                                                         end
                                                                         start))) moves)))))
           [] graph starting-vertex)) (apply concat (map second graph)))))))


(= true (__ [[:a :b]]))
(= false (__ [[:a :a] [:b :b]]))
(= true (__ [[1 2] [2 3] [3 4] [4 1]]))
(= true (__ [[:a :b] [:a :c] [:c :b] [:a :e]
             [:b :e] [:a :d] [:b :d] [:c :e]
             [:d :e] [:c :f] [:d :f]]))

;; Huh, many identical edges?
(= false (__ [[:a :b] [:a :b] [:a :c] [:c :a]
               [:a :d] [:b :d] [:c :d]]))

(= false (__ [[1 2] [2 3] [2 4] [2 5]]))
