(ns petri-nets.core
  (:gen-class))

;; WARNING: Program uses state. Keep a distance of 20 complexity Units
;; at all time

(def net-db (atom {}))
; Database for nets
(def namerels (atom {}))
; Relations for pretty-prining names

(defn clear-db
  "Reset the net-database."
  []
  (reset! net-db {}))

(defn create-net
  "Create a new empty net 'name'. If no name is supplied a unique name is generated."
  ([]
     (let [unique (str (hash (gensym)))]
       (create-net unique) unique))
  ([name]
     (swap! net-db assoc name {:places {}
                               :transitions #{}
                               :edges-in {}
                               :edges-out {}})))

(defn copy-net
  "Lets you instantiate(copy) a net. If no name for the copy is given,
a random one is generated."
  ([net]
     (let [unique (str (hash (gensym)))]
       (copy-net net unique) unique))
  ([net copy]
     (swap! net-db assoc copy (@net-db net))))

(defn merge-nets
  "Merge two nets into one. 'equiv' is a map which contains equivalent
places/transitions which are merged into one node."
  [net1 net2 equiv newnet]
  (let [merged-places (merge-places net1 net2 equivp)
        merged-trans (merge-trans net1 net2 equivt)
        merged-ins (merge-edgeins net1 net2 equivp equivt)
        merged-outs (merge-edgeouts net1 net2 equivp equivt)]
    (swap! net-db assoc newnet {:places merged-places
                                :transitions merged-trans
                                :edges-in merged-ins
                                :egdes-out merged-outs})))

(defn prefix-places
  "Return places in net prefixed with netname except those in except.
 except is expected to be a hashmap where the keys get ignored."
  ([net]
     (prefix-places net {}))
  ([net except]
     (prefix-x net (placenames net) except :places)))

(defn prefix-transitions
  "Return the transitions in net prefixed with netname except those on except.
except is expected to be a hashmap where the keys get ignored."
  ([net]
     (prefix-transitions net {}))
  ([net except]
     (prefix-x net (trans net) except :transitions)))

(defn prefix-place-trans-edges
  "Return the in-edges in net prefixed with netname except those corresponding to
keys in except."
  [net except]
  (prefix-x net (concat (placenames net) (trans net)) except :edges-in))

(defn prefix-trans-place-edges
  "Return the out-edges in net prefixed with netname exxcept those corresponding to
keys in except."
  [net except]
  (prefix-x net (concat (placenames net) (trans net)) except :edges-out))

(defn prefix-x
  "Prefix Everything in the what entry of net which is in unfiltered - except."
  [net unfiltered except what]
  (let [filtered (filter #(not (contains? except %)) unfiltered)
        name-map (into {} (map #(vector % (str net ":" %)) filtered))]
    (clojure.walk/prewalk-replace name-map ((@net-db net) what))))



(prefix-x :test (placenames :test) {:weg :a} :places)

(prefix-place-trans-edge :test {:we :foo} {}) ;;TODO filter
;;transitions key funktioniert nicht
;; 2 kanten von einem place broken
;; namen der trans sammeln  (wie prefix-trans, maybe one function)
;; renamen
;; gg
;; Alle rename funktionen auf einmal <- eine Methode nur :places etc.
;; übergeben  prewalk-replace



(defn rename-places
  "Return the places of net renamed according to equivmap."
  [net equivmap]
  (clojure.set/rename-keys ((@net-db net) :places) equivmap))

(defn rename-transitions
  "Return the transitions of net renamed according to equivmap."
  [net equivmap]
  (clojure.walk/prewalk-replace equivmap (trans net)))

(defn add-place
  "Add a place 'pname' with tokens many tokens into 'netname'."
  [netname pname tokens]
  (swap! net-db assoc-in [netname :places pname] tokens))

(defn change-tokens
  "Change number of tokens in the place 'pname' in the net 'netname'. Syntactic sugar."
  [netname pname tokens]
  (add-place netname pname tokens))

(defn add-transition
  "Add a transitions 'tname' to the net."
  [netname tname]
  (swap! net-db update-in [netname :transitions] #(clojure.set/union % #{tname})))

(defn placenames
  "Get a list of the placenames in 'net'."
  [net]
  (keys (:places (net @net-db))))

(defn trans
  "Easy access sugar to get transitions from a net."
  [net]
  ((@net-db net) :transitions))

(defn add-trans-place-edge
  "Add a new edge from a transition to a place."
  [net t place tokens]
  (swap! net-db assoc-in [net :edges-out t place] tokens))

(defn add-place-trans-edge
  "Add a new edge from a place to a transition."
  [net place t tokens]
  (swap! net-db assoc-in [net :edges-in place t] tokens))

(defn save-net
  "Save 'net' to 'file'"
  [net file]
  (spit file (net @net-db)))

(defn save-all
  "Saves all nets to file"
  [file]
  (spit file @net-db))

(defn load-net
  "Load the net from 'file' and save it in the net-databse as 'net'"
  [file net]
  (swap! net-db assoc net (load-string (slurp file))))

(defn load-all
  "Loads the net database contained in 'file' and replaces the net-database with it"
  [file]
  (reset! net-db (load-string (slurp file))))


;; Manual testing Area
(clear-db)
(create-net :test)
(create-net)
@net-db
(add-place :test :wegi 24)
(merge-places :test :test2 {:a 2})
(change-tokens :test :meter 13)
(add-transition :test :t)
(add-trans-place-edge :test :t3 :wg 7)
(add-place-trans-edge :test :wegi :t3 7)
(save-net :test "out.txt")
(load-net "out.txt" :test)
(save-all "out.txt")
(load-all "out.txt")
