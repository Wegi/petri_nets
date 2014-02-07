(ns petri-nets.core
  (:gen-class))

;; WARNING: Program uses state. Keep a distance of 20 complexity Units
;; at all time

(def net-db (atom {}))
; Database for nets

(defn clear-db
  "Reset the net-database."
  []
  (reset! net-db {}))

(defn create-net
  "Create a new empty net 'name'. If no name is supplied a unique name is generated."
  ([]
     (let [unique (str (hash (gensym)))]
       (create-net unique)))
  ([name]
     (swap! net-db assoc name {:places {}
                               :transitions #{}
                               :edges-in {}
                               :edges-out {}
                               :props #{}})))

(defn copy-net
  "Lets you instantiate(copy) a net. If no name for the copy is given,
a random one is generated. Names are still unique, because the tuple (Netname, Place/transition)
is unique."
  ([net]
     (let [unique (str (hash (gensym)))]
       (copy-net net unique)))
  ([net copy]
     (let [renamed (clojure.walk/prewalk-replace {net copy} ((@net-db net) :props))
           copynet (assoc-in @net-db [net :props] renamed)]
       (swap! net-db assoc copy (copynet net)))))

(defn placenames
  "Get a list of the placenames in 'net'."
  [net]
  (keys (:places (@net-db net))))

(defn trans
  "Easy access sugar to get transitions from a net."
  [net]
  ((@net-db net) :transitions))

(defn prefix-net
  "Prefix all places and transitions in the whole net."
  [net except]
  (let [unfiltered (clojure.set/union (trans net) (placenames net))
        filtered (filter #(not (contains? except %)) unfiltered)
        name-map (into {} (map #(vector % (str net "<>" %)) filtered))]
    (clojure.walk/prewalk-replace name-map (@net-db net))))

(defn prefix-x
  "Prefix Everything in the what entry of net which is in unfiltered - except."
  [net unfiltered except what]
  (let [filtered (filter #(not (contains? except %)) unfiltered)
        name-map (into {} (map #(vector % (str net "<>" %)) filtered))]
    (clojure.walk/prewalk-replace name-map ((@net-db net) what))))

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

(defn rename-places
  "Return the places of net renamed according to equivmap."
  [net equivmap]
  (clojure.set/rename-keys ((@net-db net) :places) equivmap))

(defn rename-transitions
  "Return the transitions of net renamed according to equivmap."
  [net equivmap]
  (clojure.walk/prewalk-replace equivmap (trans net)))

(defn rename-place-trans-edges
  "Return the in-edges renamed according to equivmap"
  [net equivmap]
  (clojure.walk/prewalk-replace equivmap ((@net-db net) :edges-in)))

(defn rename-trans-place-edges
  "Return the out-edges renamed according to equivmap"
  [net equivmap]
  (clojure.walk/prewalk-replace equivmap ((@net-db net) :edges-out)))

(defn rename-all
  "Rename all places and transitions in structure according to equivmap."
  [structure equivmap]
  (clojure.walk/prewalk-replace equivmap structure))

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

(defn add-trans-place-edge
  "Add a new edge from a transition to a place."
  [net t place tokens]
  (swap! net-db assoc-in [net :edges-out t place] tokens))

(defn add-place-trans-edge
  "Add a new edge from a place to a transition. The transition is the key."
  [net place t tokens]
  (swap! net-db assoc-in [net :edges-in t place] tokens))

(defn merge-places
  "Merge two nodes into one. Use with maps."
  [node1 node2]
  (merge-with max node1 node2))

(defn merge-trans
  "Merge two nodes into one. Use with sets."
  [node1 node2]
  (clojure.set/union node1 node2))

(defn deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level.
  (deepmerge + {:a {:b {:c 1 :d {:x 1 :y 2}} :e 3} :f 4}
               {:a {:b {:c 2 :d {:z 9} :z 3} :e 100}})
  -> {:a {:b {:z 3, :c 3, :d {:z 9, :x 1, :y 2}}, :e 103}, :f 4}
Taken from apparently not to 1.3 ported clojure.contriv.map-utils"
  [f & maps]
  (apply
    (fn m [& maps]
      (if (every? map? maps)
        (apply merge-with m maps)
        (apply f maps)))
    maps))

(defn merge-properties
  "Merge two properties together."
  [net1 net2 newnetname]
  (let [newset1 (set (clojure.walk/prewalk-replace {net1 newnetname} ((@net-db net1) :props)))
        newset2 (set (clojure.walk/prewalk-replace {net2 newnetname} ((@net-db net2) :props)))]
    (clojure.set/union newset1 newset2)))

(defn merge-nets
  "Merge net1 and net2 into a net called netname. If no name is given a unique symbol is chosen.
equiv is a map of equivalent transitions and places, that have to be merged. The key is from net1
and the value from net2."
  ([net1 net2 equiv netname]
     (let [prefixed (prefix-net net1 equiv)
           renamed (rename-all prefixed equiv)
           merged-places (merge-places (renamed :places) ((@net-db net2) :places))
           merged-trans (merge-trans (renamed :transitions) ((@net-db net2) :transitions))
           merged-ins (deep-merge-with max (renamed :edges-in) ((@net-db net2) :edges-in))
           merged-outs (deep-merge-with max (renamed :edges-out) ((@net-db net2) :edges-out))
           merged-props (concat ((@net-db net1) :properties) ((@net-db net2) :properties))
           merged-properties (merge-properties net1 net2 netname)]
       (swap! net-db assoc netname {:places merged-places
                                    :transitions merged-trans
                                    :edges-in merged-ins
                                    :edges-out merged-outs
                                    :props merged-properties}))))

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

(defn netnames
  "Return all keys (netnames) currently present in the database."
  []
  (keys @net-db))

(defn remove-net
  "Remove a net permanently."
  [net]
  (swap! net-db dissoc net))

(defn tokens-in
  "Returns number of tokens in a specified place."
  [net place]
  (get-in (@net-db net) [:places place]))

;; ####### Here work on properties beginns
(defn properties
  "Return the properties of a net."
  [net]
  ((@net-db net) :props))

(defn add-property
  "Add a property to the net. Op is the property function with the arguments args."
  [net property]
  (swap! net-db update-in [net :props] #(clojure.set/union % #{property})))
