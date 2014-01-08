(ns petri-nets.core
  (:gen-class))

;; WARNING: Program uses state. Keep a distance of 20 complexity Units
;; at all time

(def net-db (atom {}))

(defn clear-db
  "Reset the net-database."
  []
  (reset! net-db {}))

(defn create-net
  "Create a new empty net 'name'. If no name is supplied a unique name is generated."
  ([]
     (create-net (hash (gensym))))
  ([name]
     (swap! net-db assoc name {:places {}
                               :transitions #{}
                               :edges-in #{}
                               :edges-out #{}})))

(defn copy-net
  "Lets you instantiate(copy) a net. If no name for the copy is given,
a random one is generated."
  ([net]
     (copy-net net (hash (gensym))))
  ([net copy]
     (swap! net-db assoc copy (@net-db net))))

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
  (:transitions (net @net-db)))

(defn add-trans-place-edge
  "Add a new edge from a transition to a place."
  [net t place tokens]
  (swap! net-db update-in [net :edges-out] #(clojure.set/union % #{[t place tokens]})))

(defn add-place-trans-edge
  "Add a new edge from a place to a transition."
  [net place t tokens]
  (swap! net-db update-in [net :edges-in] #(clojure.set/union % #{[place t tokens]})))

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
(add-place :test :wgi 24)
(change-tokens :test :meter 13)
(add-transition :test :t2)
(add-trans-place-edge :test :t2 :wgis 5)
(add-place-trans-edge :test :wegi :t2 7)
(save-net :test "out.txt")
(load-net "out.txt" :test)
(save-all "out.txt")
(load-all "out.txt")
