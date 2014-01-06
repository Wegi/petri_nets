(ns petri-nets.core
  (:gen-class))

;; WARNING: Program uses state. Keep a distance of 20 complexity Units
;; at all time

(def net-db (atom {}))

(defn clear-db
  "Reset the net-database"
  []
  (reset! net-db {}))

(defn create-net
  "Create a new empty net 'name'"
  [name]
  (swap! net-db assoc name {:places {}
                            :transitions #{}
                            :edges_in #{}
                            :edges_out #{}}))

(defn add-place
  "Add a place 'pname' with tokens many tokens into 'netname'"
  [netname pname tokens]
  (let [newplaces (assoc (:places (@net-db netname)) pname tokens)
        newnet (assoc net :places newplaces)]
    (swap! net-db assoc netname newnet)))

(defn change-tokens
  "Change number of tokens in the place 'pname' in the net 'netname'. Syntactic Sugar"
  [netname pname tokens]
  (add-place netname pname tokens))

(defn add-transition
  "Add a transition 'tname' to the net 'netname'"
  [netname tname]
  (let [newtrans (conj (:transitions (netname @net-db)) tname)
        newnet (assoc (netname @net-db) :transitions newtrans)]
    (swap! net-db assoc netname newnet)))

(defn placenames
  "Get a list of the placenames in 'net'"
  [net]
  (map (fn [[placename x]] placename) (:places (net @net-db))))

(defn trans
  "Easy access sugar to get transitions from a net"
  [net]
  (:transitions (net @net-db)))

(defn add-edge
  "Add a new edge. Does nothing if target or origin are invalid."
  [net from to tokens]
  (if (and (from (trans net)) (some #{to} (placenames net)))
    (swap! net-db assoc
           net (merge-with clojure.set/union (@net-db net) {:edges_out #{[from to tokens]}}))
    (if (and (some #{from} (placenames net)) (to (trans net)))
      (swap! net-db assoc
             net (merge-with clojure.set/union (@net-db net) {:edges_in #{[from to tokens]}})))))

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
@net-db
(add-place :test :gi 24)
(add-transition :test :t2)
(add-edge :test :t2 :wgi 5)
(add-edge :test :wgi :t2 7)
(save-net :test "out.txt")
(load-net "out.txt" :test)
(save-all "out.txt")
(load-all "out.txt")
