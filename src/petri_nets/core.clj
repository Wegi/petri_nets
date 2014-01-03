(ns petri-nets.core
  (:gen-class))

;; WARNING: Program uses state. Keep a distance of 20 complexity Units
;; at all time

(def net-db (atom {}))

(defn clear-db []
  "Reset the net-database"
  (reset! net-db {}))

(defn create-net [name]
  "Create a new empty net 'name'"
  (def empty-net {:places {}
                  :transitions #{}
                  :edges_in #{}
                  :edges_out #{}})
  (swap! net-db assoc name empty-net))

(defn add-place [netname pname tokens]
  "Add a place 'pname' with tokens many tokens into 'netname'"
  (def net (netname @net-db))
  (def newplaces (assoc (:places net) pname tokens))
  (def newnet (assoc net :places newplaces))
  (swap! net-db assoc netname newnet))

(defn change-tokens [netname pname tokens]
  "Change number of tokens in the place 'pname' in the net 'netname'"
  (add-place netname pname tokens)) ;same operation as add-place as places are unique

(defn add-transition [netname tname]
  "Add a transition 'tname' to the net 'netname'"
  (def newtrans (conj (:transitions (netname @net-db)) tname))
  (def newnet (assoc (netname @net-db) :transitions newtrans))
  (swap! net-db assoc netname newnet))

(defn placenames [net]
  "Get a list of the placenames in 'net'"
  (map (fn [[placename x]] placename) (:places (net @net-db))))

(defn trans [net]
  "Easy access sugar to get transitions from a net"
  (:transitions (net @net-db)))

(defn add-edge [net from to tokens]
  "Add a new edge. Does nothing if target or origin are invalid."
  (if (and (from (trans net)) (some #{to} (placenames net)))
    (do
      (def newnet (assoc (net @net-db) :edges_out (conj (:edges_out (net @net-db)) (vector from to tokens))))
      (swap! net-db assoc net newnet))
    (if (and (some #{from} (placenames net)) (to (trans net)))
      (do
        (def newnet (assoc (net @net-db) :edges_in (conj (:edges_in (net @net-db)) (vector from to tokens))))
        (swap! net-db assoc net newnet)))))

(defn save-net [net file]
  "Save 'net' to 'file'"
  (spit file (net @net-db)))

(defn save-all [file]
  "Saves all nets to file"
  (spit file @net-db))

(defn load-net [file net]
  "Load the net from 'file' and save it in the net-databse as 'net'"
  (swap! net-db assoc net (load-string (slurp file))))

(defn load-all [file]
  "Loads all nets contained in 'file' and replaces the net-database with them"
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
