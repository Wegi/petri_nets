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


;; Manual testing Area
(clear-db)
(create-net :test)
@net-db
(add-place :test :wegi 24)
(add-transition :test :t2)
