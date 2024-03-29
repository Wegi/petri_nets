(ns petri-nets.api
  (:require [petri-nets.core :as core] :reload))

;; All operations are performed on a Database. If you want to see it,
;; use get-database

(defn database
  "Display the current Database-State."
  []
  @core/net-db)

(defn create-net
  "Create a new net, if the name is emitted a unique name will be chosen.
If your chosen name is allready in use nil is returned."
  ([] (core/create-net))
  ([netname]
     (if (some #{netname} (core/netnames))
       nil
       (core/create-net netname))))

(defn reset-db
  "Clear the current database."
  [] (core/clear-db))

(defn copy-net
  "Instantiate a new net based on an existing net-template. If the name for the new
net is allready in use nil is returned."
  ([template]
     (core/copy-net template))
  ([template new-netname]
     (if (some #{new-netname} (core/netnames))
       nil
       (core/copy-net template new-netname))))

(defn merge-nets
  "Merge two nets into a third one. Equivalent-map is a map of places / transitions
which are equivalent in the new graph. If the name of the new net is allready in use
nil is returned."
  [net1 net2 equivalent-map newnet]
  (if (some #{newnet} (core/netnames))
    nil
    (core/merge-nets net1 net2 equivalent-map newnet)))

(defn name-free?
  "Checks whether the name is still free in corresponding net."
  [net name]
  (if-not (or (some #{name} (core/placenames net))
              (some #{name} (core/trans net))
              (= net name))
    true
    false))

(defn add-place
  "Add a new place with its tokens to a net. If the placename is allready
present in the net nil is returned."
  [net place tokens]
  (if (name-free? net place)
    (if (number? tokens) (core/add-place net place tokens))))

(defn add-transition
  "Add a new transition to a net. If the transitions name is allready
present in the net nil is returned."
  [net transition]
  (if (name-free? net transition)
    (core/add-transition net transition)))

(defn change-tokens
  "Change tokens in place to a new amount. Tokens has to be 0 or higher.
If Tokens is less than 0 nil is returned."
  [net place tokens]
  (if (>= tokens 0)
    (core/change-tokens net place tokens)
    nil))

(defn add-trans-place-edge
  "Add a new edge from a transition to a place. If some of the
input is incorrect (for example place is not a place), nil is returned.
If tokens is less than zero nil is returned also.
If this particular edge is allready part of the net only the token number is changed."
  [net trans place tokens]
  (if (and (some #{trans} (core/trans net))
           (some #{place} (core/placenames net))
           (>= tokens 0))
    (core/add-trans-place-edge net trans place tokens)
    nil))

(defn add-place-trans-edge
  "Add a new edge from a place to a transition. If some of the input is
incorrect (for example plac eis not a place), nil is returned.
If tokens is less than zero nil is returned also.
If this particular edge is allready part of the net only the token number is changed."
  [net place transition tokens]
  (if (and (some #{transition} (core/trans net))
           (some #{place} (core/placenames net))
           (>= tokens 0))
    (core/add-place-trans-edge net place transition tokens)
    nil))

(defn add-edge
  "Add a new edge to the net. If input is incorrect nil is returned."
  [net from to tokens]
  (if (number? tokens)
    (if (some #{from} (core/placenames net))
      (add-place-trans-edge net from to tokens)
      (add-trans-place-edge net from to tokens))))

(defn save-net
  "Save a single net to file."
  [net file]
  (core/save-net net file))

(defn save-all
  "Save the whole database to file."
  [file]
  (core/save-all file))

(defn load-net
  "Load a net from a file containing a single net. If the netname of
the loaded net is allready in use, return nil."
  [file net]
  (if (some #{net} (core/netnames))
    nil
    (core/load-net file net)))

(defn load-all
  "Load a database from a file. The current database is overwritten in the process."
  [file]
  (core/load-all file))

(defn remove-net
  "Remove a net permanently from the database."
  [net]
  (if (some #{net} (core/netnames))
    (core/remove-net net)))

(defn tokens-in
  "Return number of tokens in specified place. If place or net isn't instanciated nil is returned."
  [net place]
  (if (some #{place} (core/placenames net))
    (core/tokens-in net place)
    nil))

(defn netnames
  "Return all netnames currently present in the db."
  [] (core/netnames))

(defn transitions
  "Return all transitions of a net."
  [net]
  (core/trans net))

(defn in-edges
  "Return all place-trans edges."
  [net]
  ((@core/net-db net) :edges-in))

(defn out-edges
  "Return all trans-place edges."
  [net]
  ((@core/net-db net) :edges-out))

(defn add-property
  "Add a property to the net. Op is the property-function with its arguments args."
  [net property]
  (if (some #{net} (core/netnames))
    (core/add-property net property)))

(defn properties
  "Return all properties from net."
  [net]
  (if (some #{net} (core/netnames))
    (core/properties net)))

(defn placenames
  "Return all placenames of a net."
  [net]
  (if (some #{net} (core/netnames))
    (core/placenames net)))
