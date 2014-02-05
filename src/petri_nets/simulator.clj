(ns petri-nets.simulator
  (:require [petri-nets.api :as api] :reload))

(def simstate (atom {:current :default}))
;; Which net is currently selected?
;; {:current netname}

(defn create-net
  "Create a new empty net."
  [netname]
  (api/create-net netname))

(defn select-net
  "Set a net as the currently selected one. If the net doesn't exist nil is returned."
  [net]
  (if (some #{net} (api/netnames))
    (swap! simstate assoc :current net)
    nil))

(defn create-select
  "Create a new net and select it as the current net."
  [netname]
  (api/create-net netname)
  (select-net netname))

(defn reset
  "Reset the whole simulation and the net-database."
  [] (api/reset-db))

(defn copy-net
  "Copy a net. Usefull for instanciacion."
  [template copy]
  (api/copy-net template copy))

(defn merge-nets
  "Merge two nets, equivmap is a map of places and transitions that are equivalent in both nets."
  [net1 net2 equivmap newnet]
  (api/merge-nets net1 net2 equivmap newnet))

(defn current
  "Return the currently selected net."
  [] (@simstate :current))

(defn add-place
  "Add a place to the currently selected net."
  [place tokens]
  (api/add-place (current) place tokens))

(defn add-transition
  "Add a transition to the currently selected net."
  [transition]
  (api/add-transition (current) transition))

(defn remove-tokens
  "Remove tokens from a place in the currently selected net."
  [place x-tokens]
  (let [cur (api/tokens-in (current) place)]
    (api/change-tokens (current) place (- cur x-tokens))))

(defn tokens-in
  "Return number of tokens in place."
  [place]
  (api/tokens-in (current) place))

(defn add-tokens
  "Add tokens to a place in the currently selected net."
  [place x-tokens]
  (let [cur (api/tokens-in (current) place)]
    (api/change-tokens (current) place (+ cur x-tokens))))

(defn add-edge
  "Add a new Edge to the currently selected net."
  [from to tokens]
  (api/add-edge (current) from to tokens))

(defn save-net
  "Save selected net to file."
  [file]
  (api/save-net (current) file))

(defn save-all
  "Save all nets into a Single file. (Has to be loaded wih load-all)."
  [file]
  (api/save-all file))

(defn load-net
  "Load a single net from a file. The Saved net can not have the same name
as an allready existing net."
  [file]
  (api/load-net file))

(defn load-all
  "Load multiple nets from a single file. The Current nets will be overwritten."
  [file]
  (api/load-all file))

(defn delete-net
  "Delete the selected net and set the selection to :default."
  []
  (api/remove-net (current))
  (swap! simstate assoc :current :default))

(defn fireable
  "Check whether a transition in the selected net can be fired."
  [trans]
  (let [in-edges (api/in-edges (current))
        filtered (get in-edges trans)
        mapped (map (fn [[place token]] (>= (- (tokens-in place) token) 0)) filtered)]
    (not (some false? mapped))))

(defn fire
  "Execute a transition in the selected net. If The Execution is not possible
return nil."
  [trans]
  (if (fireable trans)
    (let [ins (get (api/in-edges (current)) trans)
          outs (get (api/out-edges (current)) trans)]
      (map (fn [[p t]] (remove-tokens p t)) ins)
      (map (fn [[p t]] (add-tokens p t)) outs)
      )
    nil))
