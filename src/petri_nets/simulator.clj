(ns petri-nets.simulator
  (:require [petri-nets.api :as api] :reload))

(def simstate (atom {:current :default}))
;; Which net is currently selected?
;; {:current netname}

(defn current
  "Return the currently selected net."
  [] (@simstate :current))

(defn netnames
  "Return the names of all current existing nets."
  [] (api/netnames))

(defn all-places
  "Return all places and their tokens of a net."
  [net]
  (((api/database) net) :places))

(defn transitions
  "Return all transitions."
  [net]
  (api/transitions net))

(defn in-edges
  [net]
  (api/in-edges net))

(defn out-edges
  [net]
  (api/out-edges net))

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
  ([copy] (copy-net (current) copy))
  ([template copy]
     (api/copy-net template copy)))

(defn merge-nets
  "Merge two nets, equivmap is a map of places and transitions that are equivalent in both nets."
  [net1 net2 equivmap newnet]
  (api/merge-nets net1 net2 equivmap newnet))

(defn add-place
  "Add a place to the currently selected net."
  ([place tokens] (add-place (current) place tokens))
  ([net place tokens]
     (api/add-place net place tokens)))

(defn add-transition
  "Add a transition to the currently selected net."
  ([transition] (add-transition (current transition)))
  ([net transition]
     (api/add-transition net transition)))

(defn remove-net
  "Remove a net."
  [net]
  (api/remove-net net))

(defn remove-tokens
  "Remove tokens from a place in the currently selected net."
  ([place x-tokens] (remove-tokens (current) place x-tokens))
  ([net place x-tokens]
     (let [cur (api/tokens-in net place)]
       (api/change-tokens net place (- cur x-tokens)))))

(defn tokens-in
  "Return number of tokens in place."
  ([place] (tokens-in (current) place))
  ([net place]
     (api/tokens-in net place)))

(defn add-tokens
  "Add tokens to a place in the currently selected net."
  ([place x-tokens] (add-tokens (current) place x-tokens))
  ([net place x-tokens]
     (let [cur (api/tokens-in net place)]
       (api/change-tokens net place (+ cur x-tokens)))))

(defn add-edge
  "Add a new Edge to the currently selected net."
  ([from to tokens] (add-edge (current) from to tokens))
  ([net from to tokens]
     (api/add-edge net from to tokens)))

(defn save-net
  "Save selected net to file."
  ([file] (save-net (current) file))
  ([net file]
     (api/save-net net file)))

(defn save-all
  "Save all nets into a Single file. (Has to be loaded wih load-all)."
  [file]
  (api/save-all file))

(defn load-net
  "Load a single net from a file. The Saved net can not have the same name
as an allready existing net."
  [file net]
  (api/load-net file net))

(defn load-all
  "Load multiple nets from a single file. The Current nets will be overwritten."
  [file]
  (api/load-all file))

(defn delete-net
  "Delete the selected net and set the selection to :default."
  ([] (delete-net (current)))
  ([net]
     (api/remove-net net)
     (swap! simstate assoc :current :default)))

(defn fireable?
  "Check whether a transition in the selected net can be fired."
  ([trans] (fireable? (current) trans))
  ([net trans]
     (let [in-edges (api/in-edges net)
           filtered (get in-edges trans)
           mapped (map (fn [[place token]] (>= (- (tokens-in net place) token) 0)) filtered)]
       (or (empty? filtered) (and (not (some false? mapped)) (not (empty? mapped)))))))

(defn show-fireable!
  "Show all fireable transitions in a net. Without parameters the net is the currently selected."
  ([]
     (show-fireable! (current)))
  ([net]
     {net (filter (partial fireable? net) (api/transitions net))}))

(defn fire
  "Execute a transition in the selected net. If The Execution is not possible
return nil. If no trans parameter is given a random fireable transition
is executed."
  ([net]
     (let [fireable (get (show-fireable! net) net)]
       (if-not (empty? fireable)
         (fire net (rand-nth fireable)))))
  ([net trans]
     (if (fireable? net trans)
       (let [ins (get (api/in-edges net) trans)
             outs (get (api/out-edges net) trans)]
         (doall (map (fn [[p t]] (remove-tokens net p t)) ins))
         (doall (map (fn [[p t]] (add-tokens net p t)) outs)))
       nil)))

(defn all-fireable!
  "Show all nets and their fireable transitions, organized in a map."
  []
  (apply merge (map show-fireable! (api/netnames))))

(defn netalive
  "Check whether the net has any fireable transition."
  [net]
  (some true? (map (partial fireable? net) (api/transitions net))))

(defn add-netalive
  "Add the netalive property to the current net."
  ([] (add-netalive (current)))
  ([net]
     (api/add-property net (str "(" 'petri-nets.simulator/netalive " " \" net \" ")" ))))

(defn transitionalive
  "Checks if at least one of the in args specified transitions can be fired."
  [net args]
  (some true? (map (partial fireable? net) args)))

(defn nonempty
  "Check if at least one of the places is not empty."
  [net args]
  (let [tokensmap (map #(api/tokens-in net %) args)
        nonzeros (map zero? tokensmap)]
    (some false? nonzeros)))

(defn add-transitionalive
  "Add the transitionalive property to the current net. args has to be a sequence."
  ([args] (add-transitionalive (current) args))
  ([net args]
     (if (clojure.set/subset? (set args) (api/transitions net))
       (let [new-args (clojure.string/replace (str args) #"[()]" "")]
         (api/add-property net (str "(" 'petri-nets.simulator/transitionalive " " \" net \" " [" new-args "])"))))))

(defn add-nonempty
  "Add the nonempty property to the current net. args has to be a sequence."
  ([args] (add-nonempty (current) args))
  ([net args]
     (if (clojure.set/subset? (set args) (set (api/placenames net)))
       (let [new-args (clojure.string/replace (str (doall args)) #"[()]" "")]
         (api/add-property net (str "(" 'petri-nets.simulator/nonempty " " \" net \" " [" new-args "])"))))))

(defn add-property
  "Add a custom property to the net. property has to be a String which describes
a Vector with the property-funcion calls."
  ([property]
     (add-property (current) (str property)))
  ([net property]
     (api/add-property net (str property))))

(defn properties
  "Show all properties of a net."
  [net]
  (api/properties net))
