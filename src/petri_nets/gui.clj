(ns petri-nets.gui
  (:gen-class)
  (:use [seesaw.core])
  (:use [seesaw.chooser])
  (:require [petri-nets.simulator :as simulator]))

;Visual Components
(def mainframe  (frame :title "Agar" :on-close :exit))
(def nets (listbox :model (simulator/netnames)))
(def netbox (vertical-panel :items [(label :text "All Nets")
                                    (scrollable nets)]))
(def places (listbox))
(def placebox (vertical-panel :items [(label :text "Places")
                                      (scrollable places)]))
(def properties (listbox))
(def propertybox (vertical-panel :items [(label :text "Properties")
                                         (scrollable properties)]))
(def transitions (listbox))
(def transbox (vertical-panel :items [(label :text "Transitions")
                                      (scrollable transitions)]))
(def in-edges (listbox))
(def in-edgebox (vertical-panel :items [(label :text "Trans<-Place Edges")
                                        (scrollable in-edges)]))
(def out-edges (listbox))
(def out-edgebox (vertical-panel :items [(label :text "Trans->Place Edges")
                                         (scrollable out-edges)]))
(def b-create-net (button :text "Create new Net"))
(def b-load-net (button :text "Load Net"))
(def b-load-all (button :text "Load All"))
(def b-save-net (button :text "Save Net"))
(def b-save-all (button :text "Save All"))
(def b-merge-nets (button :text "Merge Nets"))
(def b-copy-net (button :text "Copy Selected Net"))
(def b-remove-net (button :text "Remove Selected Net"))

(def b-add-place (button :text "Add Place"))
(def place-panel (vertical-panel :items [b-add-place]))

(def b-add-transition (button :text "Add Transition"))

(def b-add-pt-edge (button :text "Add Place->Trans Edge"))
(def pt-edge-panel (vertical-panel :items [b-add-pt-edge]))

(def b-add-tp-edge (button :text "Add Trans->Place Edge"))
(def tp-edge-panel (vertical-panel :items [b-add-tp-edge]))

(def b-add-netalive (button :text "Add Netalive"))
(def b-add-transalive (button :text "Add Transitionalive (Selected)"))
(def b-add-nonempty (button :text "Add Nonempty (Selected)"))

(def b-fire-selected (button :text "Fire Selected Transition"))
(def b-fire-random (button :text "Fire Random transition"))
(def b-fire-x (button :text "Fire x steps"))
(def transition-panel (vertical-panel :items [b-add-transition
                                              b-fire-selected b-fire-random b-fire-x]))

(def prop-indicator (listbox))
(def indicator-box (vertical-panel :items [(label "Property true or false")
                                           (scrollable prop-indicator)]))

(def b-not-selected (button :text "Negate Selected Property"))
(def b-or-selected (button :text "Or Selected Properties"))

;Functions
(defn selected-net
  [] (selection nets))

(defn run-properties
  [net]
  (let [props (simulator/properties net)
        eval-props (apply list (map #(str (or (eval (read-string %)) false) " | " %) props))]
    (config! prop-indicator :model eval-props)))

;Listener
(defn l-new-net
  [e]
  (let [name (input "Enter the name for new net.")]
       (simulator/create-net name))
  (config! nets :model (simulator/netnames)))

(defn l-update-boxes
  [e]
  (when-let [net (selected-net)]
    (config! places :model (simulator/all-places net))
    (config! transitions :model (simulator/transitions net))
    (config! in-edges :model (simulator/in-edges net))
    (config! out-edges :model (simulator/out-edges net))
    (config! properties :model (simulator/properties net))))

(defn l-add-place
  [e]
  (when-let [selected (selected-net)]
    (let [name (input "Enter a Placename.")
          tokens (read-string (input "Enter Initial Tokens"))]
      (simulator/add-place selected name tokens)
      (config! places :model (simulator/all-places selected)))))

(defn l-add-transition
  [e]
  (when-let [selected (selected-net)]
    (let [name (input "Enter a Name for the Transition.")]
      (simulator/add-transition selected name)
      (config! transitions :model (simulator/transitions selected)))))

(defn l-add-pt-edge
  [e]
  (when-let [selected (selected-net)]
    (when-let [place (first (selection places))]
      (when-let [t (selection transitions)]
        (let [cost (read-string (input "Enter a cost for the edge."))]
          (simulator/add-edge selected place t cost))))
    (config! in-edges :model (simulator/in-edges selected))))

(defn l-add-tp-edge
  [e]
  (when-let [selected (selected-net)]
    (when-let [place (first (selection places))]
      (when-let [t (selection transitions)]
        (let [cost (read-string (input "Enter a cost for the edge."))]
          (simulator/add-edge selected t place cost))))
    (config! out-edges :model (simulator/out-edges selected))))

(defn l-add-netalive
  [e]
  (when-let [selected (selected-net)]
    (simulator/add-netalive selected)
    (config! properties :model (simulator/properties selected))))

(defn l-add-transalive
  [e]
  (when-let [selected (selected-net)]
    (when-let [sel-trans (selection transitions {:multi? true})]
      (simulator/add-transitionalive selected sel-trans)
      (config! properties :model (simulator/properties selected)))))

(defn l-add-nonempty
  [e]
  (when-let [selected (selected-net)]
    (when-let [sel-trans (apply list (map #(first %) (selection places {:multi? true})))]
      (simulator/add-nonempty selected sel-trans)
      (config! properties :model (simulator/properties selected)))))

(defn l-not-selected
  [e]
  (when-let [selected (selected-net)]
    (when-let [prop (selection properties)]
      (simulator/add-property selected (str "(not " prop ")"))
      (config! properties :model (simulator/properties selected)))))

(defn l-or-selected
  [e]
  (when-let [selected (selected-net)]
    (when-let [s (selection properties {:multi? true})]
      (when-let [args (str (apply list (map read-string s)))]
        (when-let [res (subs args 1 (- (.length args) 1))]
          (simulator/add-property selected (str "(or " res ")"))
          (config! properties :model (simulator/properties selected)))))))

(defn l-load-net
  [e]
  (when-let [name (input "Enter a name for the loaded net")]
    (when-let [file (choose-file)]
      (simulator/load-net (.getPath file) name)
      (config! nets :model (simulator/netnames)))))

(defn l-load-all
  [e]
  (when-let [file (choose-file)]
    (simulator/load-all (.getPath file))
    (config! nets :model (simulator/netnames))))

(defn l-save-all
  [e]
  (when-let [file (choose-file :type :save)]
    (simulator/save-all (.getPath file))
    (config! nets :model (simulator/netnames))))

(defn l-save-net
  [e]
  (when-let [net (selected-net)]
    (when-let [file (choose-file :type :save)]
      (simulator/save-net net (.getPath file))
      (config! nets :model (simulator/netnames)))))

(defn l-fire-selected
  [e]
  (when-let [net (selected-net)]
    (when-let [trans (selection transitions)]
      (simulator/fire net trans)
      (l-update-boxes e)
      (run-properties net))))

(defn fire-random
  [net]
  (simulator/fire net)
  (l-update-boxes :foo)
  (run-properties net))

(defn l-fire-random
  [e]
  (when-let [net (selected-net)]
    (fire-random net)))

(defn l-fire-x
  [e]
  (when-let [net (selected-net)]
    (when-let [in (read-string (input "How many step should be fired?"))]
      (when (number? in)
        (dotimes [n in] (fire-random net))))))

(defn l-merge-nets
  [e]
  (when-let [first-net (first (selection nets {:multi? true}))]
    (when-let [second-net (second (selection nets {:multi? true}))]
      (when-let [newnet (input "Enter a name for the merged net")]
        (let [equivstr (input "Enter equivalent Places / Transitions")
              splitted-map (clojure.string/split equivstr #",")
              res (apply array-map splitted-map)]
          (simulator/merge-nets first-net second-net res newnet)
          (l-update-boxes e)
          (config! nets :model (simulator/netnames)))))))

(defn l-copy-net
  [e]
  (when-let [original (selection nets)]
    (when-let [net-copy (input "Enter a name for the Net-Copy")]
      (simulator/copy-net original net-copy)
      (config! nets :model (simulator/netnames)))))

(defn l-remove-net
  [e]
  (when-let [net (selected-net)]
    (simulator/remove-net net)
    (config! nets :model (simulator/netnames))
    (l-update-boxes e)))

;Main Area
(defn -main [& args]
  (native!)
  (listen b-create-net :action l-new-net)
  (listen nets :selection l-update-boxes)
  (listen b-add-place :action l-add-place)
  (listen b-add-transition :action l-add-transition)
  (listen b-add-pt-edge :action l-add-pt-edge)
  (listen b-add-tp-edge :action l-add-tp-edge)
  (listen b-add-netalive :action l-add-netalive)
  (listen b-add-transalive :action l-add-transalive)
  (listen b-add-nonempty :action l-add-nonempty)
  (listen b-not-selected :action l-not-selected)
  (listen b-or-selected :action l-or-selected)
  (listen b-load-net :action l-load-net)
  (listen b-save-net :action l-save-net)
  (listen b-save-all :action l-save-all)
  (listen b-load-all :action l-load-all)
  (listen b-fire-selected :action l-fire-selected)
  (listen b-fire-random :action l-fire-random)
  (listen b-merge-nets :action l-merge-nets)
  (listen b-copy-net :action l-copy-net)
  (listen b-remove-net :action l-remove-net)
  (listen b-fire-x :action l-fire-x)
  (let [merge-copy-buttons (vertical-panel  :border 15
                                            :items [b-merge-nets b-copy-net])
        load-save-buttons (vertical-panel :border 15
                                          :items [b-create-net b-load-net b-load-all
                                                  b-save-net b-save-all b-remove-net])
        prop-buttons (vertical-panel :border 15
                                     :items [b-add-netalive b-add-transalive b-add-nonempty
                                             b-not-selected b-or-selected])
        merge-instruction (text
                           :text "When merging nets, the first 2
selected Nets get merged.
When merging, a prompt will
ask you for the names
 of equivalent transitions and places.
Please enter them separated by comma.
 Example: \"a,b,foo,bar\" (no spaces).
Sets a equivalent to b and
foo to bar.
a and foo have to be from
the first net and b and
bar from the second." :multi-line? true :editable? false)
        layout (grid-panel :columns 4 :hgap 5 :vgap 3 :items
                         [netbox load-save-buttons merge-copy-buttons merge-instruction
                          placebox transbox in-edgebox out-edgebox
                          place-panel transition-panel pt-edge-panel tp-edge-panel
                          prop-buttons propertybox indicator-box])]
    (-> mainframe (config! :content layout) pack! show!))
  (alert "When adding/deleting things, make sure to select them/the right net."))
