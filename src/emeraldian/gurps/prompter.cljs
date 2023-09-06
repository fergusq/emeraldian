(ns emeraldian.gurps.prompter
  (:require [emeraldian.gurps.templates :as templates]
            [emeraldian.gurps.calculator :as c]))

(defrecord Prompt [message options])

(def prompt-input [])
(def path [])

(defn prompt [options & {:keys [message formatter]
                         :or {message "Select one of:"
                              formatter str}}]
  (prn options prompt-input)
  (let [options (vec options)]
    (if (seq prompt-input)
      (let [choice (first prompt-input)]
        (set! prompt-input (rest prompt-input))
        (get options choice))
      (throw (->Prompt message (for [i (range 0 (count options))]
                                 {:index i
                                  :option (get options i)
                                  :text (formatter (get options i))}))))))

(defn random-prompter [options & o]
  (rand-nth options))

(def ^:dynamic *prompter* prompt)

(defn prompt-optional-CP [options]
  (*prompter* options :formatter (constantly nil)))

(defn assoc-if-exists [to key from]
  (if (contains? from key)
    (assoc to key (get from key))
    to))

(defn prompt-template [templates]
  (set! path (conj path templates))
  (let [template (*prompter* (conj templates {:name "None"})
                             :formatter :name)
        choice (reduce (fn [c key] (assoc c key (*prompter* (get-in template [:optional key]))))
                       template
                       (keys (:optional template)))
        used-CP (reduce #(merge-with + %1 (:used-CP (prompt-optional-CP (map (fn [o] {:used-CP o}) %2))))
                        (get choice :used-CP {})
                        (:optional-CP choice))
        character {:used-CP used-CP}]
    (-> character
        (assoc-if-exists :species choice)
        (assoc-if-exists :background choice)
        (assoc-if-exists :profession choice)
        (assoc-if-exists :notes choice)
        (as-> c (if (empty? (:lens choice))
                  c
                  (c/merge-characters c (prompt-template (:lens choice))))))))


(defn prompt-character [& {:keys [prompter input] :or {prompter prompt
                                                       input []}}]
  (set! prompt-input input)
  (set! path [])
  (try
    (binding [*prompter* prompter]
      (let [species (prompt-template (vals templates/species))
            background (prompt-template (vals templates/backgrounds))
            profession (prompt-template (vals templates/professions))
            character (c/merge-characters
                       species
                       background
                       profession)]
        {:prompt nil
         :character character
         :path path}))
    (catch Prompt p
      {:prompt p
       :character nil
       :path path})))