(ns emeraldian.views
  (:require
   [clojure.string :as str]
   [re-frame.core :as re-frame]
   [emeraldian.subs :as subs]
   [emeraldian.events :as events]
   [emeraldian.gurps.prompter :as prompter]
   [emeraldian.gurps.calculator :as c]
   ))

(defn change-cp-buttons [key dec inc]
  [:div.btn-group
    [:button.btn.btn-danger
     {:type "button"
      :on-click #(re-frame.core/dispatch [::events/add-CP key dec])}
     (str dec)]
   (when (not= dec -1)
     [:button.btn.btn-outline-primary
      {:type "button"
       :on-click #(re-frame.core/dispatch [::events/add-CP key -1])}
      "-1"])
   (when (not= inc 1)
     [:button.btn.btn-outline-primary
      {:type "button"
       :on-click #(re-frame.core/dispatch [::events/add-CP key +1])}
      "+1"])
   [:button.btn.btn-success
    {:type "button"
      :on-click #(re-frame.core/dispatch [::events/add-CP key inc])}
    (str "+" inc)]])

#_(defn add-skill-modal [id]
  [:div.modal.fade {:id id
                    :tabindex "-1"
                    :aria-hidden "true"
                    :aria-labeledby (str id "heading")}
   [:div.modal-dialog
    [:div.modal-content
     [:div.modal-header
      [:h1.modal-title.fs-5 {:id (str id "heading")}
       "Add Skill, Spell, Advantage, or Disadvantage"]
      [:button.btn-close {:type "button"
                          :data-bs-dismiss "modal"
                          :aria-label "Close"}]]
     [:div.modal-content
      ]
     [:div.modal-footer
      [:button.btn.btn-secondary {:type "button"
                                  :data-bs-dismiss "modal"}
       "Close"]
      [:button.btn.btn-primary {:type "button"
                                :on-click #(prn "ADD")}
       "Add"]]]]])

(defn render-character [context-map]
  [:div.character
   (let [total-cp (:TotalCP context-map)
         unused-cp (:UnusedCP context-map)
         cp-percent (-> (/ total-cp
                           (+ total-cp
                              unused-cp))
                        (* 100)
                        (Math/floor))]
     [:div.d-flex.flex-row.my-4
      [:div.me-2 [:strong "TOTAL CP: " total-cp]]
      [:div.progress {:role "progressbar"
                      :aria-valuenow (str cp-percent)
                      :style {:flex "1"}}
       [(if (< unused-cp 0)
          :div.progress-bar.bg-danger
          :div.progress-bar)
        {:style {:width (str cp-percent "%")}}]]
      [:div.ms-2 [:strong "UNUSED CP: " unused-cp]]])
   [:div
    [:h2 "Character Info"]
    [:table.table
     [:tbody
      [:tr
       [:th "Species"]
       [:td (:species context-map)]]
      [:tr
       [:th "Background"]
       [:td (:background context-map)]]
      [:tr
       [:th "Profession"]
       [:td (:profession context-map)]]
      (when (seq (:notes context-map))
        [:tr
         [:th "Notes"]
         [:td>ul
          (for [note (:notes context-map)]
            [:li note])]])]]]
   #_(add-skill-modal "add-skill-modal")
   #_[:p
      [:button.btn.btn-primary {:type "button"
                                :data-bs-toggle "modal"
                                :data-bs-target "#add-skill-modal"}
       "Add Skill, Spell, Advantage, or Disadvantage"]]
   [:div.row
    [:div.attributes.col
     [:h2 "Attributes"]
     [:table.table
      [:thead
       [:tr
        [:th "Attribute"]
        [:th "Value"]
        [:th "Used CP"]
        [:th "Actions"]]]
      [:tbody
       [:tr
        [:td "ST"]
        [:td (:ST context-map)]
        [:td (:STCP context-map)]
        [:td (change-cp-buttons :ST -10 +10)]]
       [:tr
        [:td "DX"]
        [:td (:DX context-map)]
        [:td (:DXCP context-map)]
        [:td (change-cp-buttons :DX -20 +20)]]
       [:tr
        [:td "IQ"]
        [:td (:IQ context-map)]
        [:td (:IQCP context-map)]
        [:td (change-cp-buttons :IQ -20 +20)]]
       [:tr
        [:td "HT"]
        [:td (:HT context-map)]
        [:td (:HTCP context-map)]
        [:td (change-cp-buttons :HT -10 +10)]]
       [:tr
        [:td "Per"]
        [:td (:Per context-map)]
        [:td (:PerCP context-map)]
        [:td (change-cp-buttons :Per -5 +5)]]
       [:tr
        [:td "Will"]
        [:td (:Will context-map)]
        [:td (:WillCP context-map)]
        [:td (change-cp-buttons :Will -5 +5)]]
       [:tr
        [:td "Max HP"]
        [:td (:MaxHP context-map)]
        [:td (:MaxHPCP context-map)]
        [:td (change-cp-buttons :MaxHP -2 +2)]]
       [:tr
        [:td "Max FP"]
        [:td (:MaxFP context-map)]
        [:td (:MaxFPCP context-map)]
        [:td (change-cp-buttons :MaxFP -3 +3)]]
       [:tr
        [:td "Initiative"]
        [:td (:Initiative context-map)]
        [:td (:InitiativeCP context-map)]
        [:td (change-cp-buttons :Initiative -2 +2)]]
       [:tr
        [:td "Thrust"]
        [:td (:Thrust context-map)]
        [:td ""]
        [:td ""]]
       [:tr
        [:td "Swing"]
        [:td (:Swing context-map)]
        [:td ""]
        [:td ""]]]]]
    [:div.advantages.col
     [:h2 "Advantages"]
     [:table.table
      [:thead
       [:tr
        [:th "Name"]
        [:th "Value"]
        [:th "Used CP"]
        [:th "Actions"]]]
      [:tbody
       (for [key (-> context-map :advantages keys)]
         (let [advantage (-> context-map :advantages (get key))
               points-per-level (get advantage :points-per-level 1)]
           (prn advantage)
           ^{:key key}
           [:tr
            [:td (if (= (:CP advantage) 0)
                   [:s (:name advantage)]
                   (:name advantage))]
            [:td (:value advantage)]
            [:td (:CP advantage)]
            [:td (change-cp-buttons (:key advantage) (- points-per-level) points-per-level)]]))
       [:tr
        [:td {:rowSpan 4}
         [:select.form-select {:aria-label "Select new Advantage or Disadvantage"
                               :on-change #(re-frame.core/dispatch [::events/add-CP (keyword (.. % -target -value)) 1])}
          (for [[key advantage] (sort-by first c/advantages)]
            ^{:key key}
            [:option {:value (name key)}
             (-> advantage :name)])]]]]]]]
   [:div.skills
    [:h2 "Skills"]
    [:table.table
     [:thead
      [:tr
       [:th "Name"]
       [:th "Base"]
       [:th "Value"]
       [:th "Used CP"]
       [:th "Actions"]]]
     [:tbody
      (for [key (-> context-map :skills keys)]
        (let [skill (-> context-map :skills (get key))]
          (prn skill)
          ^{:key key}
          [:tr
           [:td (if (= (:CP skill) 0)
                  [:s (:name skill)]
                  (:name skill))]
           [:td (:difficulty skill)]
           [:td (:value skill)]
           [:td (:CP skill)]
           [:td (change-cp-buttons (:key skill) -1 +1)
            [:input {:type "checkbox"
                     :style {:marginLeft "1rem"}}]]]))
      [:tr
       [:td {:rowSpan 5}
        [:select.form-select {:aria-label "Select new Skill"
                              :on-change #(re-frame.core/dispatch [::events/add-CP (keyword (.. % -target -value)) 1])}
         (for [[key skill] (sort-by first c/skills)]
           ^{:key key}
           [:option {:value (name key)}
            (-> skill :name)])]]]]]]
   [:div.spells
    [:h2 "Spells"]
    [:table.table
     [:thead
      [:tr
       [:th "Name"]
       [:th "Base"]
       [:th "Value"]
       [:th "Used CP"]
       [:th "Actions"]]]
     [:tbody
      (for [key (-> context-map :spells keys)]
        (let [skill (-> context-map :spells (get key))]
          (prn skill)
          ^{:key key}
          [:tr
           [:td (if (= (:CP skill) 0)
                  [:s (:name skill)]
                  (:name skill))]
           [:td (:difficulty skill)]
           [:td (:value skill)]
           [:td (:CP skill)]
           [:td (change-cp-buttons (:key skill) -1 +1)]]))
      [:tr
       [:td {:rowSpan 5}
        [:select.form-select {:aria-label "Select new Spell"
                              :on-change #(re-frame.core/dispatch [::events/add-CP (keyword (.. % -target -value)) 1])}
         (for [[key spell] (sort-by first c/spells)]
           ^{:key key}
           [:option {:value (name key)}
            (-> spell :name)])]]]]]]])

(defn render-template [template]
  [:div
   (when (:desc template)
     (for [pg (str/split (:desc template) "\n\n")]
       [:p.fst-italic pg]))
   (if (empty? (:used-CP template))
     "Uses no CP"
     [:table.w-100
      [:tr
       [:th.w-50 "Name"]
       [:th.w-50 "Used CP"]]
      (for [key (keys (:used-CP template))]
        ^{:key key}
        [:tr
         [:td.w-50
          (c/render-name key)]
         [:td.w-50
          (get-in template [:used-CP key])]])])])

(defn option-button [option]
  ^{:key (:index option)}
  [:a.list-group-item.list-group-item-action
   {:href "#"
    :on-click #(re-frame.core/dispatch [::events/template-choose (:index option)])}
   [:h3 (:text option)]
   (render-template (:option option))])

(defn main-panel []
  (let [prompt-inputs (re-frame/subscribe [::subs/prompt-inputs])
        prompt (prompter/prompt-character :input @prompt-inputs)
        used-CP (re-frame/subscribe [::subs/used-CP])]
    (prn "USED-CP" @used-CP)
    [:div.body.container
     [:h1
      "Emeraldian"]
     ;[:div.template-list
     ; (for [phase (:path prompt)
     ;       template phase]
     ;   (render-template template))]
     (if (nil? (:prompt prompt))
       [:div
        [:textarea
         {:value (-> prompt :character
                     (c/merge-characters {:used-CP @used-CP})
                     str)
          :readOnly true}]
        (render-character (-> prompt :character
                              (c/merge-characters {:used-CP @used-CP})
                              (c/create-context-map 171)))]
       [:div
        [:button.btn.btn-outline-primary
         {:type "button"
          :on-click #(re-frame.core/dispatch [::events/template-back])}
         "Back"]
        [:p
         (-> prompt :prompt :message)]
        [:div.list-group.list-group-flush
         (for [option (-> prompt :prompt :options)]
           (option-button option))]
        ])]))