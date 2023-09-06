(ns emeraldian.events
  (:require
   [re-frame.core :as re-frame]
   [emeraldian.db :as db]
   ))

(re-frame/reg-event-db
 ::initialize-db
 (fn [_ _]
   db/default-db))

(re-frame/reg-event-fx
 ::template-choose
 (fn [coeffects event]
   (let [option-id (second event)
         db (:db coeffects)]
     {:db (update-in db [:prompt-inputs] #(conj % option-id))})))

(re-frame/reg-event-fx
 ::template-back
 (fn [coeffects event]
   (let [db (:db coeffects)]
     {:db (update-in db [:prompt-inputs] #(pop %))})))

(re-frame/reg-event-fx
 ::add-CP
 (fn [coeffects event]
   (let [property (get event 1)
         cp (get event 2)
         db (:db coeffects)]
     {:db (update-in db [:used-CP property] #(+ % cp))})))