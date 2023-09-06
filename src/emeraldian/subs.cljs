(ns emeraldian.subs
  (:require
   [re-frame.core :as re-frame]))

(re-frame/reg-sub
 ::prompt-inputs
 (fn [db]
   (:prompt-inputs db)))

(re-frame/reg-sub
 ::used-CP
 (fn [db]
   (:used-CP db)))