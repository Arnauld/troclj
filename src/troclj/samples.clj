(ns troclj.samples
  (:use troclj.core))

(defn not-implemented [] (throw (UnsupportedOperationException.)))

(defn produce
  ([commodityId amount]
   (produce commodityId amount 1.0))
  ([commodityId amount probabilityOfSuccess]
   (let [probability (rand)]
     {:what                 :produce
      :commodityId          commodityId
      :amount               amount
      :probabilityOfSuccess probabilityOfSuccess
      :probability          probability
      :success              (< probability probabilityOfSuccess)})))

(defmethod apply-on-agent :produce [agent params]
  (if (:success params)
    (update-agent-stock agent (:commodityId params) (:amount params))
    agent))

(defn consume
  ([commodityId amount]
   (consume commodityId amount 1.0))
  ([commodityId amount probabilityOfSuccess]
   (let [probability (rand)]
     {:what                 :consume
      :commodityId          commodityId
      :amount               (* -1 amount)
      :probabilityOfSuccess probabilityOfSuccess
      :probability          probability
      :success              (< probability probabilityOfSuccess)})))

(defmethod apply-on-agent :consume [agent params]
  (if (:success params)
    (update-agent-stock agent (:commodityId params) (:amount params))
    agent))

(defn transform
  [commodityIdFrom commodityIdTo amount efficiency]
  {:what            :transform
   :commodityIdFrom commodityIdFrom
   :commodityIdTo   commodityIdTo
   :amountFrom      (* -1 amount)
   :amountTo        (* efficiency amount)
   :efficiency      efficiency})

(defmethod apply-on-agent :transform [agent params]
  (-> agent
      (update-agent-stock (:commodityIdFrom params) (:amountFrom params))
      (update-agent-stock (:commodityIdTo params) (:amountTo params))))

(def start-conditions {:farmer     10
                       :miner      10
                       :woodcutter 10
                       :refiner    10
                       :blacksmith 10})

(def predef-commodities [(new-commodity :money 1.0)
                         (new-commodity :food 1.0)
                         (new-commodity :wood 1.0)
                         (new-commodity :ore 1.0)
                         (new-commodity :metal 1.0)
                         (new-commodity :tools 1.0)
                         (new-commodity :money 0.0)])

(defn farmer-logic [agent]
  (cond (and (agent-has? agent :wood 1) (agent-has? agent :tools 1))
        [(produce :food 4)
         (consume :wood 1)
         (consume :tools 1 0.1)]
        (and (agent-has? agent :wood 1) (agent-has-not? agent :tools 1))
        [(produce :food 2)
         (consume :wood 1)]
        :else [(consume :money 2)]))

(defn new-farmer-agent [] (new-agent :farmer
                                     (new-inventory 10
                                                    {:food 1 :tools 0 :wood 0 :money 100}
                                                    {:food 0 :tools 2 :wood 3})
                                     farmer-logic))

(defn miner-logic [agent]
  (cond (and (agent-has? agent :food 1) (agent-has? agent :tools 1))
        [(produce :ore 4)
         (consume :food 1)
         (consume :tools 1 0.1)]
        (and (agent-has? agent :food 1) (agent-has-not? agent :tools 1))
        [(produce :ore 2)
         (consume :food 1)]
        :else [(consume :money 2)]))


(defn new-miner-agent [] (new-agent :miner
                                    (new-inventory 10
                                                   {:food 1 :tools 0 :ore 0 :money 100}
                                                   {:food 3 :tools 2})
                                    miner-logic))

(defn refiner-logic [agent]
  (cond (and (agent-has? agent :food 1) (agent-has? agent :tools 1))
        [(transform :ore :metal (agent-commodity-amount agent :ore) 1.0)
         (consume :food 1)
         (consume :tools 1 0.1)]
        (and (agent-has? agent :food 1) (agent-has-not? agent :tools 1))
        [(transform :ore :metal 2 1.0)
         (consume :food 1)]
        :else [(consume :money 2)]))


(defn new-refiner-agent [] (new-agent :refiner
                                      (new-inventory 10
                                                     {:food 1 :tools 0 :metal 0 :ore 0 :money 100}
                                                     {:food 3 :tools 2 :ore 5})
                                      refiner-logic))

(defn woodcutter-logic [agent]
  (cond (and (agent-has? agent :food 1) (agent-has? agent :tools 1))
        [(produce :wood 2)
         (consume :food 1)
         (consume :tools 1 0.1)]
        (and (agent-has? agent :food 1) (agent-has-not? agent :tools 1))
        [(produce :wood 1)
         (consume :food 1)]
        :else [(consume :money 2)]))

(defn new-woodcutter-agent [] (new-agent :woodcutter
                                         (new-inventory 10
                                                        {:food 1 :tools 0 :wood 0 :money 100}
                                                        {:food 3 :tools 2 :wood 5})
                                         woodcutter-logic))

(defn blacksmith-logic [agent]
  (cond (agent-has? agent :food 1)
        [(transform :metal :tools (agent-commodity-amount agent :metal) 1.0)
         (consume :food 1)
         (consume :tools 1 0.1)]
        :else [(consume :money 2)]))

(defn new-blacksmith-agent [] (new-agent :blacksmith
                                         (new-inventory 10
                                                        {:food 1 :tools 0 :metal 0 :ore 0 :money 100}
                                                        {:food 3 :tools 2 :metal 5})
                                         blacksmith-logic))

