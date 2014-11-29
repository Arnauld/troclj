(ns troclj.core
  (:import (java.util.concurrent.atomic AtomicLong))
  (:use [troclj.event :as event]))


;   ___                                    _ _ _
;  / __\___  _ __ ___  _ __ ___   ___   __| (_) |_ _   _
; / /  / _ \| '_ ` _ \| '_ ` _ \ / _ \ / _` | | __| | | |
;/ /__| (_) | | | | | | | | | | | (_) | (_| | | |_| |_| |
;\____/\___/|_| |_| |_|_| |_| |_|\___/ \__,_|_|\__|\__, |
;                                                  |___/

(defn to-map [keyFn values]
  (reduce (fn [acc v]
            (assoc acc (keyFn v) v)) {} values))

(def ^:dynamic *commodities* nil)

(defmacro with-commodities [commodities & body]
  `(binding [*commodities* (to-map :commodityId ~commodities)]
     ~@body))


(defn new-commodity [commodityId size]
  {:commodityId commodityId
   :size        size})

(defn commodity-size-of [commodityId]
  (get-in *commodities* [commodityId :size]))


;  _____                      _
;  \_   \_ ____   _____ _ __ | |_ ___  _ __ _   _
;   / /\/ '_ \ \ / / _ \ '_ \| __/ _ \| '__| | | |
;/\/ /_ | | | \ V /  __/ | | | || (_) | |  | |_| |
;\____/ |_| |_|\_/ \___|_| |_|\__\___/|_|   \__, |
;                                           |___/

(defn new-inventory [maxSize content ideal]
  {:max-size maxSize
   :content  content
   :ideal    ideal})

(defn inventory-free-space [inventory]
  (let [content (:content inventory)
        size (:max-size inventory)]
    (reduce (fn [acc [commodityId amount]]
              (- acc (* amount (commodity-size-of commodityId)))) size content)))

;   _                    _
;  /_\   __ _  ___ _ __ | |_
; //_\\ / _` |/ _ \ '_ \| __|
;/  _  \ (_| |  __/ | | | |_
;\_/ \_/\__, |\___|_| |_|\__|
;       |___/

(def agent-id-gen (AtomicLong.))
(defn- next-agent-id [] (.incrementAndGet agent-id-gen))

(defn new-agent [typeId
                 inventory
                 production-fn]
  {:id                     (next-agent-id)
   :lookback               15
   :typeId                 typeId
   :inventory              inventory
   :price-beliefs          {}
   :observed-trading-range {}
   :production             production-fn})

(defn inventory-of [agent]
  (:inventory agent))

(defn price-belief [agent commodityId]
  (get-in agent [:price-beliefs commodityId] [0.5 1.5]))    ; todo initial price belief...

(defn agent-commodity-amount [agent commodityId]
  (get-in agent [:inventory :content commodityId] 0))

(defn update-agent-stock [agent commodityId amount]
  (update-in agent [:inventory :content commodityId] (fnil + 0) amount))

(defn agent-has? [agent commodityId amount]
  (<= amount (agent-commodity-amount agent commodityId)))

(defn agent-has-not? [agent commodityId amount]
  (not (agent-has? agent commodityId amount)))


(defmulti apply-on-agent (fn [_agent event] (:what event)))

(defmethod apply-on-agent :default [_agent event]
  (throw (IllegalArgumentException.
           (str "Unsupported event " event))))


(defn perform-production
  ([agent]
   (perform-production agent (fn [agent event] (event/publish (:id agent) event))))
  ([agent eventDispatchFn]
   (let [productionFn (:production agent)]
     (reduce (fn [agent event]
               (eventDispatchFn agent event)
               (apply-on-agent agent event)) agent (productionFn agent)))))



;      _                 _               _
;  ___| | ___  __ _ _ __(_)_ __   __ _  | |__   ___  _   _ ___  ___
; / __| |/ _ \/ _` | '__| | '_ \ / _` | | '_ \ / _ \| | | / __|/ _ \
;| (__| |  __/ (_| | |  | | | | | (_| | | | | | (_) | |_| \__ \  __/
; \___|_|\___|\__,_|_|  |_|_| |_|\__, | |_| |_|\___/ \__,_|___/\___|
;                                |___/

(def ^:dynamic *clearing-house* nil)

(defn clearing-house []
  (if (nil? @*clearing-house*) (throw (IllegalStateException. "No clearing house bound!"))
                               @*clearing-house*))

(defmacro with-clearing-house [clearing-house & body]
  `(binding [*clearing-house* (atom ~clearing-house)]
     ~@body
     ))

(defn new-clearing-house [commodities]
  {:commodities commodities
   :historical  {}})


(defn historical-prices-of [commodityId]
  (get-in (clearing-house) [:historical commodityId] []))


; _     _     _    __        _
;| |__ (_) __| |  / /_ _ ___| | __
;| '_ \| |/ _` | / / _` / __| |/ /
;| |_) | | (_| |/ / (_| \__ \   <
;|_.__/|_|\__,_/_/ \__,_|___/_|\_\



; bid: buyer
; ask: seller

(defn average [values]
  (let [[total n] (reduce (fn [[sum n] v]
                            [(+ sum v) (inc n)]) [0 0] values)]
    (if (< 0 n)
      (/ total n)
      0                                                     ; todo default value
      )))

(defn min-max [values]
  (reduce (fn [[min max] v]
            (if (nil? min)
              [v v]
              [(min min v) (max max v)])) [nil nil] values))

(defn favorability [val min max]
  (if (= min max)
    0.5
    (let [dval (/ (- val min) (- max min))]
      (cond (< dval 0) 0
            (> dval 1) 1
            :else dval))))

(defn price-of [agent commodityId]
  (let [priceBelief (price-belief agent commodityId)
        [min max] priceBelief]
    (+ min (* (rand) (- max min)))))

(defn determine-sale-quantity [agent commodityId]
  (let [historicalPrices (historical-prices-of commodityId)
        mean (average (take-last (:lookback agent) historicalPrices))
        observedRanges (get-in agent [:observed-trading-range commodityId] [1]) ; todo initial observed trading range
        [omin omax] (min-max observedRanges)]
    (if (nil? omin)                                         ; no range
      0
      (let [favorability (favorability mean omin omax)
            inventory (inventory-of agent)
            idealVn (get-in inventory [:ideal commodityId])
            actualVn (get-in inventory [:content commodityId])
            surplus (max 0 (- actualVn idealVn))
            amountToSell (Math/round (* favorability surplus))]
        (max 1 amountToSell)))))

(defn new-ask [agent commodityId limit]
  (let [bidPrice (price-of agent commodityId)
        quantity (determine-sale-quantity agent commodityId)]
    {:what     :sell
     :from     (:id agent)
     :price    bidPrice
     :quantity (max quantity limit)}))

(defn determine-purchase-quantity [agent commodityId]
  (let [historicalPrices (historical-prices-of commodityId)
        mean (average (take-last (:lookback agent) historicalPrices))
        observedRanges (get-in agent [:observed-trading-range commodityId] [1]) ; todo initial observed trading range
        [omin omax] (min-max observedRanges)]
    (if (nil? omin)                                         ; no range
      0
      (let [favorability (- 1 (favorability mean omin omax))
            inventory (inventory-of agent)
            idealVn (get-in inventory [:ideal commodityId])
            actualVn (get-in inventory [:content commodityId])
            shortage (max 0 (- idealVn actualVn))
            amountToBuy (Math/round (* favorability shortage))]
        (max 1 amountToBuy)))))

(defn new-bid [agent commodityId limit]
  (let [bidPrice (price-of agent commodityId)
        quantity (determine-purchase-quantity agent commodityId)]
    {:what     :buy
     :from     (:id agent)
     :price    bidPrice
     :quantity (min quantity limit)}))

(defn generate-offer [agent commodityId]
  (let [inventory (inventory-of agent)
        idealVn (get-in inventory [:ideal commodityId])
        actualVn (get-in inventory [:content commodityId])
        surplus (- actualVn idealVn)
        shortage (- idealVn actualVn)
        unitSize (commodity-size-of commodityId)
        freeSpace (inventory-free-space inventory)
        amountPossible (min shortage (int (/ freeSpace unitSize)))]
    (cond
      ; -- surplus
      (< 0 surplus)
      (new-ask agent commodityId surplus)
      ; -- shortage, but enough space?
      (and (< 0 shortage) (< 0 amountPossible))
      (new-bid agent commodityId amountPossible)
      ; -- no order... nil? :s
      :else
      nil)
    ))

(defn generate-offers [agent]
  (reduce (fn [acc [commodityId _expectedVn]]
            (let [offer (generate-offer agent commodityId)]
              (if (nil? offer)
                acc
                (conj acc offer)))) [] (get-in agent [:inventory :ideal])))

