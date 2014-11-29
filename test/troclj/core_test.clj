(ns troclj.core-test
  (:require [clojure.test :refer :all]
            [troclj.core :refer :all])
  (:use [troclj.samples :as samples]))

(defn no-logic [& _]
  [])

(deftest update-agent-stock-0
  (let [farmer0 (new-agent :farmer
                           (new-inventory 10
                                          {:food 1 :tools 0 :money 100}
                                          {:food 3 :tools 2})
                           no-logic)
        farmer1 (update-agent-stock farmer0 :ore 7)]
    (is (= 0 (agent-commodity-amount farmer0 :ore)))
    (is (= 7 (agent-commodity-amount farmer1 :ore)))))

(deftest a-test
  (let [commodities samples/predef-commodities
        clearingHouse (new-clearing-house commodities)
        farmer (samples/new-farmer-agent)
        blacksmith (samples/new-blacksmith-agent)]
    (println "commodities" commodities)
    (println "clearingHouse" clearingHouse)
    (with-commodities commodities
                      (with-clearing-house clearingHouse
                                           (perform-production farmer)
                                           (generate-offers farmer)
                                           (perform-production blacksmith)
                                           (generate-offers blacksmith)))))
