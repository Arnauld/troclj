(ns troclj.core-test
  (:require [clojure.test :refer :all]
            [troclj.core :refer :all])
  (:use [troclj.samples :as samples]))

(deftest a-test
  (let [commodities samples/predef-commodities
        clearingHouse (new-clearing-house commodities)
        farmer samples/farmer-agent
        blacksmith samples/blacksmith-agent
        ]
    (println "commodities" commodities)
    (println "clearingHouse" clearingHouse)
    (with-commodities commodities
                      (with-clearing-house clearingHouse
                                           (perform-production farmer)
                                           (generate-offers farmer)
                                           (perform-production blacksmith)
                                           (generate-offers blacksmith)))))
