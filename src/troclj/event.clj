(ns troclj.event)


(def ^:dynamic *event-bus* nil)

(defn publish [aggregateId & args]
  (println "event-published" aggregateId "->" args))

