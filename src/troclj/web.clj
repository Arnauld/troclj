(ns troclj.web
  (:use [compojure.handler :only [site]]                    ; form, query params decode; cookie; session, etc
        [compojure.core :only [defroutes GET POST DELETE ANY context]]
        [clojure.tools.logging :only [info]]
        org.httpkit.server)
  (:require [compojure.route :as route]
            [ring.util.response :as resp]))

; _                     _ _
;| |__   __ _ _ __   __| | | ___ _ __
;| '_ \ / _` | '_ \ / _` | |/ _ \ '__|
;| | | | (_| | | | | (_| | |  __/ |
;|_| |_|\__,_|_| |_|\__,_|_|\___|_|
;

(defn ws-handler [ring-request]
  ;; unified API for WebSocket and HTTP long polling/streaming
  (with-channel ring-request channel                        ; get the channel
                (if (websocket? channel)                    ; if you want to distinguish them
                  (on-receive channel (fn [data]            ; two way communication
                                        (send! channel data)))
                  (send! channel {:status  200
                                  :headers {"Content-Type" "text/plain"}
                                  :body    "Long polling?"}))))

(defn get-agent-by-id [req]                                 ;; ordinary clojure function
  (let [agent-id (-> req :params :id)                       ; param from uri
        ]
    ))

(defn update-agentinfo [req]                                ;; ordinary clojure function
  (let [agent-id (-> req :params :id)                       ; param from uri
        password (-> req :params :password)]                ; form param
    ))

;                 _
; _ __ ___  _   _| |_ ___  ___
;| '__/ _ \| | | | __/ _ \/ __|
;| | | (_) | |_| | ||  __/\__ \
;|_|  \___/ \__,_|\__\___||___/
;

(defroutes all-routes
           (GET "/" [] (resp/file-response "index.html" {:root "assets"}))
           ;(GET "/" [] show-landing-page)
           (GET "/ws" [] ws-handler)                        ;; websocket
           (context "/agent/:id" []
                    (GET / [] get-agent-by-id)
                    (POST / [] update-agentinfo))
           (route/files "/assets/" {:root "assets"})        ;; static file url prefix /assets, in `assets` folder
           (route/not-found "<p>Page not found.</p>"))      ;; all other, return 404

(defn start-server []
  ; Ring server
  (let [port 8080]
    (run-server (site #'all-routes) {:port port})
    (info (str "server started on http://127.0.0.1:" port ""))))

(defn -main [& args]
  (start-server))
