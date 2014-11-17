(ns lab02.core
  (:gen-class)
  (:require [clj-http.client :as client])
  (:require [net.cgrand.enlive-html :as html])
  (:use [slingshot.slingshot :only [try+]]))

(defn create-node
  [id parent uris depth uri status children location]
  {:id id :parent parent :uris uris :depth depth :uri uri :status status :children children :location location}
)

(defn get-lines
  [file]
  (clojure.string/split-lines (slurp file))
)

(defn uuid
  []
  (str (java.util.UUID/randomUUID)))

(defn not-nil?
  [x]
  (not (nil? x)))

(defn get-indent
  [n]
  (apply str (repeat n " ")))

(defn get-status-message
  [node]
  (if (= (:status node) 404)
    " bad"
    (let [message (str " " (count (:uris node)) " link(s)")]
      (if (not-nil? (:location node))
        (str message " redirects " (:location node)))
      message
    ) #_let
  ) #_if
) #_fn

(defn print-formatted-line
  [node level]
  (let [indent (* 2 level)
        uri (:uri node)
        status-message (get-status-message node)]
    (println (str (get-indent indent) uri status-message))
    ) #_let
  ) #_fn

(defn remove-nils
  [col]
  (filter not-nil? col))

(defn get-sutable-lines
  [file]
  (remove-nils (get-lines file)))

(defn get-valid-urls
  [content]
  (->> (html/select content #{[:a]})
      (map #(:href (:attrs %1)))
      (remove-nils)))

(defn get-parent-id
  [parent]
  (if (nil? parent)
    nil
    (:id parent))
) #_fn

(defn fetch-url
  [url]
  (try+
    (client/get url {:throw-exceptions false})
    (catch Object _ {:status 404 :headers nil})
    ) #_try
  ) #_fn

(defn get-formatted-content
  [raw-content]
  (let [content-type (:content-type (:headers raw-content))]
    (if (and (not= 404 (:status raw-content)) (boolean (re-find #"text/html" content-type)))
      (html/html-snippet (:body raw-content))
      nil)
    )
  )

(defn get-location-header
  [raw-content]
  (let [status (:status raw-content)]
    (if (boolean (some #(= status %) '(300 301 302 303 307)))
      (:location (:headers raw-content)) #_then
      nil) #_else
    ) #_let
  )#_fn

(defn parse-specific-page
  [parent url depth]
  (println url)
  (let [raw-content (fetch-url url)
        status (:status raw-content)
        id (uuid)
        formatted-content (get-formatted-content raw-content)
        child (if (not-nil? formatted-content)
                (create-node id (get-parent-id parent) (get-valid-urls formatted-content) depth url status (atom []) (get-location-header raw-content))
                (create-node id (get-parent-id parent) '() depth url status (atom []) (get-location-header raw-content))) #_if
        ]
        (swap! (:children parent) conj child)
        child
    ) #_let
  ) #_fn

(defn create-root-node
  [urls depth]
  (create-node (uuid) nil urls depth "root" nil (atom []) nil))

(defn visit-node
  [node urls depth]
  (let [new-depth (dec depth)]
    (if (< depth 1)
      node
      (doseq [child (pmap #(parse-specific-page node %1 depth) urls)] (visit-node child (:uris child) new-depth))
    ) #_if
  ) #_let
)

(defn do-walk
  [node level]
  (print-formatted-line node level)
  (doseq [child @(:children node)] (do-walk child (inc level))))

(defn walk-tree
  [root]
  (do-walk root 0))

(defn do-crawling
  [file-name depth]
  (let [urls (get-sutable-lines file-name)
        parent (create-root-node urls depth)]
    (visit-node parent urls depth)
    parent)
  )

(defn -main
  [& args]
  (if (not= (count args) 2)
    (println "Failure! The parameter number is illegal")
    (let [root (do-crawling (first args) (last args))]
      (walk-tree root))
    ) #_root
  ) #_-main