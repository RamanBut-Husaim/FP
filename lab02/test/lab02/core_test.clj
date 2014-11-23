(ns lab02.core-test
  (:require [clojure.test :refer :all]
            [lab02.core :refer :all]
            [net.cgrand.enlive-html :as html]))

(defn load-formatted-content-from-file
  [file-name]
  (html/html-snippet (slurp file-name)))

(deftest parse-html
  (testing "Parse html page correctly."
    (let [content (load-formatted-content-from-file "resources/index.html")
          uris (get-valid-urls content)]
      (is (= (count uris) 4)))
  ) #_testing
)

(deftest page-not-found
  (testing "Page not found"
    (let [server-respone (fetch-url "http://295e492141d4c35d74f7f8e2fbd9301f.com")]
      (is (= (:status server-respone) 404)))
  ) #_testing
)
