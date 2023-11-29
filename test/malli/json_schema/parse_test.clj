(ns malli.json-schema.parse-test
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [malli.util-test :as tu]
            [malli.json-schema.parse3 :as sut]))

(def json-schema-tests-dir
  ".gitlibs/libs/io.github.json-schema-org/JSON-Schema-Test-Suite/3dab98cae07f244afcb278c0b7a03c0d8975a3de/tests")

(def test-files
  ["draft7/properties.json" "draft7/ref.json"])

(defn read-test-suite-file [path]
  (-> (slurp (io/file (System/getProperty "user.home") json-schema-tests-dir path))
      (tu/from-json)
      (walk/keywordize-keys)))

(deftest json-schema-test-suite-test
  (doseq [test-file test-files]
    (testing test-file
      (doseq [{:keys [schema tests description]} (read-test-suite-file test-file)]
        (testing description
          (let [malli (try (sut/json-schema-document->malli schema)
                           (catch Throwable ex
                             (is false
                                 (str "failed to create malli for schema: " \newline
                                      schema \newline
                                      ex))))
                malli-schema (try (when malli (m/schema malli))
                                  (catch Throwable ex
                                    (is false
                                        (str "failed to create malli-schema or schema: " \newline
                                             schema \newline
                                             "from malli:" \newline
                                             malli \newline
                                             ex))))]
            (when (and malli-schema (m/schema? malli-schema))
              (doseq [{:keys [valid data description]} tests]
                (testing description
                  (is (= valid (m/validate malli-schema data))
                      (str "schema: " schema \newline
                           "data: " data \newline
                           "malli: " malli \newline
                           (if valid
                             (str "valid, but got errors: " \newline
                                  (string/join \newline (:errors (m/explain malli-schema data))))
                             "invalid, but matched"))))))))))))
