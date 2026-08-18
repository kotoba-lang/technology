(ns kotoba.technology-test
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.technology :as tech]))

(deftest registry-loads
  (let [reg (tech/registry)]
    (is (= :kotoba/technology (:kotoba.registry/id reg)))
    (is (<= 10 (count (tech/technologies reg))))))

(deftest resolves-eda-cfd-cae
  (testing "known engineering technologies are addressable by id"
    (is (= "Electronic Design Automation" (:name (tech/get-technology :eda))))
    (is (= "Computational Fluid Dynamics" (:name (tech/get-technology :cfd))))
    (is (= "Computer-Aided Engineering Contract" (:name (tech/get-technology :cae))))))

(deftest capability-resolution
  (let [m (tech/capability-map [:simulation/flow :audit/export])]
    (is (contains? m :cfd))
    (is (contains? m :audit-ledger))))

(deftest stack-satisfies-capabilities
  (is (tech/satisfies-capabilities? [:cfd :audit-ledger]
                                    [:simulation/flow :audit/export]))
  (is (not (tech/satisfies-capabilities? [:forms]
                                         [:simulation/flow]))))

(deftest resolve-stack-partitions-instead-of-throwing
  (testing "known ids resolve to records, exactly as `stack` returns them"
    (let [{:keys [resolved unknown]} (tech/resolve-stack [:telemetry :dmn])]
      (is (= [:telemetry :dmn] (mapv :id resolved)))
      (is (= [] unknown))))
  (testing "an id this registry does not provide is REPORTED, not thrown"
    (let [{:keys [resolved unknown]} (tech/resolve-stack [:telemetry :no-such-technology :dmn])]
      (is (= [:telemetry :dmn] (mapv :id resolved)) "order of what does resolve is preserved")
      (is (= [:no-such-technology] unknown))))
  (testing "`stack` stays strict -- a caller asking for a set wants all of it"
    ;; `clojure.lang.ExceptionInfo` is a JVM class name and does not exist
    ;; under ClojureScript; `ex-info` produces a `cljs.core/ExceptionInfo`
    ;; there. The assertion is the same assertion — this is the reader
    ;; conditional earning its keep rather than papering over a difference.
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                 (tech/stack [:no-such-technology]))))
  (testing "nothing known at all is still an answer, not an exception"
    (is (= {:resolved [] :unknown [:a :b]} (tech/resolve-stack [:a :b])))))

;; ---------------------------------------------------------------------------
;; Portability, and the nil that means `nobody could look`
;;
;; This namespace was `.clj` until 2026-08-18, and one line made it so:
;; `(slurp (io/resource …))`. Through it `kotoba.iso3166` was JVM-only too,
;; and so was anything that wanted either. The workspace's runtime order puts
;; the JVM last, so a registry of facts pinning its consumers to it was the
;; wrong way round.
;;
;; The conversion's risk is not that the reader conditional is missing — it is
;; that the `:cljs` branch is never evaluated. `test/run_portable.cljs` runs
;; this file under nbb for exactly that reason.
;; ---------------------------------------------------------------------------

(deftest an-unreadable-registry-is-nil-and-not-empty
  (testing "a caller must be able to tell `there are no technologies` from
            `nobody could look`. An empty vector would make a missing file
            indistinguishable from an empty registry, which is the defect
            this workspace has found in fourteen places"
    (with-redefs [tech/registry (constantly nil)]
      (is (nil? (tech/technologies)) "nil, not []")
      (testing "and nothing downstream invents an empty answer. `(into {} …)`
                over nil yields `{}`, so an unreadable registry would index
                to an empty map, `get-technology` would answer nil for every
                id, and `capability-map` would report no capabilities — three
                plausible answers all meaning only that a file was missing"
        (is (nil? (tech/by-id)))
        (is (nil? (tech/capability-map [:anything])))
        (is (nil? (tech/get-technology :anything))))
      (is (false? (tech/readable?))))))

(deftest the-registry-is-actually-readable-here
  (testing "the negative test above is only meaningful if the positive one
            passes — otherwise `nil` would be the answer in both cases and
            neither would be measuring anything"
    (is (true? (tech/readable?)))
    (is (seq (tech/technologies)))))

(deftest technologies-reads-the-registry-it-was-given
  (testing "the one-arg form used to call `registry` for the zero-arg case
            and `:technologies` for the one-arg case, so passing a registry
            and passing nothing took different paths"
    (let [reg (tech/registry)]
      (is (= (tech/technologies reg) (tech/technologies))))))

(deftest the-real-read-path-fails-to-nil-not-to-a-plausible-value
  (testing "the test above redefines `registry`, which BYPASSES the resource
            reader entirely — so it measures what `registry` does with a nil
            and never that a failed read produces one. Three mutations
            survived on exactly that gap. Pointing `registry-resource` at a
            path that is not there exercises the reader itself, under both
            runtimes: `:clj` gets no classpath resource, `:cljs` gets an
            fs error it catches"
    (with-redefs [tech/registry-resource "kotoba/technology/no-such-file.edn"]
      (is (nil? (tech/registry)))
      (is (false? (tech/readable?)))
      (is (nil? (tech/technologies)))
      (is (nil? (tech/by-id))))))
