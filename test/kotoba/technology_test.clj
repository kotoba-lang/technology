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
    (is (thrown? clojure.lang.ExceptionInfo (tech/stack [:no-such-technology]))))
  (testing "nothing known at all is still an answer, not an exception"
    (is (= {:resolved [] :unknown [:a :b]} (tech/resolve-stack [:a :b])))))
