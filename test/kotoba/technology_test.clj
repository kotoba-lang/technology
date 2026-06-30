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
