(ns kotoba.technology
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def registry-resource "kotoba/technology/registry.edn")

(defn registry
  "Load the technology registry."
  []
  (edn/read-string (slurp (io/resource registry-resource))))

(defn technologies
  ([] (:technologies (registry)))
  ([reg] (:technologies reg)))

(defn by-id
  ([] (by-id (registry)))
  ([reg] (into {} (map (juxt :id identity) (technologies reg)))))

(defn get-technology
  ([id] (get-technology (registry) id))
  ([reg id] (get (by-id reg) id)))

(defn capability-map
  "Return {technology-id matching-capabilities} for the requested capabilities."
  ([capabilities] (capability-map (registry) capabilities))
  ([reg capabilities]
   (let [wanted (set capabilities)]
     (->> (technologies reg)
          (keep (fn [{:keys [id capabilities]}]
                  (let [matches (set/intersection wanted capabilities)]
                    (when (seq matches) [id matches]))))
          (into {})))))

(defn stack
  "Return ordered technology records for ids."
  ([ids] (stack (registry) ids))
  ([reg ids]
   (let [idx (by-id reg)]
     (mapv #(or (get idx %)
                (throw (ex-info "unknown technology" {:id %})))
           ids))))

(defn satisfies-capabilities?
  ([ids capabilities] (satisfies-capabilities? (registry) ids capabilities))
  ([reg ids capabilities]
   (let [provided (->> (stack reg ids) (mapcat :capabilities) set)]
     (set/subset? (set capabilities) provided))))
