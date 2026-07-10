(ns kotoba.technology
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def registry-resource "kotoba/technology/registry.edn")

;; registry.edn is stored on disk as Datomic/Datascript tx-data (a vector of
;; entity maps, root entity first, one entity per technology referenced by
;; :db/id via :kotoba.registry/technologies) so it stays directly transactable
;; and queryable. `registry` reconstitutes the original bare-keyed shape
;; ({:kotoba.registry/id ... :technologies [{:id ... :name ...} ...]}) from
;; that tx-data so every downstream function below keeps working unchanged.
(defn- tx-data? [content]
  (and (vector? content) (seq content) (map? (first content)) (contains? (first content) :db/id)))

(defn- unnamespace-item [entity]
  (into {} (map (fn [[k v]] [(keyword (name k)) v])) (dissoc entity :db/id)))

(defn- reconstitute-registry [tx]
  (let [by-id (into {} (map (juxt :db/id identity)) tx)
        root (first tx)
        tech-refs (:kotoba.registry/technologies root)
        technologies (mapv #(unnamespace-item (get by-id %)) tech-refs)]
    (-> root
        (dissoc :db/id :kotoba.registry/technologies)
        (assoc :technologies technologies))))

(defn registry
  "Load the technology registry."
  []
  (let [content (edn/read-string (slurp (io/resource registry-resource)))]
    (if (tx-data? content)
      (reconstitute-registry content)
      content)))

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
