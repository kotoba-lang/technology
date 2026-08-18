(ns kotoba.technology
  "The technology registry, portable.

  ## Why this is `.cljc` and not `.clj`

  One line made this namespace JVM-only — `(slurp (io/resource …))` — and
  through it `kotoba.iso3166`, which requires this, and everything that
  would have wanted either. That is the wrong way round for this workspace:
  the runtime order is kotoba-wasm → clojurewasm → ClojureScript → nbb, with
  the JVM last. A registry of facts has no business being the thing that
  pins a consumer to a runtime.

  ## Reading the resource, and saying when it could not be read

  There is no portable `io/resource`. Under `:clj` this reads the classpath
  resource; under `:cljs` it reads the file from disk relative to the
  process's working directory, which works under nbb and Node and does not
  work in a browser.

  **`registry` returns nil when the resource could not be read, and never an
  empty registry.** A caller must be able to tell *there are no
  technologies* from *nobody could look* — `technologies` therefore also
  answers nil rather than `[]`, and `readable?` is the direct question. An
  empty vector here would make a missing file indistinguishable from an
  empty registry, which is this workspace's most-repeated defect."
  (:require #?(:clj [clojure.edn :as edn] :cljs [cljs.reader :as edn])
            [clojure.set :as set]
            #?(:clj [clojure.java.io :as io])))

(def registry-resource "kotoba/technology/registry.edn")

(defn- read-resource
  "The resource's text, or nil when it could not be read.

  nil is a real answer here and is propagated rather than swallowed."
  [path]
  #?(:clj (some-> (io/resource path) slurp)
     :cljs (try (.readFileSync (js/require "fs") (str "resources/" path) "utf8")
                (catch :default _ nil))))

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
  "Load the technology registry, or **nil** when the resource could not be
  read.

  nil is not an empty registry. Under `:cljs` the resource is read relative
  to the working directory, so a caller running from elsewhere gets nil —
  and must be able to tell that from a registry that genuinely lists
  nothing."
  []
  (when-let [txt (read-resource registry-resource)]
    (let [content (edn/read-string txt)]
      (if (tx-data? content)
        (reconstitute-registry content)
        content))))

(defn readable?
  "Could the registry be read at all? The direct form of the question the
  nils below answer indirectly."
  []
  (some? (registry)))

(defn technologies
  ([] (technologies (registry)))
  ([reg] (:technologies reg)))

(defn by-id
  "Technologies indexed by `:id`, or **nil** when the registry could not be
  read.

  `(into {} …)` over nil yields `{}`, which is why this needs saying: an
  unreadable registry would otherwise index to an empty map, `get-technology`
  would answer nil for every id, and `resolve-stack` would report every id as
  unknown — three plausible-looking answers, all of them meaning only that a
  file was missing. Caught here by the test that redefines `registry` to nil,
  after the same class had already been found fourteen times elsewhere in
  this workspace."
  ([] (by-id (registry)))
  ([reg] (when-let [ts (technologies reg)]
           (into {} (map (juxt :id identity)) ts))))

(defn get-technology
  ([id] (get-technology (registry) id))
  ([reg id] (get (by-id reg) id)))

(defn capability-map
  "Return {technology-id matching-capabilities} for the requested capabilities."
  ([capabilities] (capability-map (registry) capabilities))
  ([reg capabilities]
   (when-let [ts (technologies reg)]
     (let [wanted (set capabilities)]
       (->> ts
          (keep (fn [{:keys [id capabilities]}]
                  (let [matches (set/intersection wanted capabilities)]
                      (when (seq matches) [id matches]))))
            (into {}))))))

(defn stack
  "Return ordered technology records for ids. Throws on an id this
  registry does not provide.

  Strict on purpose: a caller asking for a specific set of technologies
  and getting a shorter list back would silently proceed with less than
  it asked for. Callers that are ASKING WHETHER a technology exists want
  `resolve-stack` instead."
  ([ids] (stack (registry) ids))
  ([reg ids]
   (let [idx (by-id reg)]
     (mapv #(or (get idx %)
                (throw (ex-info "unknown technology" {:id %})))
           ids))))

(defn resolve-stack
  "Partition `ids` into the technology records this registry provides and
  the ids it does not.

  `{:resolved [record ..] :unknown [id ..]}`.

  This exists because a consumer registry naming a technology nobody has
  built yet is a NORMAL state, not a programming error — it is a
  backlog entry. `stack` throwing turned every such entry into an
  exception that took down the whole query, so a caller could not even
  ask what was missing. Measured 2026-08-03: 29 of the 651 industries in
  `kotoba-lang/industry` named at least one technology absent from here,
  and `execution-plan` was unusable for every one of them."
  ([ids] (resolve-stack (registry) ids))
  ([reg ids]
   (let [idx (by-id reg)]
     {:resolved (into [] (keep idx) ids)
      :unknown  (into [] (remove idx) ids)})))

(defn satisfies-capabilities?
  ([ids capabilities] (satisfies-capabilities? (registry) ids capabilities))
  ([reg ids capabilities]
   (let [provided (->> (stack reg ids) (mapcat :capabilities) set)]
     (set/subset? (set capabilities) provided))))
