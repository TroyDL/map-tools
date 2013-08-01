(ns map-tools.core
  (:require [clojure.string :as str]))

(declare sift sift* strain-maps extract-id trim-res)

(defn return
  "Return collection of distinct values from maps of a given key or
   nested key."
  [maps keypath]
  (->> maps (map #(get-in % keypath)) distinct))

(defn distinct-on
  "Eliminate duplicate maps from collection based on [key|nested-key]
    given.
   Note: maps that do not contain the key or those which have nil for
    the value will be removed as well."
  [maps keypath]
  (loop [maps maps test-set #{nil} res ()]
    (if-let [map (first maps)]
      (if-not (contains? test-set (get-in map keypath))
        (recur (next maps)
               (conj test-set (get-in map keypath))
               (conj res map))
        (recur (next maps) test-set res))
      res)))

(defn strain
  "Given a collection of maps and a key path, returns the collection
   excluding those missing that key in their map.  Regardless of value."
  [maps keypath]
  (filter #(get-in % keypath) maps))

(defn sift
  "Filter collection of maps on arbitrary number of kv pairs.
   Optional predicate for nested maps.
   Example: (sift (comp :address :data) schools-coll {:zip 93722})
   Example: (sift teams-coll {:mascot 'Bears' :state 'CA'})"
  ([coll kvmap]
     (sift nil coll kvmap))
  ([pred coll kvmap]
     (reduce #(sift* pred % %2) coll (seq kvmap))))

(defn sift* [pred coll [k v]] ;;Helper for sift fn
  (if pred
    (filter #(= v (get (pred %) k)) coll)
    (filter #(= v (get % k)) coll)))


;;;; IN PROGRESS

(defn proto-flatten-map
  "Take map and return single level map with vector keypaths as the keys.

   Ex:
     (proto-flatten-map {:a {:b {:c 0} :d 1} :e 2})
     => {[:a :b :c] 0, [:a :d] 1, [:e] 2}"
  ([m] (proto-flatten-map m []))
  ([m parent-key] 
     (when m
       (let [res (atom {})] ;; Switch to transients 
         (doseq [[k v] m]
           (if (map? v)
             (if (empty? parent-key) 
               (swap! res into (proto-flatten-map v [k]))
               (swap! res into (proto-flatten-map v (conj parent-key k))))
             (when (map? (select-keys m [k])) 
               (let [[k v] (first (select-keys m [k]))
                     k (conj parent-key k) 
                     new-map {k v}] 
                 (swap! res into new-map)))))
         @res))))

(proto-flatten-map {:a {:b {:c 0} :d 1} :e 2})
