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

(comment (defn ssift
            "Given a collection of maps and a collection of key-value pairs will
   return a collection maps where every key-value pair exists in each
   map.
   Example: (sift my-maps {:key1 'val1', :key2 'val2'})
   Example: (sift user-maps {:zip 93722 :lastname 'Smith'})"
            [coll kv]
            (let [kv (seq kv)])
            (loop [maps coll
                   res ()]
              (if maps
                (if (= (second kv)
                       (get-in (first maps) (first kv)))
                  (recur (next maps)
                         (conj res (first maps))))
                res)))

         (defn sssift
           ([map [k v]]
              (sssift map (vec (list k)) v))
           ([map keypath val]
              (println "keypath =" keypath)
              (if (= val (get-in map keypath))
                map ;;if kv pair is in map, return map
                (when (map? (get-in map keypath))
                  (for [key (keys (get-in map keypath))]
                    (sssift map (conj (seq keypath) key) val))))))


         (defn s1 [map1 key val]
           (println map1)
           (when (map? map1) 
             (if (= val (get map1 key))
               map1 ;;if kv pair is in map, return map
               (filter map? (map (first map1) (keys (first map1)))))))

         (defn pull-1st-level-maps [m1]
           (->> m1
                keys
                (map m1)
                (filter map?)))


         (defn s0 [maps kv-map]
           (apply (reduce s1) (seq kv-map)))

         (defn sift!
           [maps kvs]
           (->> (map #(map (fn [kv] (sssift % kv)) (seq kvs)) maps)
                (map first)
                (filter not-empty)))

         #_(filter not-empty (map first (map #(map (fn [kv] (sssift % kv)) kvs) maps)))
         )
