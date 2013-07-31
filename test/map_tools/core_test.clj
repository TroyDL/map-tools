(ns map-tools.core-test
  (:require [clojure.test :refer :all]
            [map-tools.core :refer :all]))

(run-all-tests)

(deftest return-test
  (let [m1 '({:k1 "v1" :k2 {:k2* "v2*"}} {:k1 "v3" :k2 {:k2* "v4*"}})
        m2 '({"k1" "v1", "k2" {"k2*" "v2*"}}
             {"k1" "v3", "k2" {"k2*" "v4*"}})
        m3 '({1 "v1" 2 {3 "v2*"}} {1 "v3" 2 {3 "v4*"}})
        t1-1 (return m1 [:k1])
        t1-2 (return m1 [:k2])
        t1-3 (return m1 [:k2 :k2*])
        e1-1 '("v1" "v3")
        e1-2 '({:k2* "v2*"} {:k2* "v4*"})
        e1-3 '("v2*" "v4*")
        t2-1 (return m2 ["k1"])
        t2-2 (return m2 ["k2"])
        t2-3 (return m2 ["k2" "k2*"])
        e2-1 '("v1" "v3")
        e2-2 '({"k2*" "v2*"} {"k2*" "v4*"})
        e2-3 '("v2*" "v4*")
        t3-1 (return m3 [1])
        t3-2 (return m3 [2])
        t3-3 (return m3 [2 3])
        e3-1 '("v1" "v3")
        e3-2 '({3 "v2*"} {3 "v4*"})
        e3-3 '("v2*" "v4*")
        ]
    (is (=
         t1-1
         e1-1))
    (is (=
         t1-2
         e1-2))
    (is (=
         t1-3
         e1-3))))

(deftest distinct-on-test
  (let [m1 '({:k1 "v1" :k2 {:k2* "v2*"}} {:k1 "v3" :k2 {:k2* "v4*"}}
             {:k1 "v1" :k2 {:k3* "v2*"}} {:k1 "v1" :k3 {:k2* "v4"}})
        m2 '({"k1" "v1", "k2" {"k2*" "v2*"}}
             {"k1" "v3", "k2" {"k2*" "v4*"}}
             {"k1" "v1", "k2" {"k3*" "v2*"}}
             {"k1" "v1", "k3" {"k2*" "v4"}})
        m3 '({1 "v1" 2 {3 "v2*"}} {1 "v3" 2 {3 "v4*"}}
             {1 "v1" 2 {4 "v2*"}} {1 "v1" 3 {3 "v4"}})

        ;; Basic unnested key test -- Regular keywords
        t1-1 (distinct-on m1 [:k1])
        ;; Unnested key with a nested value
        t1-2 (distinct-on m1 [:k2])
        ;; Nested key via keypath
        t1-3 (distinct-on m1 [:k2 :k2*])
        e1-1 '({:k1 "v3" :k2 {:k2* "v4*"}} {:k1 "v1" :k2 {:k2* "v2*"}})
        e1-2 '({:k1 "v1" :k2 {:k3* "v2*"}} {:k1 "v3" :k2 {:k2* "v4*"}}
               {:k1 "v1" :k2 {:k2* "v2*"}})
        e1-3 '({:k1 "v3" :k2 {:k2* "v4*"}} {:k1 "v1" :k2 {:k2* "v2*"}})

        ;; Strings as keywords
        t2-1 (distinct-on m2 ["k1"])
        t2-2 (distinct-on m2 ["k2"])
        t2-3 (distinct-on m2 ["k2" "k2*"])
        e2-1 '({"k1" "v3", "k2" {"k2*" "v4*"}}
               {"k1" "v1", "k2" {"k2*" "v2*"}})
        e2-2 '({"k1" "v1", "k2" {"k3*" "v2*"}}
               {"k1" "v3", "k2" {"k2*" "v4*"}}
               {"k1" "v1", "k2" {"k2*" "v2*"}})
        e2-3 '({"k1" "v3", "k2" {"k2*" "v4*"}}
               {"k1" "v1", "k2" {"k2*" "v2*"}})

        ;; Numbers as keywords
        t3-1 (distinct-on m3 [1])
        t3-2 (distinct-on m3 [2])
        t3-3 (distinct-on m3 [2 3])
        e3-1 '({1 "v3" 2 {3 "v4*"}} {1 "v1" 2 {3 "v2*"}})
        e3-2 '({1 "v1" 2 {4 "v2*"}} {1 "v3" 2 {3 "v4*"}}
               {1 "v1" 2 {3 "v2*"}})
        e3-3 '({1 "v3" 2 {3 "v4*"}} {1 "v1" 2 {3 "v2*"}})]

    (is (=
         t1-1
         e1-1))
    (is (=
         t1-2
         e1-2))
    (is (=
         t1-3
         e1-3))

    (is (=
         t2-1
         e2-1))
    (is (=
         t2-2
         e2-2))
    (is (=
         t2-3
         e2-3))

    (is (=
         t3-1
         e3-1))
    (is (=
         t3-2
         e3-2))
    (is (=
         t3-3
         e3-3))))

(deftest strain-test
  (let [m0 '({:k1 1} {:k1 2} {:k2 3} {:k2 {:k1 4}} {:k1 {:k2 5}})
        m1 '({"k1" 1} {"k1" 2} {"k2" 3} {"k2" {"k1" 4}} {"k1" {"k2" 5}})
        m2 '({1 1} {1 2} {2 3} {2 {1 4}} {1 {2 5}})

        t0 (strain m0 [:k1])
        e0 '({:k1 1} {:k1 2} {:k1 {:k2 5}})

        t1 (strain m1 ["k1"])
        e1 '({"k1" 1} {"k1" 2} {"k1" {"k2" 5}})

        t2 (strain m2 [1])
        e2 '({1 1} {1 2} {1 {2 5}})]
    (is (=
         t0
         e0))
    (is (=
         t1
         e1))
    (is (=
         t2
         e2))))


;;TODO
(comment (deftest sift-test))
