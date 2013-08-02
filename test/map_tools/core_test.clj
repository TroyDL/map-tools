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
    (is (= t1-1 e1-1))
    (is (= t1-2 e1-2))
    (is (= t1-3 e1-3))))

(deftest distinct-on-test
  (let [m1 '({:k1 "v1" :k2 {:k2* "v2*"}} {:k1 "v3" :k2 {:k2* "v4*"}}
             {:k1 "v1" :k2 {:k3* "v2*"}} {:k1 "v1" :k3 {:k2* "v4"}})
        m2 '({"k1" "v1", "k2" {"k2*" "v2*"}}
             {"k1" "v3", "k2" {"k2*" "v4*"}}
             {"k1" "v1", "k2" {"k3*" "v2*"}}
             {"k1" "v1", "k3" {"k2*" "v4"}})
        m3 '({1 "v1" 2 {3 "v2*"}} {1 "v3" 2 {3 "v4*"}}
             {1 "v1" 2 {4 "v2*"}} {1 "v1" 3 {3 "v4"}})
        e1-1 '({:k1 "v3" :k2 {:k2* "v4*"}} {:k1 "v1" :k2 {:k2* "v2*"}})
        e1-2 '({:k1 "v1" :k2 {:k3* "v2*"}} {:k1 "v3" :k2 {:k2* "v4*"}}
               {:k1 "v1" :k2 {:k2* "v2*"}})
        e1-3 '({:k1 "v3" :k2 {:k2* "v4*"}} {:k1 "v1" :k2 {:k2* "v2*"}})
        e2-1 '({"k1" "v3", "k2" {"k2*" "v4*"}}
               {"k1" "v1", "k2" {"k2*" "v2*"}})
        e2-2 '({"k1" "v1", "k2" {"k3*" "v2*"}}
               {"k1" "v3", "k2" {"k2*" "v4*"}}
               {"k1" "v1", "k2" {"k2*" "v2*"}})
        e2-3 '({"k1" "v3", "k2" {"k2*" "v4*"}}
               {"k1" "v1", "k2" {"k2*" "v2*"}})
        e3-1 '({1 "v3" 2 {3 "v4*"}} {1 "v1" 2 {3 "v2*"}})
        e3-2 '({1 "v1" 2 {4 "v2*"}} {1 "v3" 2 {3 "v4*"}}
               {1 "v1" 2 {3 "v2*"}})
        e3-3 '({1 "v3" 2 {3 "v4*"}} {1 "v1" 2 {3 "v2*"}})
        ms [m1 m2 m3]
        e1s [e1-1 e1-2 e1-3]
        e2s [e2-1 e2-2 e2-3]
        e3s [e3-1 e3-2 e3-3]
        v1s [[:k1] [:k2] [:k2 :k2*]]
        v2s [["k1"] ["k2"] ["k2" "k2*"]]
        v3s [[1] [2] [2 3]]
        t1s (map #(distinct-on m1 %) v1s) 
        t2s (map #(distinct-on m2 %) v2s) 
        t3s (map #(distinct-on m3 %) v3s)]
    (doseq [[e t] (map list e1s t1s)]
      (is (= t e)))
    (doseq [[e t] (map list e1s t1s)]
      (is (= t e)))
    (doseq [[e t] (map list e1s t1s)]
      (is (= t e)))))

(deftest strain-test
  (let [m0 '({:k1 1} {:k1 2} {:k2 3} {:k2 {:k1 4}} {:k1 {:k2 5}})
        m1 '({"k1" 1} {"k1" 2} {"k2" 3} {"k2" {"k1" 4}} {"k1" {"k2" 5}})
        m2 '({1 1} {1 2} {2 3} {2 {1 4}} {1 {2 5}})
        ms [m0 m1 m2]
        e0 '({:k1 1} {:k1 2} {:k1 {:k2 5}})
        e1 '({"k1" 1} {"k1" 2} {"k1" {"k2" 5}})
        e2 '({1 1} {1 2} {1 {2 5}})
        es [e0 e1 e2]
        vs [[:k1] ["k1"] [1]]
        ts (map #(strain % %2) ms vs)]
    (doseq [[e t] (map list es ts)]
      (is (= t e)))))

(deftest flatten-map-test
  (let [m0 {}
        m1 {:a 0}
        m2 {:a 0 :b 1}
        m3 {"a" 0}
        m4 {0 1}
        m5 {"a" "0" "b" '(1)}
        m6 {0 "1" 1 "2"}
        m7 {:a {:a1 0}}
        m8 {:a {:a1 {:a2 0}}}
        m9 {:a {:a1 0 :a1.1 {:a2 1 :a2.2 {:a3 2} :a2.3 3} :a1.2 4} :b {:b1 5}}
        e0 {}
        e1 {[:a] 0}
        e2 {[:a] 0 [:b] 1}
        e3 {["a"] 0}
        e4 {[0] 1}
        e5 {["a"] "0" ["b"] '(1)}
        e6 {[0] "1" [1] "2"}
        e7 {[:a :a1] 0}
        e8 {[:a :a1 :a2] 0}
        e9 {[:a :a1] 0 [:a :a1.1 :a2] 1 [:a :a1.1 :a2.2 :a3] 2
            [:a :a1.1 :a2.3] 3 [:a :a1.2] 4 [:b :b1] 5}
        ms [m0 m1 m2 m3 m4 m5 m6 m7 m8 m9]
        es [e0 e1 e2 e3 e4 e5 e6 e7 e8 e9]
        ts (map flatten-map ms)]
    (doseq [[e t] (map list es ts)]
      (is (= e t)))))

(deftest find-keypaths-test
  (let [m0 {}
        m1 {:a 0}
        m2 {:a {:b 0}}
        m3 {:a {"b" {'(c) {4 {:e 6} :e 6}}}}
        ms [m0 m1 m2 m3]
        es [nil '([:a]) '([:a :b]) '([:a "b" (c) 4 :e] [:a "b" (c) :e])]
        ts (map #(find-keypaths % %2) ms [{:a 1} {:a 0} {:b 0} {:e 6}])]
    (println ts)
    (doseq [[e t] (map list es ts)]
      (is (= e t)))))

;;TODO
(comment (deftest sift-test))
