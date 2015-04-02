(ns playhll.datagen
  (:use [clojure.pprint :only (pprint)])
  (:require (bigml.sketchy [hyper-loglog :as hll]
                           [min-hash :as mh])
            (clojure [set :as set])))

(defn dataset-1
 [{:keys [domain minsize maxsize gen]}]
  (into [] 
    (for [_ (range gen)]
    (let [s (+ minsize (rand-int (- maxsize minsize)))]
      (into [] 
            (for [_ (range s)] (rand-int domain)))))))

(defn naive-score
  [x y]
  (let [x (set x)
        y (set y)]
    (/ (float (count (set/intersection x y))) (count x))))

(defn naive-match
  [{:keys [query data]}]
  (apply max-key #(apply + %) data))

(defn -main []
  (let [ds (dataset-1 {:domain 100 :minsize 10 :maxsize 20 :gen 3})
        qry (first 
              (dataset-1 {:domain 10 :minsize 10 :maxsize 10 :gen 1}))]
    (println "Data sets:")
    (pprint ds)
    (println "Query set:")
    (pprint qry)
    (let [d* (naive-match {:query qry :data ds})]
      (println "Match:" d*)
      (println "Score = ", (naive-score qry d*)))))

