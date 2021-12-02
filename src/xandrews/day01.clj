(ns xandrews.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input-1 (slurp (io/resource "day01.1.txt")))

(def sonar-readings (map #(Integer/parseInt %) (s/split input-1 #"\n")))

;; # Part 1

(reduce (fn [[count prev] cur]
          (if (> cur prev)
            [(inc count) cur]
            [count cur]))
        [0 100000000000]
        sonar-readings)

;; # Part 2

(def triple-readings
  "return a new collection containing three consecutive values from sonar readings"
  (map (fn [& args] args)
       sonar-readings
       (next sonar-readings)
       (nnext sonar-readings)))

(reduce (fn [[count prevsum] cur]
          (let [sum (reduce + cur)]
            (if (> sum prevsum)
              [(inc count) sum]
              [count sum])))
        [0 100000000000]
        triple-readings)
