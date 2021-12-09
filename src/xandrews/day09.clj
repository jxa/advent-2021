(ns xandrews.day09
  (:require [clojure.string :as s]
            [clojure.core.matrix :as matrix]
            [clojure.java.io :as io]))

;; # Day 9: Smoke Basin

;; These caves seem to be lava tubes. Parts are even still volcanically active;
;; small hydrothermal vents release smoke into the caves that slowly settles
;; like rain.

;; If you can model how the smoke flows through the caves, you might be able to
;; avoid it and be that much safer. The submarine generates a heightmap of the
;; floor of the nearby caves for you (your puzzle input).

;; Smoke flows to the lowest point of the area it's in. For example, consider
;; the following heightmap:

(def test-heightmap
  "
2199943210
3987894921
9856789892
8767896789
9899965678")

;; Each number corresponds to the height of a particular location, where 9 is
;; the highest and 0 is the lowest a location can be.

(defn parse-heights
  [s]
  (->> s
       s/split-lines
       (filter not-empty)
       (mapv (fn [line]
              (mapv #(Character/digit % 10) line)))))

(def test-heights
  (parse-heights test-heightmap))

;; Your first goal is to find the low points - the locations that are lower than
;; any of its adjacent locations. Most locations have four adjacent locations
;; (up, down, left, and right); locations on the edge or corner of the map have
;; three or two adjacent locations, respectively. (Diagonal locations do not
;; count as adjacent.)

(matrix/select test-heights 0 0)

(defn safe-select-point
  [m x y]
  (try
    (matrix/select m x y)
    (catch Throwable e nil)))

(safe-select-point test-heights -1 -2)

(defn adjacent-seq [m]
  (map (fn [[x y]]
         {:point (safe-select-point m x y)
          :adjacent (filterv (complement nil?)
                             [(safe-select-point m x (inc y))
                              (safe-select-point m x (dec y))
                              (safe-select-point m (inc x) y)
                              (safe-select-point m (dec x) y)])})
       (matrix/index-seq m)))

(adjacent-seq test-heights)

;; In the above example, there are four low points, all highlighted: two are in
;; the first row (a 1 and a 0), one is in the third row (a 5), and one is in the
;; bottom row (also a 5). All other locations on the heightmap have some lower
;; adjacent location, and so are not low points.

(defn low-point?
  [{:keys [point adjacent]}]
  (< point (apply min adjacent)))

(defn low-points
  [height-matrix]
  (filter low-point? (adjacent-seq height-matrix)))

(low-points test-heights)

;; The risk level of a low point is 1 plus its height. In the above example, the
;; risk levels of the low points are 2, 1, 6, and 6. The sum of the risk levels
;; of all low points in the heightmap is therefore 15.

(defn risk
  [{:keys [point]}]
  (inc point))

(reduce + (map risk (low-points test-heights)))

;; Find all of the low points on your heightmap. What is the sum of the risk
;; levels of all low points on your heightmap?

(reduce + (map risk (low-points (parse-heights (slurp (io/resource "day09.txt"))))))
