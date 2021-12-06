(ns xandrews.day05
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [nextjournal.clerk :as clerk]))

;; # Day 5: Hydrothermal Venture

;; You come across a field of hydrothermal vents on the ocean floor! These vents
;; constantly produce large, opaque clouds, so it would be best to avoid them if
;; possible.

;; They tend to form in lines; the submarine helpfully produces a list of nearby
;; lines of vents (your puzzle input) for you to review. For example:

(def test-input
  "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
")

;; Each line of vents is given as a line segment in the format x1,y1 -> x2,y2
;; where x1,y1 are the coordinates of one end the line segment and x2,y2 are the
;; coordinates of the other end. These line segments include the points at both
;; ends. In other words:

;;     An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
;;     An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.

(re-matches #"(\d+),(\d+) -> (\d+),(\d+)" "0,9 -> 5,9")

(defn parse-coordinates
  [input]
  (->> input
       s/split-lines
       (map (fn [line]
              (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" line)))
       (map (fn [[_ x1 y1 x2 y2]]
              [[(Integer/parseInt x1) (Integer/parseInt y1)]
               [(Integer/parseInt x2) (Integer/parseInt y2)]]))))

(def test-coords
  (parse-coordinates test-input))

;; For now, only consider horizontal and vertical lines: lines where either x1 =
;; x2 or y1 = y2.

(defn is-h-or-v? [line]
  (let [[[x1 y1] [x2 y2]] line]
    (if (or (= x1 x2)
            (= y1 y2))
      true
      false)))

(is-h-or-v? (nth test-coords 0))
(is-h-or-v? (nth test-coords 0))

;; So, the horizontal and vertical lines from the above list would produce the
;; following diagram:

;;     .......1..
;;     ..1....1..
;;     ..1....1..
;;     .......1..
;;     .112111211
;;     ..........
;;     ..........
;;     ..........
;;     ..........
;;     222111....

(defn line->points
  [[[x1 y1] [x2 y2]]]

  (for [x (range (min x1 x2) (inc (max x1 x2)))
        y (range (min y1 y2) (inc (max y1 y2)))]
   [x y]))

;;     An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
;;     An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.
(line->points [[1 1] [1 3]])
(line->points [[9 7] [7 7]])

;; In this diagram, the top left corner is 0,0 and the bottom right corner is
;; 9,9. Each position is shown as the number of lines which cover that point or
;; . if no line covers that point. The top-left pair of 1s, for example, comes
;; from 2,2 -> 2,1; the very bottom row is formed by the overlapping lines 0,9
;; -> 5,9 and 0,9 -> 2,9.

(defn overlapping-points
  [input]
  (->> input
       parse-coordinates
       (filter is-h-or-v?)
       (mapcat line->points)
       (reduce (fn [m point]
                 (let [num-points (get m point 0)]
                   (assoc m point (inc num-points))))
               {})
       (vals)
       (filter (partial < 1))
       count))

(overlapping-points test-input)

;; To avoid the most dangerous areas, you need to determine the number of points
;; where at least two lines overlap. In the above example, this is anywhere in
;; the diagram with a 2 or larger - a total of 5 points.

;; Consider only horizontal and vertical lines. At how many points do at least
;; two lines overlap?

(def input (slurp (io/resource "day05.txt")))
(overlapping-points input)


;; Unfortunately, considering only horizontal and vertical lines doesn't give
;; you the full picture; you need to also consider diagonal lines.

;; Because of the limits of the hydrothermal vent mapping system, the lines in
;; your list will only ever be horizontal, vertical, or a diagonal line at
;; exactly 45 degrees. In other words:

;;     An entry like 1,1 -> 3,3 covers points 1,1, 2,2, and 3,3.
;;     An entry like 9,7 -> 7,9 covers points 9,7, 8,8, and 7,9.

;; Considering all lines from the above example would now produce the following
;; diagram:

;;     1.1....11.
;;     .111...2..
;;     ..2.1.111.
;;     ...1.2.2..
;;     .112313211
;;     ...1.2....
;;     ..1...1...
;;     .1.....1..
;;     1.......1.
;;     222111....

;; You still need to determine the number of points where at least two lines
;; overlap. In the above example, this is still anywhere in the diagram with a 2
;; or larger - now a total of 12 points.

;; Consider all of the lines. At how many points do at least two lines overlap?

(defn line->points-diag
  [[[x1 y1] [x2 y2]]]
  (let [f (fn [a b]
            (if (< a b) inc dec))
        range (fn [a b]
                (if (= a b)
                  (repeat a)
                  (loop [n a
                         v []]
                    (if (= n b)
                      (conj v n)
                      (recur ((f a b) n) (conj v n))))))]
    (map vector (range x1 x2) (range y1 y2))))

(line->points-diag [[1 1] [3 3]])
(line->points-diag [[9 7] [7 9]])
(line->points-diag [[1 1] [1 3]])
(line->points-diag [[9 7] [7 7]])

(defn overlapping-points-incl-diag
  [input]
  (->> input
       parse-coordinates
       (mapcat line->points-diag)
       (reduce (fn [m point]
                 (let [num-points (get m point 0)]
                   (assoc m point (inc num-points))))
               {})
       (vals)
       (filter (partial < 1))
       count))

(overlapping-points-incl-diag test-input)
(overlapping-points-incl-diag input)

#_(nextjournal.clerk/show! "src/xandrews/day05.clj")
