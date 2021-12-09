(ns xandrews.day07
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]))

;; # Day 7: The Treachery of Whales

;; A giant whale has decided your submarine is its next meal, and it's much
;; faster than you are. There's nowhere to run!

;; Suddenly, a swarm of crabs (each in its own tiny submarine - it's too deep
;; for them otherwise) zooms in to rescue you! They seem to be preparing to
;; blast a hole in the ocean floor; sensors indicate a massive underground cave
;; system just beyond where they're aiming!

;; The crab submarines all need to be aligned before they'll have enough power
;; to blast a large enough hole for your submarine to get through. However, it
;; doesn't look like they'll be aligned before the whale catches you! Maybe you
;; can help?

;; There's one major catch - crab submarines can only move horizontally.

;; You quickly make a list of the horizontal position of each crab (your puzzle
;; input). Crab submarines have limited fuel, so you need to find a way to make
;; all of their horizontal positions match while requiring them to spend as
;; little fuel as possible.

;; For example, consider the following horizontal positions:

;;     16,1,2,0,4,2,7,1,2,14

(def test-input "16,1,2,0,4,2,7,1,2,14")

;; This means there's a crab with horizontal position 16, a crab with horizontal
;; position 1, and so on.

(defn positions
  [in]
  (as-> in x
      (s/split x #",")
      (map #(Integer/parseInt %) x)))

(positions test-input)

;; Each change of 1 step in horizontal position of a single crab costs 1 fuel.
;; You could choose any horizontal position to align them all on, but the one
;; that costs the least fuel is horizontal position 2:

;;     Move from 16 to 2: 14 fuel
;;     Move from 1 to 2: 1 fuel
;;     Move from 2 to 2: 0 fuel
;;     Move from 0 to 2: 2 fuel
;;     Move from 4 to 2: 2 fuel
;;     Move from 2 to 2: 0 fuel
;;     Move from 7 to 2: 5 fuel
;;     Move from 1 to 2: 1 fuel
;;     Move from 2 to 2: 0 fuel
;;     Move from 14 to 2: 12 fuel

(defn default-crab-cost
  [a b]
  (if (> a b)
    (- a b)
    (- b a)))

(defn alignment-costs
  ([positions]
   (alignment-costs positions default-crab-cost))
  ([positions cost-fn]
   (reduce (fn [v p]
             (assoc v p
                    (reduce (fn [cost position]
                              (+ cost
                                 (cost-fn position p)))
                            0
                            positions)))
           []
           (range (apply min positions) (inc (apply max positions))))))

(def test-costs
  (alignment-costs (positions test-input)))

;; This costs a total of 37 fuel. This is the cheapest possible outcome; more
;; expensive outcomes include aligning at position 1 (41 fuel), position 3 (39
;; fuel), or position 10 (71 fuel).

(defn min-index
  [v]
  (.indexOf v (apply min v)))

(defn cheapest-position
  [input]
  (-> input
      positions
      alignment-costs
      min-index))

(cheapest-position test-input)

;; Determine the horizontal position that the crabs can align to using the least
;; fuel possible. How much fuel must they spend to align to that position?

(defn least-fuel
  [input]
  (->> input
      positions
      alignment-costs
      (apply min)))

(least-fuel test-input)

(least-fuel
 (s/trim (slurp (io/resource "day07.txt"))))

;; # Part Two ---

;; The crabs don't seem interested in your proposed solution. Perhaps you
;; misunderstand crab engineering?

;; As it turns out, crab submarine engines don't burn fuel at a constant rate.
;; Instead, each change of 1 step in horizontal position costs 1 more unit of
;; fuel than the last: the first step costs 1, the second step costs 2, the
;; third step costs 3, and so on.

(defn arithmetic-sum
  [n]
  (* (/ n 2)
   (+ 2 (dec n))))

(defn crab-cost
  [a b]
  (let [x (min a b)
        y (max a b)
        n ( - y x)]
    (arithmetic-sum n)))

(crab-cost 1 0)
(crab-cost 1 1)
(crab-cost 1 2)
(crab-cost 1 5)
(crab-cost 5 16)
(crab-cost 16 5)

;; As each crab moves, moving further becomes more expensive. This changes the
;; best horizontal position to align them all on; in the example above, this
;; becomes 5:

;;     Move from 16 to 5: 66 fuel
;;     Move from 1 to 5: 10 fuel
;;     Move from 2 to 5: 6 fuel
;;     Move from 0 to 5: 15 fuel
;;     Move from 4 to 5: 1 fuel
;;     Move from 2 to 5: 6 fuel
;;     Move from 7 to 5: 3 fuel
;;     Move from 1 to 5: 10 fuel
;;     Move from 2 to 5: 6 fuel
;;     Move from 14 to 5: 45 fuel

;; This costs a total of 168 fuel. This is the new cheapest possible outcome;
;; the old alignment position (2) now costs 206 fuel instead.

(defn least-fuel-v2
  [input cost-fn]
  (as-> input x
      (positions x)
      (alignment-costs x crab-cost)
      (apply min x)))

(least-fuel-v2 test-input crab-cost)

;; Determine the horizontal position that the crabs can align to using the least
;; fuel possible so they can make you an escape route! How much fuel must they
;; spend to align to that position?

(least-fuel-v2
 (s/trim (slurp (io/resource "day07.txt")))
 crab-cost)
