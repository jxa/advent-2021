(ns xandrews.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.core.matrix :as matrix]
            [nextjournal.clerk :as clerk]))

;; # Day 4: Giant Squid

(def input (s/split-lines (slurp (io/resource "day04.txt"))))
(def numbers-called (mapv #(Integer/parseInt %) (s/split (first input) #",")))

;; You're already almost 1.5km (almost a mile) below the surface of the ocean,
;; already so deep that you can't see any sunlight. What you can see, however,
;; is a giant squid that has attached itself to the outside of your submarine.

;; Maybe it wants to play bingo?

;; Bingo is played on a set of boards each consisting of a 5x5 grid of numbers.
;; Numbers are chosen at random, and the chosen number is marked on all boards
;; on which it appears. (Numbers may not appear on all boards.) If all numbers
;; in any row or any column of a board are marked, that board wins. (Diagonals
;; don't count.)

;; The submarine has a bingo subsystem to help passengers (currently, you and
;; the giant squid) pass the time. It automatically generates a random order in
;; which to draw numbers and a random set of boards (your puzzle input). For
;; example:

(def test-numbers [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1])
(def test-board-str
  "

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
")

(defn parse-row
  [s]
  (mapv #(Integer/parseInt %)
        (filter not-empty (s/split s #" "))))

(defn parse-boards
  [lines]
  (loop [boards []
         board []
         lines lines]
    (let [line (first lines)]
      (cond
        (nil? line)
        (conj boards board)

        (and (empty? line) (not-empty board))
        (recur (conj boards board) [] (next lines))

        (empty? line)
        (recur boards board (next lines))

        :else
        (recur boards
               (conj board (parse-row line))
               (next lines))))))

(def test-boards
  (parse-boards (s/split-lines test-board-str)))

(def a-board (first test-boards))

(clerk/table a-board)

(clerk/table (matrix/eq a-board 7))

(clerk/table
 (matrix/add
  (matrix/eq a-board 7)
  (matrix/eq a-board 4)))

(matrix/eq test-boards 7)

;; After the first five numbers are drawn (7, 4, 9, 5, and 11), there are no
;; winners, but the boards are marked as follows (shown here adjacent to each
;; other to save space):

;;     22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
;;      8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
;;     21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
;;      6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
;;      1 12 20 15 19        14 21 16 12  6         2  0 12  3  7

;; After the next six numbers are drawn (17, 23, 2, 0, 14, and 21), there are
;; still no winners:

;;     22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
;;      8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
;;     21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
;;      6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
;;      1 12 20 15 19        14 21 16 12  6         2  0 12  3  7

;; Finally, 24 is drawn:

;;     22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
;;      8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
;;     21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
;;      6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
;;      1 12 20 15 19        14 21 16 12  6         2  0 12  3  7

;; At this point, the third board wins because it has at least one complete row
;; or column of marked numbers (in this case, the entire top row is marked: 14
;; 21 17 24 4).

;; Each board has a score which is 0 by default, 1 if the number has been called.
(def win-row [1 1 1 1 1])

(defn winner?
  [score]
  (boolean
   (or
    (some #(= win-row %) (matrix/rows score))
    (some #(= win-row %) (matrix/columns score)))))

(defn call-number
  "Returns the new scores matrix after applying the called number"
  [boards scores number]
  (matrix/add scores
              (matrix/eq boards number)))

;; scores can be initialized with scalar 0
(call-number test-boards 0 7)

(def winner-stats
  (loop [scores (matrix/mul test-boards 0)
         numbers test-numbers
         prev-number nil]
    (let [number (first numbers)
          winners (mapv winner? scores)
          winner-idx (.indexOf winners true)]
      (if (< 0 winner-idx)
        [(nth test-boards winner-idx) (nth scores winner-idx) prev-number]
        (recur (call-number test-boards scores number)
               (rest numbers)
               number)))))

;; The score of the winning board can now be calculated. Start by finding the
;; sum of all unmarked numbers on that board; in this case, the sum is 188.
;; Then, multiply that sum by the number that was just called when the board
;; won, 24, to get the final score, 188 * 24 = 4512.

;; We get unmarked numbers from the score matrix.
(def unmarked-number-sum
  (matrix/esum
   (matrix/mul
    (nth winner-stats 0)
    (matrix/eq (nth winner-stats 1) 0))))

(def final-score
  (* unmarked-number-sum (nth winner-stats 2)))

(defn winner
  [boards numbers]
  (loop [scores (matrix/mul boards 0)
         numbers numbers
         prev-number nil]
    (let [number (first numbers)
          winners (mapv winner? scores)
          winner-idx (.indexOf winners true)]
      (if (< 0 winner-idx)
        [(nth boards winner-idx) (nth scores winner-idx) prev-number]
        (recur (call-number boards scores number)
               (rest numbers)
               number)))))

(defn winning-board-score [board score number]
  (* number
     (matrix/esum
      (matrix/mul board
                  (matrix/eq score 0)))))

(apply winning-board-score (winner test-boards test-numbers))

;; To guarantee victory against the giant squid, figure out which board will win
;; first. What will your final score be if you choose that board?

(apply winning-board-score
       (winner (parse-boards (rest input)) numbers-called))

;; # Part Two

;; On the other hand, it might be wise to try a different strategy: let the
;; giant squid win.

;; You aren't sure how many bingo boards a giant squid could play at once, so
;; rather than waste time counting its arms, the safe thing to do is to figure
;; out which board will win last and choose that one. That way, no matter which
;; boards it picks, it will win for sure.

;; In the above example, the second board is the last to win, which happens
;; after 13 is eventually called and its middle column is completely marked. If
;; you were to keep playing until this point, the second board would have a sum
;; of unmarked numbers equal to 148 for a final score of 148 * 13 = 1924.

;; Figure out which board will win last. Once it wins, what would its final
;; score be?

(defn last-to-win
  [boards numbers]
  (loop [scores (matrix/mul boards 0)
         numbers numbers
         prev-number nil
         last-idx -1]
    (let [number (first numbers)
          winners (mapv winner? scores)
          idx (.indexOf winners false)]
      (if (every? true? winners)
        [(nth boards last-idx) (nth scores last-idx) prev-number]
        (recur (call-number boards scores number)
               (rest numbers)
               number
               (if (<= 0 idx) idx last-idx))))))

(apply winning-board-score
       (last-to-win test-boards test-numbers))

(apply winning-board-score
       (last-to-win (parse-boards (rest input)) numbers-called))

#_(nextjournal.clerk/show! "src/xandrews/day04.clj")
