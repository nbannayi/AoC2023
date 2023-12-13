;; Advent of Code 2023, Day 08 - Haunted Wasteland.
;; Clojure.

(ns HauntedWasteland
  (:gen-class) 
  (:require [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String helper functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Strip first and last chars off a string.
(defn remove-first-last-char [s]
  (subs s 1 (dec (count s))))

;; Strip last char off a string.
(defn remove-last-char [s] 
  (subs s 0 (dec (count s))))

;; Strip first char off a string.
(defn remove-first-char [s] 
  (subs s 1 (count s)))

;; Return last char of string.
(defn get-last-char [s]
  (nth s (dec (count s))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; lcm helper functions.
;;;;;;;;;;;;;;;;;;;;;;;;

;; Greatest common divisor.
(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

;; Lowest common multiple.
(defn lcm [a b]
  (if (zero? a)
    0
    (/ (Math/abs (* a b)) (gcd a b))))

;; Function to work out lcm on an arbitrary length vector.
(defn lcm-of-vector [numbers]
  (reduce lcm 1 numbers))

;;;;;;;;;;;;;;;;;;;;;
;; Parsing functions.
;;;;;;;;;;;;;;;;;;;;;

;; Parse an individual map line into a vector.
(defn parse-map-line [map-line]
  (let [tokens (str/split map-line #" ")
        key (nth tokens 0)
        left (remove-first-last-char (nth tokens 2))
        right (remove-last-char (nth tokens 3))]
    [(str ":" key) [left right]]))
    
;; Parse the input directions.
(defn get-directions [inputFile]  
  (let [tokens (str/split (slurp inputFile) #"\n")
        directions (nth tokens 0)]
    directions))

;; Parse the destinations.
(defn get-destinations-map [inputFile]
  (let [tokens (str/split (slurp inputFile) #"\n")
        destinations (map parse-map-line (subvec tokens 2 (count tokens)))
        destinations-map (into {} destinations)] 
    destinations-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 1 specific functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Get direction at step.
(defn get-direction-at-step [directions, n]
  (->> (mod (dec n) (count directions))
       (nth directions)))
  
;; Navigate 1 step and return the next location.
(defn navigate-step [step, current-location, directions, destinations-map]
  (let [l-or-r (get-direction-at-step directions step)
        next-location-vec (get destinations-map (str ":" current-location))
        next-location (if (= l-or-r \L)
                        (nth next-location-vec 0)
                        (nth next-location-vec 1))] 
  next-location))

;; Navigate through the map and return the number of steps to get to the end.
(defn get-no-steps-1 [step, current-location, directions, destinations-map] 
  (let [next-location (navigate-step step, current-location, directions, destinations-map)] 
    (if (= next-location "ZZZ")
      step
      (recur (inc step) next-location directions destinations-map))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2 specific functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Get all keys in destination map that end in A and return as vector. 
(defn get-start-locations [destination-map]
  (let [locations (into [] (keys destination-map))]
    (vec (map remove-first-char (filter (fn [l] (= (get-last-char l) \A)) locations)))))

;; Modification to search for different end condition for part 2
(defn get-no-steps-vectorised [step, current-location, directions, destinations-map]
  (let [next-location (navigate-step step, current-location, directions, destinations-map)] 
    (if (= (get-last-char next-location) \Z) 
      step 
      (recur (inc step) next-location directions destinations-map))))

;; Find indvidual cycle lengths then find lowest common multiple - that's when they will be in sync. 
(defn get-no-steps-2 [step, current-location-vector, directions, destinations-map]
  (let [cycles-vector (vec (map (fn [location] (get-no-steps-vectorised step location directions destinations-map)) current-location-vector))]
    (lcm-of-vector cycles-vector)))

;;;;;;;;;;;;;;;;;;;;;;;
;; Put it all together.
;;;;;;;;;;;;;;;;;;;;;;;

;; Get all input.
(def input-file "Day08Input.txt")
(def directions (get-directions input-file))
(def destinations-map (get-destinations-map input-file))

;; Part 1.
(def no-steps-1 (get-no-steps-1 1 "AAA" directions destinations-map))
(println "Part 1 answer:" no-steps-1)

;; Part 2.
(def start-locations (get-start-locations destinations-map))
(def no-steps-2 (get-no-steps-2 1 start-locations directions destinations-map))
(println "Part 2 answer:" no-steps-2)