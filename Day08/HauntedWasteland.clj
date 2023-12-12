;; Advent of Code 2023, Day 08 - Haunted Wasteland.
;; Clojure.

(ns HauntedWasteland
  (:gen-class) 
  (:require [clojure.string :as str]))

;; Strip first and last chars off a string.
(defn remove-first-last-char [s]
  (subs s 1 (dec (count s))))

;; Strip just last char off a string.
(defn remove-last-char [s] 
  (subs s 0 (dec (count s))))

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

;; Get direction at step.
(defn get-direction-at-step [directions, n]
  (let [pos (mod (dec n) (count directions))]
    (nth directions pos)
    )
  )

;; Navigate throgh the map and return the number of steps to get to the end.
(defn navigate [step, current-location, directions, destinations-map] 
  (let [l-or-r (get-direction-at-step directions step)
        next-location-vec (get destinations-map (str ":" current-location))
        next-location (if (= l-or-r \L)
                        (nth next-location-vec 0)
                        (nth next-location-vec 1))] 
    ;;(println "step:" step "current-location:" current-location "direction:" l-or-r "next-location:" next-location)
    (if (= next-location "ZZZ")
      step
      (recur (inc step) next-location directions destinations-map))
    )
  )

;; Get all input.
(def input-file "Day08Input.txt")
(def directions (get-directions input-file))
(def destinations-map (get-destinations-map input-file))

;; Navigate through map.
(def no-steps (navigate 1 "AAA" directions destinations-map))
(println "Part 1 answer:" no-steps)
