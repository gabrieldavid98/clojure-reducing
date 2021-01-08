(ns clojure-reducing.core
  (:require
   [clojure.java.io :as io]
   [clojure.data.csv :as csv]
   [semantic-csv.core :as sc])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; Simple reducing
(def simple-reducing (reduce + [8 4000 10 300]))

;; Internally the function + when called with more than
;; 2 arguments uses a version of reduce  
(def simple-reducing-2 (apply + [8 4000 10 300]))

;; ============================================
;; Finding the Day with the Maximum Temperature

(def weather-days
  [{:max 31
    :min 27
    :description :sunny
    :date "2019-09-24"}
   {:max 28
    :min 25
    :description :cloudy
    :date "2019-09-25"}
   {:max 22
    :min 18
    :description :rainy
    :date "2019-09-26"}
   {:max 23
    :min 16
    :description :stormy
    :date "2019-09-27"}
   {:max 35
    :min 19
    :description :sunny
    :date "2019-09-28"}])

;; We need to be able to write functions that return the entire map for the day 
;; with the highest maximum temperature, the lowest minimum, and so on

(defn max-temp []
  (reduce (fn [max-day-so-far this-day]
            (if (> (:max this-day) (:max max-day-so-far))
              this-day
              max-day-so-far))
          weather-days))

(defn min-max-temp []
  (reduce (fn [min-max-day-so-far this-day]
            (if (< (:max this-day) (:max min-max-day-so-far))
              this-day
              min-max-day-so-far))
          weather-days))

;; ============================================

(defn min-and-max []
  (reduce (fn [{:keys [minimum maximum]} new-number]
            {:minimum (if (and minimum (> new-number minimum))
                        minimum
                        new-number)
             :maximum (if (and maximum (< new-number maximum))
                        maximum
                        new-number)})
          {} ;; Init value
          [5 23 5004 845 22]))

;; Partitioning
;; (partition 3 [1 2 3 4 5 6 7 8 9 10])
;; (partition-all 3 [1 2 3 4 5 6 7 8 9 10])

(defn segment-by-sum [limit ns]
  (let [result (reduce (fn [{:keys [segments current] :as acc} n]
                         (let [current-with-n (conj current n)
                               total-with-n (apply + current-with-n)]
                           (if (> total-with-n limit)
                             (assoc acc
                                    :segments (conj segments current)
                                    :current [n])
                             (assoc acc :current current-with-n))))
                       {:segments [] :current []}
                       ns)]
    (conj (:segments result) (:current result))))

;; =============================================================================

(def numbers [4 9 2 3 7 9 5 2 6 1 4 6 2 3 3 6 1])

(defn parity-totals [ns]
  (:ret
   (reduce (fn [{:keys [current] :as acc} n]
             (if (and (seq current)
                      (or (and (odd? (last current)) (odd? n))
                          (and (even? (last current)) (even? n))))
               (-> acc
                   (update :ret conj [n (apply + current)])
                   (update :current conj n))
               (-> acc
                   (update :ret conj [n 0])
                   (assoc :current [n]))))
           {:current [] :ret []}
           ns)))

;; Measuring Elevation Differences on Slopes
;; =========================================

(def distance-elevation
  [[0 400]
   [12.5 457]
   [19 622]
   [21.5 592]
   [29 615]
   [35.5 892]
   [39 1083]
   [43 1477]
   [48.5 1151]
   [52.5 999]
   [57.5 800]
   [62.5 730]
   [65 1045]
   [68.5 1390]
   [70.5 1433]
   [75 1211]
   [78.5 917]
   [82.5 744]
   [84 667]
   [88.5 860]
   [96 671]
   [99 584]
   [108 402]
   [115.5 473]])

(defn same-slope-as-current? [current elevation]
  (or (= 1 (count current))
      (let [[[_ next-to-last] [_ the-last]] (take-last 2 current)]
        (or (>= next-to-last the-last elevation)
            (<= next-to-last the-last elevation)))))

(defn distances-elevation-to-next-peak-or-valley
  [data]
  (->
   (reduce (fn [{:keys [current] :as acc}
                [distance elevation :as this-position]]
             (cond (empty? current)
                   {:current [this-position]
                    :calculated [{:race-position distance
                                  :elevation elevation
                                  :distance-to-next 0
                                  :elevation-to-next 0}]}
                   (same-slope-as-current? current elevation)
                   (-> acc
                       (update :current conj this-position)
                       (update :calculated conj
                               {:race-position distance
                                :elevation elevation
                                :distance-to-next (- (first (first current)) distance)
                                :elevation-to-next (- (second (first current)) elevation)}))
                   :else
                   (let [[prev-distance
                          prev-elevation
                          :as peak-or-valley] (last current)]
                     (-> acc
                         (assoc :current [peak-or-valley this-position])
                         (update :calculated conj
                                 {:race-position distance
                                  :elevation elevation
                                  :distance-to-next (- prev-distance distance)
                                  :elevation-to-next (- prev-elevation elevation)})))))
           {:current []
            :calculated []}
           (reverse data))
   :calculated
   reverse))

;; =============================================================================
;; Reducing without reduce

(def matches
  [{:winner-name "Kvitova P."
    :loser-name "Ostapenko J."
    :tournament "US Open"
    :location "New York"
    :date "2016-08-29"}
   {:winner-name "Kvitova P."
    :loser-name "Buyukakcay C."
    :tournament "US Open"
    :location "New York"
    :date "2016-08-31"}
   {:winner-name "Kvitova P."
    :loser-name "Svitolina E."
    :tournament "US Open"
    :location "New York"
    :date "2016-09-02"}
   {:winner-name "Kerber A."
    :loser-name "Kvitova P."
    :tournament "US Open"
    :location "New York"
    :date "2016-09-05"}
   {:winner-name "Kvitova P."
    :loser-name "Brengle M."
    :tournament "Toray Pan Pacific Open"
    :location "Tokyo"
    :date "2016-09-20"}
   {:winner-name "Puig M."
    :loser-name "Kvitova P."
    :tournament "Toray Pan Pacific Open"
    :location "Tokyo"
    :date "2016-09-21"}])

;; Lookup table
;; to get values simply use (get matches-by-date "The-date")
(def matches-by-date (zipmap (map :date matches) matches))

(def dishes
  [{:name "Carrot Cake"
    :course :dessert}
   {:name "French Fries"
    :course :main}
   {:name "Celery"
    :course :appetizer}
   {:name "Salmon"
    :course :main}
   {:name "Rice"
    :course :main}
   {:name "Ice Cream"
    :course :dessert}])

;; Group data by :course
;; (group-by :course dishes)
;; =============================================================================
;; 
;; Quick Summary Statistics with group-by

(def csv "resources/match_scores_1968-1990_unindexed_csv.csv")

(defn tennis-csv->tournament-match-counts [csv]
  (with-open [r (io/reader csv)]
    (->> (csv/read-csv r)
         sc/mappify
         (group-by :tourney_slug)
         (map (fn [[k ms]] [k (count ms)]))
         (into {}))))

(def tournaments-totals (tennis-csv->tournament-match-counts csv))

;; =============================================================================
;;
;; Complex Accumulation with reduce

(defn win-loss-by-player [csv]
  (with-open [r (io/reader csv)]
    (->> (csv/read-csv r)
         sc/mappify
         (reduce (fn [acc {:keys [winner_slug loser_slug]}]
                   (-> acc
                       (update-in [winner_slug :wins] 
                                  (fn [wins] (inc (or wins 0))))
                       (update-in [loser_slug :losses]
                                  (fn [losses] (inc (or losses 0))))))
                 {}))))
