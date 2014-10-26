(ns lab01.core
  (:gen-class))
(use 'clojure.java.io)

(def Ra 3)
(def Rb (* 1.5 Ra))
(def EpsHigh 0.5)
(def EpsLow 0.15)

(defn parse-number
  [s]
  (if (re-find #"^-?\d+\.?\d*$" s)
    (read-string s)))

(defn parse-string
  [inputStr]
  (->> (clojure.string/split inputStr #",")
       (map clojure.string/trim)
       (butlast)
       (map parse-number)))

(defn euclidian-distance
  [c1 c2]
  (->> (map - c1 c2) (map #(* % %)) (reduce +)))

(defn hamming-distance
  [c1 c2]
  (count (filter true? (map not= c1 c2))))

(defn not-nil?
  [x]
  (not (nil? x)))

(defn remove-nils
  [col]
  (filter not-nil? col))

(defn create-point
  [elem]
  {:coordinates (into [] elem)})

(defn create-point-with-dist
  [point dist]
  (assoc point :dist dist))

(defn read-coordinates
  [fileName]
  (let [content (slurp fileName)]
    (reduce (fn [collection elem]
              (let [parsed-string (remove-nils (parse-string elem))]
                  (conj collection (create-point parsed-string))))
            [] (clojure.string/split-lines content))))

(defn potential
  [distance]
  (Math/exp (- (* (/ 4 (* Ra Ra)) distance))))

(defn revised-potential
  [distance]
  (Math/exp (- (* (/ 4 (* Rb Rb)) distance))))

(defn calculate-point-potential
  [point points distance-calculator]
  (create-point-with-dist point (reduce #(+ %1 (potential (distance-calculator (:coordinates point) (:coordinates %2)))) 0 points)))

(defn calculate-potentials
  [points distance-calculator]
  (map #(calculate-point-potential %1 points distance-calculator) points))

(defn find-max
  [points]
  (loop [max (:dist (first points)) index 0 cnt (dec (count points))]
    (let [currentMax (:dist (nth points cnt))]
      (if (= cnt 0)
         (nth points index)
      (recur (if (< max currentMax) currentMax max) (if (< max currentMax) cnt index) (dec cnt))))))

(defn revise-potential
  [point kernel distance-calculator]
  (assoc point :dist (- (:dist point) (* (:dist kernel) (revised-potential (distance-calculator (:coordinates point) (:coordinates kernel)))) )))

(defn revise-point-potentials
  [points kernel distance-calculator]
  (->> (map #(revise-potential %1 kernel distance-calculator) points)
       (sort-by #(- (:dist %1)))))

(defn calculate-min-distance
  [point points distance-calculator]
  (->> (map #(distance-calculator (:coordinates point) (:coordinates %1)) points)
       (apply min)))

(defn run-clusterization
  [points distance-calculator]
  (let [initialPotentials
        (->> (calculate-potentials points distance-calculator)
             (sort-by #(- (:dist %1))))]
    (let [firstKernel (first initialPotentials)]
      (loop [kernels [firstKernel] elements (rest initialPotentials)]
        (let [revisedPoints (revise-point-potentials elements (first kernels) distance-calculator)]
          (let [newKernel (first revisedPoints)]
            (cond
              (> (:dist newKernel) (* EpsHigh (:dist firstKernel))) (recur (cons newKernel kernels) (rest revisedPoints))
              (< (:dist newKernel) (* EpsLow (:dist firstKernel))) (sort-by #(- (:dist %1)) kernels)
              (>= (+ (/ (calculate-min-distance newKernel kernels distance-calculator) Ra) (/ (:dist newKernel) (:dist firstKernel))) 1) (recur (cons newKernel kernels) (rest revisedPoints))
              :else (recur kernels (cons (assoc newKernel :dist 0) (rest revisedPoints)))) )   ))                    )))

(defn -main
  [& args]
  (if (>= (count args) 2)
    (let [points (read-coordinates (first args))
          distance (if (= (last args) "hamming") hamming-distance euclidian-distance)]
      (run-clusterization points distance))
  (println "Not enough arguments specified")))