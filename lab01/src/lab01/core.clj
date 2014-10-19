(ns lab01.core
  (:gen-class))
(use 'clojure.java.io)

(def Ra 3)
(def Rb (* 1.5 Ra))
(def EpsHigh 0.5)
(def EpsLow 0.15)

(defn average
  [numbers]
  (/ (apply + numbers) (count numbers)))

(defn parse-number
  [s]
  (if (re-find #"^-?\d+\.?\d*$" s)
    (read-string s)))

(defn parse-string
  [inputStr]
  (->> (clojure.string/split inputStr #",")
       (map clojure.string/trim)
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
  {:coordinates (:coordinates point) :dist dist})

(defn read-coordinates
  [fileName]
  (let [content (slurp fileName)]
    (->> (map parse-string (clojure.string/split-lines content))
         (map remove-nils)
         (filter not-empty)
         (map create-point))))

(defn potential
  [distance]
  (Math/exp (- (* (/ 4 (* Ra Ra)) distance))))

(defn revised-potential
  [distance]
  (Math/exp (- (* (/ 4 (* Rb Rb)) distance))))

(defn calculate-point-potential
  [point points]
  (loop [dist 0 cnt (dec (count points))]
    (if (= cnt -1)
      (create-point-with-dist point dist)
     (recur (+ dist (potential (euclidian-distance (:coordinates point) (:coordinates (nth points cnt))))) (dec cnt)))))

(defn calculate-potentials
  [points]
  (map #(calculate-point-potential %1 points) points))

(defn find-max
  [points]
  (loop [max (:dist (first points)) index 0 cnt (dec (count points))]
    (let [currentMax (:dist (nth points cnt))]
      (if (= cnt 0)
         (nth points index)
      (recur (if (< max currentMax) currentMax max) (if (< max currentMax) cnt index) (dec cnt))))))

(defn revise-potential
  [point kernel]
  {:coordinates (:coordinates point) :dist (- (:dist point) (* (:dist kernel) (revised-potential (euclidian-distance (:coordinates point) (:coordinates kernel)))) )})

(defn revise-point-potentials
  [points kernel]
  (map #(revise-potential %1 kernel) points))

(defn calculate-min-distance
  [point points]
  (->> (map #(euclidian-distance (:coordinates point) (:coordinates %1)) points)
       (apply min)))

(defn run-clusterization
  [points]
  (let [initialPotentials
        (->> (calculate-potentials points)
             (sort-by #(:dist %1)))]
    (let [firstKernel (last initialPotentials)]
      (loop [kernels [firstKernel] elements (take (dec (count initialPotentials)) initialPotentials)]
        (let [revisedPoints (revise-point-potentials elements (last kernels))]
          (let [newKernel (last revisedPoints)
                minDist (calculate-min-distance newKernel kernels)]
            (if (> (:dist newKernel) (* EpsHigh (:dist firstKernel)))
              (recur (conj kernels newKernel) (take (dec (count revisedPoints)) revisedPoints))
            (if (< (:dist newKernel) (* EpsLow (:dist firstKernel)))
              kernels
            (if (>= (+ (/ minDist Ra) (/ (:dist newKernel) (:dist firstKernel))) 1)
              (recur (conj kernels newKernel) (take (dec (count revisedPoints)) revisedPoints))
            (recur kernels (conj (take (dec (count revisedPoints)) revisedPoints) {:coordinates (:coordinates newKernel) :dist 0})))) ))   ))                    )))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println args)
  (let [points (read-coordinates "бабочка.txt")]
    (run-clusterization points)
    #_(->> (calculate-potentials points)
         (sort-by #(:dist %1)))))
(-main "butterfly.txt" "hamming")