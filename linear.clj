(defn v+ [x y] (mapv + x y))
(defn v- [x y] (mapv - x y))
(defn v* [x y] (mapv * x y))

(defn v*s
  [x, & y] (if y (mapv (fn [x1] (* x1 (reduce * y))) x) x))

(defn scalar [x, y] (reduce + (v* x y)))

(defn getCoordinate [x y i j] (- (* (x i) (y j)) (* (x j) (y i))))
(defn twoVector [x y] [(getCoordinate x y 1 2) (getCoordinate x y 2 0) (getCoordinate x y 0 1)])
(defn vect [& x] (reduce twoVector x))

(defn m+ [x, y] (mapv v+ x y))
(defn m- [x, y] (mapv v- x y))
(defn m* [x, y] (mapv v* x y))

(defn m*s [x & y] (if y (mapv (fn [x1] (v*s x1 (reduce * y))) x) x))

(defn m*v [x, y] (mapv (fn [x1] (scalar x1 y)) x))

(defn nthCol [x n] (mapv (fn [xi] (xi n)) x))
(defn transpose [x] (into [] (for [i (range (count (x 0)))] (nthCol x i))))

(defn twoMatrix [x, y] (mapv (fn [xi] (mapv (fn [yi] (scalar xi yi)) (transpose y))) x))
(defn m*m [& x] (reduce twoMatrix x))


(defn s+ [x, y] (cond (vector? x) (mapv s+ x y) :else (+ x y)))

(defn s- [x, y] (cond (vector? x) (mapv s- x y) :else (- x y)))

(defn s* [x, y] (cond (vector? x) (mapv s* x y) :else (* x y)))
