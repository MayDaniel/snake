(ns snake.core
  (:import [javax.swing JPanel JFrame Timer]
           [java.awt Color Font]
           [java.awt.event KeyEvent KeyListener ActionListener]))

(def screen-width 800)
(def screen-height 600)

(def size 10) ;; for both the x and y of all objects.

(defprotocol SObject
  (body [x] "Gets sequence of vectors ([x y] ...) of it's current positions.")
  (head [x] "Gets the first vector in it's body. (body x)")
  (generate [x] "\"Respawns\" an object. (simply associates a new :body, with regenerated positions.)"))

(defprotocol SDraw
  (draw [x graphics] "Draws x's :body onto graphics."))

(defprotocol SMove
  (turn [x direction] "Associates a :direction with x.")
  (move [x] "Adds a new head to x's body, removing it's last segment.")
  (grow [x] "Adds a new head to x's body, increasing its :body count."))

(defn collision?
  "Checks whether two objects that implement the Object protocol are colliding."
  [a b]
  (some true? (for [x (:body a) y (:body b)]
                (every? #(>= 10 % -10) (map - x y)))))

(defn within-bounds?
  "Checks whether x and y and within the screen's boundaries."
  [x y]
  (and (>= (- screen-width size 20) x 0)
       (>= (- screen-height size 40) y 0)))

(defn key->xy
  "Returns a movement [x y] from a keyPressed event."
  [event]
  (condp = (.getKeyCode event)
    KeyEvent/VK_LEFT  [-5  0]
    KeyEvent/VK_RIGHT [ 5  0]
    KeyEvent/VK_UP    [ 0 -5]
    KeyEvent/VK_DOWN  [ 0  5]))

(defrecord Player [body color direction]
  SObject
  (body [_] body)
  (head [_] (first body))
  (generate [x] (assoc x :body (list (vector (/ screen-width 2) (/ screen-height 2))) :direction [-5 0]))
  SDraw
  (draw [_ graphics]
    (.setColor graphics color)
    (doseq [[x y] body] (.fillRect graphics x y size size)))
  SMove
  (turn [x e] (assoc x :direction (key->xy e)))
  (move [x] (let [head (map + (head x) direction)]
              (if (apply within-bounds? head)
                (assoc x :body (cons head (butlast body)))
                x)))
  (grow [x] (let [head (map + (head x) direction)]
              (if (apply within-bounds? head)
                (assoc x :body (cons head body))
                x))))

(defrecord Apple [body color]
  SObject
  (body [_] body)
  (head [_] (first body))
  (generate [x] (assoc x :body (list (vector (rand-int (- screen-width size 20))
                                             (rand-int (- screen-height size 40))))))
  SDraw
  (draw [_ graphics]
    (.setColor graphics color)
    (doseq [[x y] body] (.fillRect graphics x y size size))))

(def player (atom (generate (Player. nil Color/RED nil))))
(def apple (atom (generate (Apple. nil Color/GREEN))))

(defn panel []
  (proxy [JPanel KeyListener ActionListener] []
    (paintComponent [g] (proxy-super paintComponent g) (draw @player g) (draw @apple g))
    (actionPerformed [_] (if (collision? @player @apple)
                           (do (swap! player grow) (swap! apple generate))
                           (swap! player move))
                         (.repaint this))
    (keyPressed [e] (swap! player turn e))))

(defn frame []
  (doto (JFrame. "Snake")
    (.setSize screen-width screen-height) 
    (.setVisible true)))

(defn -main [& args]
  (let [frame (frame)
        panel (panel)] 
    (.setFocusable panel true) 
    (.addKeyListener panel panel)
    (.start (Timer. 75 panel))
    (.add frame panel)))