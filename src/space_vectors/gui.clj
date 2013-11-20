(ns space-vectors.gui
  (:gen-class)
  (:require [space-vectors.core :refer [parse]]
            [seesaw.core :refer [text text! scrollable listen frame native! invoke-later pack! show! border-panel]]))

(def !history (atom ()))
(def !index (atom 0))

(def enter-key 10)
(def up-arrow 38)
(def down-arrow 40)

(def user-input (text))
(def output-box (text :multi-line? true :editable? false))
(def output-scroll (scrollable output-box))

(listen user-input
        :key-pressed
        (fn [e]
          (condp = (.getKeyCode e)

            enter-key
            (let [in (text user-input)
                  {:keys [input result]} (parse in)]
              (text! output-box
                     (str (text output-box)
                          "> " input
                          "\n" result "\n"))
              (when input
                (text! user-input "")
                (swap! !history (comp distinct conj) in)
                (reset! !index 0)))

            up-arrow (do
                       (text! user-input (nth @!history @!index))
                       (when (< @!index (dec (count @!history)))
                         (swap! !index inc)))

            down-arrow (do
                         (if (= 0 @!index)
                           (text! user-input "")
                           (do
                             (swap! !index dec)
                             (text! user-input (nth @!history @!index)))))
            
            nil)))

(defn -main [& args]
  (native!)
  (invoke-later
   (-> (frame :title "Space Vectors"
              :size [640 :by 480]
              :content (border-panel
                        :center output-scroll
                        :south user-input
                        :vgap 5 :hgap 5 :border 5)
              :on-close :exit)
       pack!
       show!)))
