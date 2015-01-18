(ns courjera.core
    (:require [clj-http.client :as client]
              [clojure.data.json :as json]
              [clj-time.core :as t]
              [clj-time.format :as f]))

(def login-url "https://accounts.coursera.org/oauth2/v1/token")
(def login-headers {
  "User-Agent" "Coursera/1277 CFNetwork/672.0.8 Darwin/14.0.0"
  "Content-Type" "application/x-www-form-urlencoded"})



(defn auth-token [username, password]
    (let [response (client/post login-url
                 {:headers login-headers
                  :form-params {
                    :username       username
                    :password       password
                    :grant_type     "password"
                    :client_secret  "BbgG2AgJlVj6uDghhUdRCQ"
                    :client_id      "51"}})
           response-json (json/read-str (:body response))]
    (str (get response-json "token_type") " " (get response-json "access_token"))))

(def enroll-url "https://api.coursera.org/api/users/v1/me/enrollments")
(def enroll-headers {
  "User-Agent" "Coursera/1277 (iPad; iOS 7.0.4; Scale/1.00)"
  "Authorization" (auth-token "martinolantino@suremail.info" "password")
    })


(defn course-ids []
    (let [contents (slurp "src/courjera/course_list.json")
          contents-map (json/read-str contents :key-fn keyword)
          courses (:courses contents-map)]
      (map :id courses)))

(def multi-parser (f/formatter (t/default-time-zone) "YYYY-MM-dd" "YYYY/MM/dd"))

(defn active? [course]
  {:pre [(:end_date course)]}
  (let [course-end-date (f/parse (f/formatters :date) (:end_date course))
        not-finished? (t/before? (t/today-at 23 59) course-end-date)
        active? (:active course)]
        ; course-end-date))
        (and not-finished? active?)))

(defn active-course-ids []
    (let [contents (slurp "src/courjera/course_list.json")
          contents-map (json/read-str contents :key-fn keyword)
          courses (:courses contents-map)
          active-courses (->> courses
                              (filter :end_date)
                              (filter active?))]
      (map :id active-courses)))

(defn enroll [course-id]
  (let [enroll! (fn []
                 (client/post enroll-url
                              {:headers enroll-headers
                               :form-params {"sessionId" course-id}
                               :content-type :json}))]
    (try (enroll!)
      (catch Exception e (println (str "Error enrolling in course with id:" course-id e))))))

(defn enroll-all-active-courses []
  (pmap enroll (active-course-ids)))
