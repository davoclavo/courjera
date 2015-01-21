(ns courjera.core
    (:require [clj-http.client :as client]
              [clojure.data.json :as json]
              [clojure.string :as string]
              [clj-time.core :as t]
              [clj-time.format :as f]
              [courjera.models :as m]))

(def login-url "https://accounts.coursera.org/oauth2/v1/token")
(def login-headers {
  "User-Agent" "Coursera/1277 CFNetwork/672.0.8 Darwin/14.0.0"
  "Content-Type" "application/x-www-form-urlencoded"})

(def enroll-url "https://api.coursera.org/api/users/v1/me/enrollments")
(def enroll-headers {
  "User-Agent" "Coursera/1277 (iPad; iOS 7.0.4; Scale/1.00)"
  "Authorization" (auth-token "martinolantino@suremail.info" "password")})

(def course-catalog-url "https://d1hpa2gdx2lr6r.cloudfront.net/maestro/api/topic/list2")

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


(defn course-catalog []
    (let [catalog-filename "src/courjera/course_list.json"
          read-contents #(json/read-str % :key-fn keyword)
          contents (slurp catalog-filename)]
      (if contents
        ;; FIXME david doesnt like to call read-contents twice
        (read-contents contents)
        (let [contents ((client/get course-catalog-url) :body)]
          (spit catalog-filename contents)
          (read-contents contents)))))

(def multi-parser (f/formatter (t/default-time-zone) "YYYY-MM-dd" "YYYY/MM/dd"))

(defn active? [course]
  {:pre [(:end_date course)]}
  (let [course-end-date (f/parse (f/formatters :date) (:end_date course))
        not-finished? (t/before? (t/today-at 23 59) course-end-date)
        active? (:active course)]
        (and not-finished? active?)))

;; FIXME - Figure out how to find an active course
(defn active-course-ids []
    (let [courses (:courses (course-catalog))
          active-courses (->> courses
                              (filter :end_date)
                              (filter active?))]
      (map :id active-courses)))

(defn enroll [session-id]
  (let [enroll! (fn []
                 (client/post enroll-url
                              {:headers enroll-headers
                               :form-params {"sessionId" session-id}
                               :content-type :json}))]
    (try (enroll!)
      (catch Exception e (println (str "Error enrolling in course with session id:" session-id e))))))

(defn enroll-all-active-courses []
  (pmap enroll (active-course-ids)))

(defn enrolled-sessions [] ())

;; Fetch list of enrolled courses
;; GET https://api.coursera.org/api/users/v1/me/enrollments())

(defn session->sections
  [session]
  {:pre [(m/valid-session? session)]
   :post [(map m/valid-section? %)]}
  ())

(defn section->items
  [section]
  {:pre [(m/valid-section? section)]
   :post [(map m/valid-item? %)]}

  ())

(defn item->video
  [item]
  {:pre [(m/valid-item? item)]
   :post [(m/valid-video? %)]}

  ())

(defn session->videos [session]
  (let [sections (session->sections session)
        items (mapcat section->items sections)]
    (mapcat item->video items)))




(defmacro generate-coursera-endpoint-fns []
  (let [base-url "https://api.coursera.org/api/sessions/v1/"
        endpoints {:enrollments "me/enrollments"
                   ;; GET https://api.coursera.org/api/sessions/v1/974416/sections
                   :sections ":session-id/sections"
                   ;; GET https://api.coursera.org/api/sessions/v1/974416/sections/1/items
                   :items ":session-id/sections/:section-id/items"}]
    (cons `do
          (for [[endpoint url] endpoints]
            (let [url-placeholders (re-seq #":[^\/]+" url)
                  url-params (->> url-placeholders
                                  (map #(.replaceAll % ":" ""))
                                  (map symbol))
                  fn-name (str "coursera-" (name endpoint) "-url")]

              `(defn ~(symbol fn-name)
                 ~(str fn-name " returns the endpoint for " endpoint " given: " (string/join " and " url-params))
                 [~@url-params]
                 (let [substitutions# (map vector [~@url-placeholders] [~@url-params])
                       parametrized-endpoint-url# (reduce (fn [url# [placeholder# value#]]
                                                                (.replaceAll url# placeholder# value#))
                                                          ~url
                                                          substitutions#)]
                   (str ~base-url parametrized-endpoint-url#))))))))

(macroexpand '(generate-coursera-endpoint-fns))
(generate-coursera-endpoint-fns)
(coursera-items-url "341" "12")
