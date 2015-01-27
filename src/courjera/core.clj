(ns courjera.core
    (:require [clj-http.client :as client]
              [clojure.data.json :as json]
              [clojure.string :as string]
              [clj-time.core :as t]
              [clj-time.format :as f]
              [clojure.java.io :as io]))

;; (System/setProperty "https.proxyHost" "localhost")
;; (System/setProperty "https.proxyPort" "8080")
;; ; keytool -importcert -keystore secure.ts -storepass S3cuR3pas$! -file mitmproxy-ca-cert.cer
;; (System/setProperty "javax.net.ssl.trustStore" "/Users/davidgu/.mitmproxy/secure.ts")


(def course-catalog-url "https://d1hpa2gdx2lr6r.cloudfront.net/maestro/api/topic/list2")

(def auth-url "https://accounts.coursera.org/oauth2/v1/token")
(def auth-headers {"User-Agent" "Coursera/1277 CFNetwork/672.0.8 Darwin/14.0.0"
                   "Content-Type" "application/x-www-form-urlencoded"})

(defn auth-token [username password]
    (let [response (client/post auth-url
                 {:headers auth-headers
                  :form-params {
                    :username       username
                    :password       password
                    :grant_type     "password"
                    :client_secret  "BbgG2AgJlVj6uDghhUdRCQ"
                    :client_id      "51"}})
           response-json (json/read-str (:body response))]
    (str (get response-json "token_type") " " (get response-json "access_token"))))

(def ^:dynamic *coursera-headers* {"User-Agent" "Coursera/1277 (iPad; iOS 7.0.4; Scale/1.00)"
                                   "Authorization" nil})

(defmacro with-coursera-credentials
  "Set the credentials to be used for all contained Coursera requests."
  [username password & body]
  `(binding [*coursera-headers* {"User-Agent" "Coursera/1277 (iPad; iOS 7.0.4; Scale/1.00)"
                                 "Authorization" ~(auth-token username password)}]
     (doall
       ~@body)))


(defn course-catalog []
    (let [catalog-filename "src/courjera/course_list.json"
          read-contents #(json/read-str % :key-fn keyword)
          file-exists (.exists (io/as-file catalog-filename))]
      (if file-exists
        ;; FIXME david doesnt like to call read-contents twice
        (read-contents (slurp catalog-filename))
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

;; FIXME -
(def enroll-url "https://api.coursera.org/api/users/v1/me/enrollments")
(defn enroll [session-id]
  (let [enroll! (fn []
                 (client/post enroll-url
                              {:headers *coursera-headers*
                               :form-params {"sessionId" session-id}
                               :content-type :json}))]
    (try (enroll!)
      (catch Exception e (println (str "Error enrolling in course with session id:" session-id e))))))

(defn enroll-all-active-courses []
  (pmap enroll (active-course-ids)))


(defn coursera-request
  "Hits Coursera API given a URL"
  [url]
  (let [contents ((client/get url {:headers *coursera-headers*}) :body)]
    (json/read-str contents :key-fn keyword)))

(defmacro generate-coursera-endpoint-fns
  "Defines functions that hit Coursera API according to the endpoints map"
  []
  (let [base-url "https://api.coursera.org/api/"
        endpoints {:enrollments "users/v1/me/enrollments"
                   :sections "sessions/v1/:session-id/sections"
                   :items "sessions/v1/:session-id/sections/:section-id/items"}]
    (cons `do
          (for [[endpoint url] endpoints]
            (let [url-placeholders (re-seq #":[^\/]+" url)
                  url-params (->> url-placeholders
                                  (map #(.replaceAll % ":" ""))
                                  (map symbol))
                  fn-name (str "coursera-" (name endpoint))]

              `(defn ~(symbol fn-name)
                 ~(str fn-name " returns the endpoint for " endpoint " given: " (string/join " and " url-params))
                 [~@url-params]
                 (let [substitutions# (map vector [~@url-placeholders] [~@url-params])
                       parametrized-endpoint-url# (reduce (fn [url# [placeholder# value#]]
                                                                (.replaceAll url# placeholder# (str value#)))
                                                          ~url
                                                          substitutions#)
                       full-url# (str ~base-url parametrized-endpoint-url#)]
                   (coursera-request full-url#))))))))

(generate-coursera-endpoint-fns)
;(macroexpand '(generate-coursera-endpoint-fns))

(defn enrollment->session
  "Transform a enrollment into a session, does not hit API"
  ;; {
  ;;   :id 105071078,
  ;;   :sessionId 278,
  ;;   :isSigTrack false,
  ;;   :courseId 93,
  ;;   :active true,
  ;;   :startDate 1384128000,
  ;;   :endDate 1389571200,
  ;;   :startStatus "Past"
  ;; }
  ;; ->
  ;; {
  ;;    :id 278
  ;; }
  [enrollment]
  (hash-map :id (:sessionId enrollment)))

(defn enrolled-sessions
  "Get a list of enrolled sessions"
  []
  (let [enrollments (:enrollments (coursera-enrollments))]
    (map enrollment->session enrollments)))

(defn session->sections
  "Transform a session into sections, hits API"
  ;; {
  ;;    :id 278
  ;; }
  ;; ->
  ;; [{
  ;;     :displayOrder 1,
  ;;     :id 2,
  ;;     :lastPublishedDate 0,
  ;;     :sessionId "278",
  ;;     :title "Week 1: Nanotechnology Introduction",
  ;;     :uid "278.section.2",
  ;;     :visibleDate -1000
  ;;  }
  ;;  ...]
  [session]
  ;; {:pre [(m/valid-session? session)]
  ;; :post [(map m/valid-section? %)]}
  (:sections (coursera-sections (:id session))))

(defn section->items
  "Transform a section into items, hits API"
  ;; {
  ;;    :id 2,
  ;;    :sessionId "278",
  ;; }
  ;; ->
  ;; [{
  ;;     :displayOrder 1,
  ;;     :id 2,
  ;;     :lastPublishedDate 0,
  ;;     :sessionId "278",
  ;;     :title "Week 1: Nanotechnology Introduction",
  ;;     :uid "278.section.2",
  ;;     :visibleDate -1000
  ;;  }
  ;;  ...]
  [section]
  ;{:pre [(m/valid-section? section)]
  ; :post [(map m/valid-item? %)]}
  (:items (coursera-items (:sessionId section) (:id section))))

(defn item->video
  "Transform an item into videos, does not hit API"
  [item]
  ;{:pre [(m/valid-item? item)]
  ; :post [(m/valid-video? %)]}
  (let [is-lecture? (= "lecture" (get item :itemType))
        video-nested-keywords [:metadata :media :normal_x264]
        sert-nested-keywords [:metadata :srtUrls :en]]
    (if is-lecture?
      {:link (get-in item video-nested-keywords)
       :srt (get-in item sert-nested-keywords)})))

(defn session->videos
  "Transform a session into videos, hits API"
  [session]
  (let [sections (session->sections session)
        items (mapcat section->items sections)]
    (mapcat item->video items)))
