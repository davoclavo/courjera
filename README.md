# courjera
Clojure wrapper for Coursera Private API


## Examples

```clj
;; Get all your watchable videos
(with-coursera-credentials "your@email.com" "yourpassword"
  (let [sessions (enrolled-sessions)
        sections (map session->sections sessions)
        items (mapcat section->items sections)
        videos (map item->video items)]
    (map :link videos)))


;; Get all the videos from session 278
(with-coursera-credentials "your@email.com" "yourpassword"
  (let [sections (session->sections {:id 278})
        items (mapcat section->items sections)
        videos (map item->video items)]
    videos))

;; Same but shorter
(with-coursera-credentials "your@email.com" "yourpassword"
  (session->videos {:id 278}))
```
