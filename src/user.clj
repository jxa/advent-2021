(ns user)

(require '[nextjournal.clerk :as clerk])

;; start Clerk's buit-in webserver on the default port 7777, opening the browser when done
;; (clerk/serve! {:browse? true})

;; ;; either call `clerk/show!` explicitly
;; (clerk/show! "notebooks/rule_30.clj")

;; or let Clerk watch the given `:paths` for changes
(clerk/serve! {:browse? true :watch-paths ["src/xandrews"]})

;; ;; start with watcher and show filter function to enable notebook pinning
;; (clerk/serve! {:watch-paths ["notebooks" "src"] :show-filter-fn #(clojure.string/starts-with? % "notebooks")})

(defn notebook-files []
  (->> (clojure.java.io/file "src/xandrews")
       file-seq
       (filter #(.isFile %))
       (filter #(clojure.string/starts-with? (.getName %) "day"))
       (map #(.getPath %))
       sort))

(defn render-static-site
  []
  (nextjournal.clerk/build-static-app! {:paths (notebook-files) :out-path "docs"}))
