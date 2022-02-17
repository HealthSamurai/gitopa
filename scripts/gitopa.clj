(ns gitopa
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [babashka.fs :as fs]
            [clojure.string :as str])
  (:import [java.lang ProcessBuilder]))

(def home (System/getProperty "user.dir"))

(defn path [path]
  (str home "/" (name path) ))

(defn workdir [nm]
  (path (str "workdir/" (name nm))))

(defn read-stream [s]
  (let [r (io/reader s)]
    (loop [acc []]
      (if-let [l (.readLine r)]
        (recur (conj acc l))
        acc))))

(defn proc [{dir :dir env :env args :exec}]
  (let [proc (ProcessBuilder. (into-array String args))
        _ (when dir (.directory proc (io/file dir)))
        _ (when env
            (let [e (.environment proc)]
              #_(.clear e)
              (doseq [[k v] env]
                (.put e (name k) (str v)))))]
    proc))

(defn exec [{dir :dir env :env args :exec :as opts}]
  (let [prc (proc opts)
        p (.start prc)]
    (.waitFor p)
    {:status (.exitValue p)
     :stdout (read-stream (.getInputStream p))
     :stderr (read-stream (.getErrorStream p))}))

(defn exec! [args]
  (let [res (exec args)]
    (when-not (= 0 (:status res))
      (throw (Exception. ^String (str  (:stderr res)))))))

(defn run [opts]
  (let [prc (proc opts)]
    (.start prc)))

(defn init-env [cfg]
  (if (:key cfg)
    {:GIT_SSH_COMMAND (format "ssh -i %s -o IdentitiesOnly=yes -o StrictHostKeyChecking=no" (path (:key cfg)))}
    {}))

(defn init-repo [nm cfg]
  (let [env (init-env cfg)]
    (println :init/repo nm (:repo cfg))
    (exec! {:env env :exec ["git" "clone" (:repo cfg) (workdir nm)]})))

(defn current-commit [nm cfg]
  (let [env (init-env cfg)]
    (-> (exec {:exec ["git" "rev-parse" "HEAD"] :env env :dir (workdir nm)})
        (get-in [:stdout 0]))))

(defn update-repo [nm cfg]
  (let [env (init-env cfg)
        wd (workdir nm)]
    (print ".")
    (flush)
    (exec! {:env env :exec ["git" "pull" "--ff-only"] :dir wd})
    (exec! {:env env :exec ["git" "submodule" "init"] :dir wd})
    (exec! {:env env :exec ["git" "submodule" "update" "--recursive"] :dir wd})))

(defn restart-docs [state nm cfg]
  (when-let [prc (get @state nm)]
    (println :stop nm)
    (.destroy prc))
  (let [prc (proc {:exec (:exec cfg) :dir (workdir nm)})]
    (.inheritIO prc)
    (println :start nm)
    (swap! state assoc nm (.start prc))))

(defn restart [state nm cfg]
  (let [commit (current-commit nm cfg)
        prev-commit-file (workdir (str (name nm) ".status"))
        pcommit (when (fs/exists? prev-commit-file)
                      (slurp prev-commit-file))]
    (when (or (not (= commit pcommit))
              (not (get @state nm)))
      (println :reload nm pcommit :=> commit)
      (spit prev-commit-file commit)
      (restart-docs state nm cfg))))

(defn reconcile [state {cfgs :sites}]
  (doseq [[nm cfg] cfgs]
    (try 
      (when-not (fs/exists? (workdir (name nm)))
        (init-repo nm cfg))
      (update-repo nm cfg)
      (restart state nm cfg)
      (catch Exception e
        (println :error nm (.getMessage e))))))

(defn service-nginx [nm cfg]
  (format
   "
server {
  listen 80
  server_name %s;
  location / {
      proxy_set_header Host $host;
      proxy_set_header X-Real-IP $remote_addr;
      proxy_pass http://localhost:%s;
  }
}
" (:host cfg) (:port cfg)))

(defn generate-nginx [opts]
  (let [servers (->> (:sites opts)
                     (mapv (fn [[nm cfg]] (service-nginx nm cfg)))
                     (str/join "\n"))]
    (spit (path "workdir/nginx.config") (str "http {" servers "}"))))

(defn do-loop [state cfg-file]
  (loop []
    (when-not (:stop @state)
      (let [cfg (edn/read-string (slurp (or cfg-file "sites.edn")))]
        (generate-nginx cfg)
        (reconcile state cfg)
        (Thread/sleep (:timeout cfg)))
      (recur))))

(defn start [cfg-file]
  (println "Starting...")
  (let [state (atom {})]
    (when-not (fs/exists? (path "workdir"))
      (fs/create-dir (path "workdir")))
    (future 
      (do-loop state cfg-file)
      (println "Stop"))
    state))

(defn stop [state]
  (swap! state assoc :stop true))

(defn run [cfg-file]
  (let [state (atom {})]
    (when-not (fs/exists? (path "workdir"))
      (fs/create-dir (path "workdir")))
    (do-loop state cfg-file)))


(comment

  (def srv (start "sites.edn"))

  (stop srv)

  (def opts (edn/read-string (slurp "sites.edn")))

  (generate-nginx opts)
  
  (bean @srv)

  )
