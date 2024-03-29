(ns gitopa
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [org.httpkit.server :as http-kit])
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

(defn init-env [cfg]
  (if (:key cfg)
    {:GIT_SSH_COMMAND (format "ssh -i %s -o IdentitiesOnly=yes -o StrictHostKeyChecking=no" 
                              (if (str/starts-with? (:key cfg) "/")
                                (:key cfg)
                                (path (:key cfg))))}
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
  (let [prc (proc {:exec (:exec cfg) :dir (workdir nm)
                   :env (merge {:hook-site-name (name nm)}
                               (select-keys cfg [:hook-listener-port]))})]
    (.inheritIO prc)
    (println :start nm)
    (swap! state assoc nm (.start prc))))

(defn changed-files [nm cfg prev-comm cur-comm]
  (let [env (init-env cfg)]
    (-> (exec {:exec ["git" "diff" "--name-only" prev-comm cur-comm] :env env :dir (workdir nm)})
        (get-in [:stdout]))))

(defn restart [state nm cfg]
  (let [commit (current-commit nm cfg)
        prev-commit-file (workdir (str (name nm) ".status"))
        pcommit (when (fs/exists? prev-commit-file)
                  (slurp prev-commit-file))
        first-start? (not (get @state nm))
        update? (not (= commit pcommit))
        has-not-ignored-changed-files?
        (when (and update? (not first-start?))
          (->> (changed-files nm cfg pcommit commit)
               (remove (fn [f] (when (:re-ignore-changes cfg)
                                 (re-find (re-pattern (:re-ignore-changes cfg)) f))))
               seq))]
    (when (or first-start? update?)
      (if (or first-start? has-not-ignored-changed-files?)
        (do (println :reload nm pcommit :=> commit)
            (spit prev-commit-file commit)
            (restart-docs state nm cfg))

        (do (println :only-ignored-files-changed :no-restart)
            (spit prev-commit-file commit)
            #_"TODO: send signal to service (localhost:<port>/_reload-files) here")))))


(def lock (Object.))
(defn reconcile [state {cfgs :sites :as cfg}]
  (locking lock
    (doseq [[nm scfg] cfgs
            :let [cfg (merge cfg scfg)]]
      (try
        (when-not (fs/exists? (workdir (name nm)))
          (init-repo nm cfg))
        (update-repo nm cfg)
        (restart state nm cfg)
        (catch Exception e
          (println :error nm e))))))

(defn service-nginx [nm cfg]
  (format
   "
server {
  listen 80;
  server_name %s;
  location / {
      proxy_set_header Host $host;
      proxy_set_header X-Real-IP $remote_addr;
      proxy_pass http://localhost:%s;
  }
}
" (:host cfg) (:port cfg)))

(def events "\nevents {\n worker_connections  4096;\n}\n")
(defn generate-nginx [opts]
  (let [servers (->> (:sites opts)
                     (mapv (fn [[nm cfg]] (service-nginx nm cfg)))
                     (str/join "\n"))]
    (spit (path "workdir/nginx.config")
          (str events "\nhttp {" servers "}"))))

(defn do-loop [state cfg]
  (loop []
    (when-not (:stop @state)
      #_(generate-nginx cfg)
      (reconcile state cfg)
      (Thread/sleep (:timeout cfg))
      (recur))))

(defn start-hook-server
  [state cfg]
  (when-let [port (:hook-listener-port cfg)]
    (println :starting-hook-server-on port)
    (let [opts {:port port
                :ip (or (:hook-listener-ip cfg) "0.0.0.0")}
          server (http-kit/run-server (fn [{:keys [uri] :as req}]
                                        (println :query-on-uri  uri)
                                        (let [site-key (-> uri (subs 1) keyword)
                                              cfg-selected (update cfg :sites select-keys [site-key])]
                                          (if (seq (:sites cfg-selected))
                                            (do (reconcile state cfg-selected)
                                                {:status 200})
                                            {:status 404})))
                                      opts)]
      (println :start-hook-server opts)
      (swap! state assoc :hook-server server))))

(defn start [cfg-file]
  (println "Starting...")
  (let [state (atom {})
        cfg (edn/read-string (slurp (or cfg-file "sites.edn")))]
    (when-not (fs/exists? (path "workdir"))
      (fs/create-dir (path "workdir")))
    (future
      (try (start-hook-server state cfg)
           (do-loop state cfg)
           (println :stop)
           (catch Exception e
             (println :exception-on-start e))))
    state))

(defn stop [state]
  (swap! state assoc :stop true)
  (when-let [srv (-> @state :hook-server)]
    (srv)
    (println :hook-server-stopped)))

(defn run [cfg-file]
  (let [state (atom {})
        cfg (edn/read-string (slurp (or cfg-file "sites.edn")))]
    (when-not (fs/exists? (path "workdir"))
      (fs/create-dir (path "workdir")))
    (start-hook-server state cfg)
    (do-loop state cfg)))


(comment

  (def srv (start "sites.edn"))

  (stop srv)

  (def opts (edn/read-string (slurp "sites.edn")))

  (generate-nginx opts)
  
  (bean @srv)

  )
