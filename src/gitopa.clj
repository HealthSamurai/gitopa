(ns gitopa
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [cmd]))

(defn get-root [ztx]
  (:gitopa-dir @ztx))

(defn state-dir [ztx branch & pth]
  (str (get-root ztx) "/gitopa/" branch (if pth (str "/" (str/join "/" pth)) "")))

(defn branch-dir [ztx branch & pth]
  (str (get-root ztx) "/" branch (if pth (str "/" (str/join "/" pth)) "")))

(defn lc-file [ztx branch]
  (state-dir ztx branch "last-commit"))

(defn rid-file [ztx branch]
  (state-dir ztx branch "run-id"))

(defn repo-dir [ztx]
  (str (get-root ztx) "/.repo"))

(defn set-commit [ztx branch c]
  (spit (lc-file ztx branch) c))

(defn read-config [ztx]
  (read-string (slurp (str (get-root ztx) "/gitopa/gitopa.edn"))))

(defn get-config [ztx]
  (:gitopa @ztx))

(defn clone-branch [ztx brn & [repo]]
  (when-not (cmd/exists? (branch-dir ztx brn))
    (cmd/$> `[git clone -b ~brn ".repo" ~brn] {:dir (get-root ztx)})
    (when repo
      (cmd/$> `[git remote add src ~repo] {:dir (branch-dir ztx brn)}))))

(defn pwd []
  (.getParent (io/file (System/getProperty "user.dir"))))

(defn init [repo & [dir]]
  (let [dir (or dir (pwd))]
    (cmd/$> `[mkdir -p ~dir])
    (when-not (cmd/exists? dir ".repo")
      (cmd/$> `[git clone ~repo --mirror ".repo"] {:dir dir}))
    (cmd/$> '[git remote update] {:dir (str dir "/.repo")})
    (when-not (cmd/exists? dir "gitopa")
      (clone-branch (atom {:gitopa-dir dir}) "gitopa" repo))
    (cmd/$> '[git pull] {:dir (str dir "/gitopa")})
    (let [f (io/file (str dir "/gitopa/gitopa.edn"))]
      (if (.exists f)
        (let [cfg (read-string (slurp f))]
          (println :config cfg))
        (println :error "No gitopa.edn file")))))


(defn init-branches [ztx]
  (doseq [br (:branches (get-config ztx))]
    (let [brn (name br)]
      (cmd/$> `[mkdir -p ~(state-dir ztx brn "queue")])
      (cmd/$> `[mkdir -p ~(state-dir ztx brn "runs")])
      (spit (state-dir ztx brn ".gitignore") "queue")
      (let [lc-file (state-dir ztx brn "last-commit")]
        (when-not (cmd/exists? lc-file)
          (set-commit ztx brn (str/trim (cmd/$> `[git log -b ~brn -n 1 "--pretty=%H" ~brn] {:dir (repo-dir ztx)})))))
      (let [id-file (state-dir ztx brn "run-id")]
        (when-not (cmd/exists? id-file)
          (spit id-file "0")))
      (clone-branch ztx brn))))

(defn parse-commit [s branch]
  (assoc (zipmap [:commit :message :user :email :date] (mapv str/trim (str/split s #"¦"))) :branch branch))

(defn changes [ztx branch from to]
  (->> 
   (cmd/lns (cmd/$> `[git diff ~branch --name-status ~from ~to] {:dir (repo-dir ztx)}))
   (remove str/blank?)
   (reduce (fn [acc l]
             (let [[ch f] (str/split l #"\t" 2)]
               (assoc-in acc (str/split f #"/") (keyword ch))))
           {})))

(defn git-logs [ztx branch & [n]]
  (->> (cmd/lns
        (cmd/$> `[git log -b ~branch "--date=format:'%Y-%m-%dT%H:%M:%S'" "--pretty=%H¦%s¦%aN¦%aE¦%cd" -n ~(or n 10)]
                {:dir (repo-dir ztx)}))
       (remove str/blank?)
       (mapv #(parse-commit % branch))
       (reverse)))


(defn get-commits [ztx branch]
  (let [last-commit (slurp (lc-file ztx branch))
        commits  (->> (cmd/lns
                       (cmd/$> `[git log -b ~branch "--date=format:'%Y-%m-%dT%H:%M:%S'" "--pretty=%H¦%s¦%aN¦%aE¦%cd" ~(str last-commit ".." branch)]
                               {:dir (repo-dir ztx)}))
                      (remove str/blank?)
                      (mapv #(parse-commit % branch))
                      (reverse))]
    (loop [res []
           prev last-commit
           [c & cs] commits]
      (if (nil? c)
        res
        (recur (conj res
                     (assoc c
                            :prev prev
                            :changes (changes ztx branch prev (:commit c))))
               (:commit c)
               cs)))))

(defn next-id [ztx branch]
  (let [id-file (rid-file ztx branch)
        rid (read-string (slurp id-file))]
    (spit id-file (inc rid))
    (inc rid)))

(defn update-repo [ztx]
  (cmd/$> '[git remote update] {:dir (repo-dir ztx)}))

(defn queue [ztx branch commit]
  (let [rid (next-id ztx branch)
        c (assoc commit :id rid)]
    (println "*" c)
    (spit (state-dir ztx branch "queue" rid) (pr-str c))
    (set-commit ztx branch (:commit c))))

(defn queue-commits [ztx] 
  (update-repo ztx)
  (doseq [br (mapv name (:branches (get-config ztx)))]
    (let [cms (get-commits ztx br)]
      (doseq [c cms]
        (queue ztx br c)))))

(defn start-loop [ztx service-path timeout f & args]
  (let [t (Thread. (fn []
                     (loop []
                       (print ".") (flush)
                       (apply f ztx args)
                       (Thread/sleep timeout)
                       (recur))))]
    (swap! ztx assoc-in (into [:services] service-path) t)
    (.start t)))

(defn stop-loop [ztx service-path]
  (when-let [svc (get-in @ztx (into [:services] service-path))]
    (.stop svc)))

(defn get-job [ztx branch]
  (when-let [file (->> (io/file (state-dir ztx branch "queue"))
                       .listFiles
                       (sort-by #(read-string (.getName %)))
                       first)]
    (-> (read-string (slurp file))
        (assoc :file (.getPath file)))))

(defn dequeue-job [ztx branch job]
  (let [file (state-dir ztx branch "queue" (:id job))]
    (cmd/$> `[rm ~file])))

(comment
  (get-job ztx "master")
  (def p
    (cmd/start-proc
     {:exec '[ps aux]
      :env {:MYENV "myenv"}
      :dir "/Users/niquola"
      :out (io/file "/tmp/test-logs")}))
  p
  (.exitValue p)
  )

(defn branch-worker [ztx branch]
  (when-let [job (get-job ztx branch)]
    (let [run  (:run (get-config ztx))
          br-dir (branch-dir ztx branch)]
      (swap! ztx assoc-in [:gitopa :jobs branch] job)
      (println :start branch (:id job))
      (cmd/$> `[mkdir -p ~(state-dir ztx branch "logs")])
      (cmd/$> `[git checkout ~(:commit job)] {:dir br-dir})
      (let [out  (io/file (state-dir ztx branch "logs" (:id job)))
            proc (cmd/proc {:exec (:exec run)
                            :env (merge (:env run) {:RUN (pr-str job)})
                            :dir br-dir
                            :out out})
            start (System/nanoTime)
            p (.start proc)]
        (swap! ztx assoc-in [:jobs branch :proc] p)
        (.waitFor p)
        (let [job (assoc job
                         :duration (/ (- (System/nanoTime) start) 1000000)
                         :status (.exitValue p))]
          (spit (state-dir ztx branch "runs" (:id job))
                (pr-str job))
          (dequeue-job ztx branch job)
          (println :done branch job))))))

(defn ensure-workers [ztx]
  (doseq [br (mapv name (:branches (get-config ztx)))]
    (when-not (get-in ztx [:services :workers br])
      (println :worker/start br)
      (start-loop ztx [:workers br] 2000 branch-worker br))))

(defn watch-config [ztx]
  (update-repo ztx)
  (cmd/$> `[git pull] {:dir (str (get-root ztx) "/gitopa")})
  (let [new-cfg (read-config ztx)
        cfg (get-config ztx)]
    (when-not (=  cfg new-cfg)
      (println "\n" :config-changed "\n" :old cfg "\n" :new new-cfg)
      (swap! ztx assoc :gitopa new-cfg)
      (init-branches ztx)
      (ensure-workers ztx))))

(defn sync-state [ztx]
  (let [dir (str (get-root ztx) "/gitopa")]
    (cmd/lns (cmd/$> `[git diff --stat] {:dir dir}))))

(defn start [& [dir]]
  (let [dir (or dir (pwd))
        ztx (atom {:gitopa {} :gitopa-dir dir})]
    (watch-config ztx)
    (start-loop ztx [:queue] 2000  queue-commits)
    (start-loop ztx [:config] 2000 watch-config)
    ztx))

(defn do-stop [ztx path svcs]
  (doseq [[k v] svcs]
    (cond (instance? Thread v)
          (do (.stop v)
              (println :stop (conj path k))
              (swap! ztx update-in path dissoc k))
          (map? v)
          (do-stop ztx (conj path k) v))))

(defn stop-all [ztx]
  (do-stop ztx [:services] (:services @ztx)))

(comment

  (def d "dir")
  (def rp "git@github.com:project.git")

  (init rp d)


  (def ztx  (start d))

  (init-branches ztx)

  (git-logs ztx "master" 10)
  (set-commit ztx "master" "b6f2f906a7de5f934784f319363b33003d9aa0cd")
  (set-commit ztx "master" "caa749b629a453052e92b629c5f6d1e8f3638c27")

  (get-commits ztx "master")
  (queue-commits ztx)

  (update-repo ztx)

  (start-loop ztx [:queue] 2000 queue-commits)
  (stop-loop ztx [:queue])

  (watch-config ztx)
  (start-loop ztx [:config] 2000 watch-config)
  (stop-loop ztx [:config])

  (sync-state ztx)

  (:services @ztx)

  (get-config ztx)
  (stop-all ztx)

  (start-loop ztx [:queue] 2000  queue-commits)
  (ensure-workers ztx)

  (start-loop ztx [:workers "master"] 2000 branch-worker "master")

  (branch-worker ztx "master")

  )
