(ns cmd
  (:require
   [clojure.java.shell :refer [sh] :as shell]
   [cheshire.core :as json]
   [clj-yaml.core :as yaml]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import [java.io File]
           [java.lang ProcessBuilder]
           [java.security MessageDigest]
           [java.math BigInteger]))

(defn u [& args]
  (->> args
       (reduce (fn [acc x]
                 (if (nil? x)
                   acc
                   (conj acc
                         (cond
                           (vector? x)
                           (apply u x)

                           (or (keyword? x) (string? x) (symbol? x))
                           (name x)

                           (map? x)
                           (->> x
                                (mapv (fn [[k v]]
                                        (str "-" (name k)
                                             (when-not (= true v)
                                               (str " " v)))))
                                (str/join " "))
                           :else (str x)))))
               [])
       (str/join " ")))



(defn md5 [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn env [m]
  (->> 
   m
   (mapv (fn [[k v]] (str (name k) "='" v "'")))
   (str/join " ")))

(defn lns [s]
  (str/split s #"\n"))

(defn split-cols [l]
  (str/split l #"\s+"))

(defn tbl [s]
  (let [lns (lns s) 
        cols (mapv keyword (split-cols (first lns)))]
    (->> (rest lns)
         (mapv (fn [x] (zipmap cols (split-cols x)))))))

(defn proc-res [res]
  (if (= (:exit res) 0)
    (:out res)
    (do 
      (println :Error)
      (println :cmd (:cmd res))
      (println :out (:out res))
      (println :error (:err res))
      (throw (Exception. (str (:cmd res) ": " (:err res)))))))

(defn to-yaml [x]
  (clj-yaml.core/generate-string x))

(defn from-yaml [x]
  (clj-yaml.core/parse-string x))

(defn to-json [x]
  (cheshire.core/generate-string x))

(defn from-json [x]
  (cheshire.core/parse-string x keyword))

(defn $> [cmd & [opts]]
  (let [cmd* (u cmd)
        res (apply clojure.java.shell/sh "bash" "-c" cmd* (apply concat (vec opts)))]
    (proc-res (assoc res :cmd cmd*))))



(defn tmp-file [& [content]]
  (let [f (File/createTempFile "tmp" "ssh")]
    (when content
      (spit (.getPath f) content))
    (.getPath f)))

(defn read-stream [s]
  (with-open [r (clojure.java.io/reader s)]
    (loop [acc []]
      (if-let [l (.readLine r)]
        (recur (conj acc l))
        acc))))

(defn proc [{dir :dir env :env args :exec out :out}]
  (let [proc (ProcessBuilder. (into-array String ["bash" "-c" (u args)]))
        _ (when dir (.directory proc (io/file dir)))
        _ (when env
            (let [e (.environment proc)]
              #_(.clear e)
              (doseq [[k v] env]
                (.put e (name k) (str v)))))
        _ (when out
            (.redirectOutput proc out)
            (.redirectError proc out))]
    proc))

(defn start-proc [opts]
  (.start (proc opts)))

(defn exec [{dir :dir env :env args :exec :as opts}]
  (let [prc (proc opts)
        p (.start prc)]
    (.waitFor p)
    {:status (.exitValue p)
     :stdout (read-stream (.getInputStream p))
     :stderr (read-stream (.getErrorStream p))}))

(defn exists?
  ([path]
   (.exists (io/file path)))
  ([root path]
   (.exists (io/file (str root (if (str/starts-with? path "/")
                                 path
                                 (str "/" path)))))))


(comment


  (tbl (:stdout (exec {:dir "../box"
                       :exec ["ls" "-lah"]})
                )
       )

  (tbl (:stdout (exec {:exec ["cd ../box && make small-test"]})))

  (u '[ls -lah])

  ($> '[ls -lah "/"])
  
  (tbl ($> '[ps aux]))
  (lns ($> '[ps aux]))

  (def opt {:a true})

  (lns ($> `[docker ps ~opt]))
  (tbl ($> `[docker ps ~opt]))

  )
