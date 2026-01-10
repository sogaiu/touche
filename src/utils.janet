(import ./errors :as e)

(defn parse-path
  [path]
  (def revcap-peg
    ~(sequence (capture (sequence (choice (to (choice "/" `\`))
                                          (thru -1))))
               (capture (thru -1))))
  (when-let [[rev-name rev-dir]
             (-?>> (string/reverse path)
                   (peg/match revcap-peg)
                   (map string/reverse))]
    [(or rev-dir "") rev-name]))

(comment

  (parse-path "/tmp/fun/my.fnl")
  # =>
  ["/tmp/fun/" "my.fnl"]

  (parse-path "/my.janet")
  # =>
  ["/" "my.janet"]

  (parse-path "pp.el")
  # =>
  ["" "pp.el"]

  (parse-path "/etc/init.d")
  # =>
  ["/etc/" "init.d"]

  (parse-path "/")
  # =>
  ["/" ""]

  (parse-path "")
  # =>
  ["" ""]

  )

(defn is-file?
  [path]
  #
  (= :file (os/stat path :mode)))

(defn merge-indexed
  [left right]
  (cond
    (and (nil? left) (nil? right)) nil
    (nil? left) (array ;right)
    (nil? right) (array ;left)
    (distinct [;left ;right])))

(comment

  (merge-indexed nil nil)
  # =>
  nil

  (merge-indexed [:a :b :c] nil)
  # =>
  @[:a :b :c]

  (merge-indexed nil [1 2 3])
  # =>
  @[1 2 3]

  (merge-indexed [:ant :bee :cat] [:bee :dog])
  # =>
  @[:ant :bee :cat :dog]

  )

# XXX: find a better home for this
(def conf-file ".tche.jdn")

(defn parse-conf-file
  [conf-file]
  (def b {:in "parse-conf-file" :args {:conf-file conf-file}})
  #
  (let [src (try (slurp conf-file)
              ([e] (e/emf (merge b {:e-via-try e})
                          "failed to slurp: %s" conf-file)))
        cnf (try (parse src)
              ([e] (e/emf (merge b {:e-via-try e})
                          "failed to parse: %s" conf-file)))]
    (when (not cnf)
      (e/emf b "failed to load: %s" conf-file))
    #
    (when (not (dictionary? cnf))
      (e/emf b "expected dictionary in conf, got: %s" (type cnf)))
    #
    (def roots
      (if-let [r (get cnf :roots)]
        (array ;r)
        nil))
    #
    [(array ;(get cnf :includes @[]))
     (invert (get cnf :excludes @{}))
     roots]))

