(import ./errors :prefix "")

(defn u/parse-path
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

  (u/parse-path "/tmp/fun/my.fnl")
  # =>
  ["/tmp/fun/" "my.fnl"]

  (u/parse-path "/my.janet")
  # =>
  ["/" "my.janet"]

  (u/parse-path "pp.el")
  # =>
  ["" "pp.el"]

  (u/parse-path "/etc/init.d")
  # =>
  ["/etc/" "init.d"]

  (u/parse-path "/")
  # =>
  ["/" ""]

  (u/parse-path "")
  # =>
  ["" ""]

  )

(defn u/is-file?
  [path]
  #
  (= :file (os/stat path :mode)))

(defn u/merge-indexed
  [left right]
  (cond
    (and (nil? left) (nil? right)) nil
    (nil? left) (array ;right)
    (nil? right) (array ;left)
    (distinct [;left ;right])))

(comment

  (u/merge-indexed nil nil)
  # =>
  nil

  (u/merge-indexed [:a :b :c] nil)
  # =>
  @[:a :b :c]

  (u/merge-indexed nil [1 2 3])
  # =>
  @[1 2 3]

  (u/merge-indexed [:ant :bee :cat] [:bee :dog])
  # =>
  @[:ant :bee :cat :dog]

  )

# XXX: find a better home for this
(def u/conf-file ".tche.jdn")

(defn u/parse-conf-file
  [u/conf-file]
  (def b {:in "parse-conf-file" :args {:conf-file u/conf-file}})
  #
  (let [src (try (slurp u/conf-file)
              ([e] (e/emf (merge b {:e-via-try e})
                          "failed to slurp: %s" u/conf-file)))
        cnf (try (parse src)
              ([e] (e/emf (merge b {:e-via-try e})
                          "failed to parse: %s" u/conf-file)))]
    (when (not cnf)
      (e/emf b "failed to load: %s" u/conf-file))
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

