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

(defn u/parse-conf-file
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
    [(array ;(get cnf :includes @[]))
     (array ;(get cnf :excludes @[]))]))

