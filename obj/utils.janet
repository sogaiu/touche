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

