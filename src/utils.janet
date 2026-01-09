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

  (parse-path "/")
  # =>
  ["/" ""]

  (parse-path "")
  # =>
  ["" ""]

  )

