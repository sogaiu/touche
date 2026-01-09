(def s/sep
  (if (= :windows (os/which))
    `\`
    "/"))

(defn s/find-files
  [dir &opt pred]
  (default pred identity)
  (def paths @[])
  (defn helper
    [a-dir]
    (each path (os/dir a-dir)
      (def sub-path
        (string a-dir s/sep path))
      (case (os/stat sub-path :mode)
        :directory
        (when (not= path ".git")
          (when (not (os/stat (string sub-path s/sep ".gitrepo")))
            (helper sub-path)))
        #
        :file
        (when (pred sub-path)
          (array/push paths sub-path)))))
  (helper dir)
  paths)

(comment

  (s/find-files "." |(string/has-suffix? ".janet" $))

  )

(defn s/clean-end-of-path
  [path a-sep]
  (when (one? (length path))
    (break path))
  (if (string/has-suffix? a-sep path)
    (string/slice path 0 -2)
    path))

(comment

  (s/clean-end-of-path "hello/" "/")
  # =>
  "hello"

  (s/clean-end-of-path "/" "/")
  # =>
  "/"

  )

(defn s/has-janet-shebang?
  [path]
  (with [f (file/open path)]
    (def first-line (file/read f :line))
    (when first-line
      (and (string/find "env" first-line)
           (string/find "janet" first-line)))))

(defn s/collect-paths
  [includes &opt pred]
  (default pred identity)
  (def filepaths @[])
  # collect file and directory paths
  (each thing includes
    (def apath (s/clean-end-of-path thing s/sep))
    (def mode (os/stat apath :mode))
    # XXX: should :link be supported?
    (cond
      (= :file mode)
      (array/push filepaths apath)
      #
      (= :directory mode)
      (array/concat filepaths (s/find-files apath pred))))
  #
  filepaths)

