(import ./errors :prefix "")
(import ./files :prefix "")
(import ./settings :prefix "")

(defn a/merge-indexed
  [left right]
  (cond
    (and (nil? left) (nil? right)) nil
    (nil? left) (array ;right)
    (nil? right) (array ;left)
    (distinct [;left ;right])))

(comment

  (a/merge-indexed nil nil)
  # =>
  nil

  (a/merge-indexed [:a :b :c] nil)
  # =>
  @[:a :b :c]

  (a/merge-indexed nil [1 2 3])
  # =>
  @[1 2 3]

  (a/merge-indexed [:ant :bee :cat] [:bee :dog])
  # =>
  @[:ant :bee :cat :dog]

  )

(defn a/parse-args
  [args]
  (def b {:in "parse-args" :args {:args args}})
  #
  (def the-args (array ;args))
  #
  (def head (get the-args 0))
  #
  (when (or (= head "-h") (= head "--help")
            # might have been invoked with no paths in repository root
            (and (not head)
                 (not (f/is-file? s/conf-file))))
    (break @{:show-help true}))
  #
  (when (or (= head "-v") (= head "--version")
            # might have been invoked with no paths in repository root
            (and (not head) 
                 (not (f/is-file? s/conf-file))))
    (break @{:show-version true}))
  #
  (def opts
    (if head
      (if-not (and (string/has-prefix? "{" head)
                   (string/has-suffix? "}" head))
        @{}
        (let [parsed
              (try (parse (string "@" head))
                ([e] (e/emf (merge b {:e-via-try e})
                            "failed to parse options: %n" head)))]
          (when (not (and parsed (table? parsed)))
            (e/emf b "expected table but found: %s" (type parsed)))
          #
          (array/remove the-args 0)
          parsed))
      @{}))
  #
  (def [includes excludes roots]
    (cond
      # paths on command line take precedence over conf file
      (not (empty? the-args))
      [the-args @{} nil]
      # conf file
      (f/is-file? s/conf-file)
      (s/parse-conf-file s/conf-file)
      #
      (e/emf b "unexpected result parsing args: %n" args)))
  #
  (setdyn :test/color?
          (not (or (os/getenv "NO_COLOR") (get opts :no-color))))
  #
  (merge opts
         {:includes (a/merge-indexed includes (get opts :includes @[]))
          # value of :excludes ends up as a table
          :excludes (merge-into excludes
                                (invert (get opts :excludes @{})))
          :roots (a/merge-indexed roots (get opts :roots))}))

(comment

  (def old-value (dyn :test/color?))

  (setdyn :test/color? false)

  (a/parse-args ["src/main.janet"])
  # =>
  @{:excludes @{}
    :includes @["src/main.janet"]
    :roots nil}

  (a/parse-args ["-h"])
  # =>
  @{:show-help true}

  (a/parse-args ["{:overwrite true}" "src/main.janet"])
  # =>
  @{:excludes @{}
    :includes @["src/main.janet"]
    :overwrite true
    :roots nil}

  (a/parse-args [`{:excludes ["src/args.janet"]}` "src/main.janet"])
  # =>
  @{:excludes @{"src/args.janet" 0}
    :includes @["src/main.janet"]
    :roots nil}

  (setdyn :test/color? old-value)

  )

