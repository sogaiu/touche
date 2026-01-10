(import ./args :prefix "")
(import ./commands :prefix "")
(import ./files :prefix "")
(import ./errors :prefix "")
(import ./log :prefix "")
(import ./output :prefix "")
(import ./settings :prefix "")

###########################################################################

(def version "DEVEL")

(def usage
  ``
  Usage: tche [<file-or-dir>...]
         tche [-h|--help] [-v|--version]

  Test out using comment-hidden expressions...touche!

  Parameters:

    <file-or-dir>          path to file or directory

  Options:

    -h, --help             show this output
    -v, --version          show version information

  Configuration:

    .tche.jdn              configuration file

  Examples:

    Create and run tests in `src/` directory:

    $ tche src

    `tche` can be used via `jpm`, `jeep`, etc. with
    some one-time setup.  Create a suitable `.tche.jdn`
    file in a project's root directory and a runner
    file in a project's `test/` subdirectory (see below
    for further details).

    Run via `jeep test`:

    $ jeep test

    Run via `jpm test`:

    $ jpm test

    Run using the configuration file via direct
    invocation:

    $ tche

  Example `.tche.jdn` content:

    {# describes what to test - file and dir paths
     :includes ["src" "bin/my-script"]
     # describes what to skip - file paths only
     :excludes ["src/sample.janet"]}

  Example runner file `test/trigger-tche.janet`:

    (import ../tche)

    (tche/main)
  ``)

########################################################################

(defn main
  [& args]
  (def start-time (os/clock))
  #
  (def opts (a/parse-args (drop 1 args)))
  #
  (when (get opts :show-help)
    (l/noten :o usage)
    (os/exit 0))
  #
  (when (get opts :show-version)
    (l/noten :o version)
    (os/exit 0))
  #
  (def test-sets @[])
  (def old-dir (os/cwd))
  (if-let [roots (get opts :roots)]
    # if roots are specified, includes / excludes are ignored
    (each r roots
      (os/cd r)
      (when (f/is-file? s/conf-file)
        (def [includes excludes _] (s/parse-conf-file s/conf-file))
        (when (not (empty? includes))
          (when (not (empty? excludes))
            (merge-into (get opts :excludes) excludes))
          (array/push test-sets
                      [r (f/collect-paths includes f/seems-like-janet?)])))
      (os/cd old-dir))
    # typical operation does not involve multiple roots
    (array/push test-sets
                [(os/cwd) (f/collect-paths (get opts :includes)
                                           f/seems-like-janet?)]))
  # turn off user-oriented output if raw output requested
  (when (get opts :raw) (l/clear-d-tables!))
  # 0 - successful testing / updating
  # 1 - at least one test failure
  # 2 - caught error
  (def [exit-code test-results]
    (try
      (if (or (get opts :update) (get opts :update-first))
        (c/make-run-update test-sets opts)
        (c/make-run-report test-sets opts))
      ([e f]
        (l/noten :e)
        (if (dictionary? e)
          (e/show e)
          (debug/stacktrace f e "internal "))
        [2 @[]])))
  #
  (if (get opts :raw)
    (print (o/color-form test-results))
    (l/notenf :i "Total processing time was %.02f secs."
              (- (os/clock) start-time)))
  #
  (when (not (get opts :no-exit))
    (os/exit exit-code)))

