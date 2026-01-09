(import ./args :as a)
(import ./commands :as c)
(import ./errors :as e)
(import ./log :as l)
(import ./output :as o)
(import ./search :as s)

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
  (def src-paths
    (s/collect-paths (get opts :includes)
                     |(or (string/has-suffix? ".janet" $)
                          (s/has-janet-shebang? $))))
  (when (get opts :raw)
    (l/clear-d-tables!))
  # 0 - successful testing / updating
  # 1 - at least one test failure
  # 2 - caught error
  (def [exit-code test-results]
    (try
      (if (or (get opts :update) (get opts :update-first))
        (c/make-run-update src-paths opts)
        (c/make-run-report src-paths opts))
      ([e f]
        (if (dictionary? e)
          (do (l/noten :e) (e/show e))
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

