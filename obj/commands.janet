(import ./errors :prefix "")
(import ./log :prefix "")
(import ./output :prefix "")
(import ./rewrite :prefix "")
(import ./tests :prefix "")

########################################################################

(defn c/mrr-single
  [input &opt opts]
  # try to make and run tests, then collect output
  (def [exit-code test-results test-out test-err]
    (t/make-and-run input opts))
  (when (= :no-tests exit-code)
    (break [:no-tests nil]))
  #
  (def {:report report} opts)
  (default report o/report)
  # print out results
  (report test-results test-out test-err)
  #
  (when (not= 0 exit-code)
    (break [:exit-code test-results]))
  #
  [:no-fails test-results])

(defn c/make-run-report
  [src-paths opts]
  (def b @{:in "make-run-report" :args {:src-paths src-paths :opts opts}})
  #
  (def excludes (get opts :excludes))
  (def p-paths @[])
  (def f-paths @[])
  (def test-results @[])
  # generate tests, run tests, and report
  (each path src-paths
    (when (and (not (has-value? excludes path))
               (= :file (os/stat path :mode)))
      (l/note :i path)
      (def single-result (c/mrr-single path opts))
      (put b :locals @{:single-result single-result :path path})
      (def [desc tr] single-result)
      (array/push test-results [path tr])
      (case desc
        :no-tests
        (l/noten :i " - no tests found")
        #
        :no-fails
        (let [n-tests (get tr :num-tests)
              ratio (o/color-ratio n-tests n-tests)]
          (l/notenf :i " - [%s]" ratio)
          (array/push p-paths path))
        #
        :exit-code
        (let [n-tests (get tr :num-tests)
              n-passes (- n-tests (length (get tr :fails)))
              ratio (o/color-ratio n-passes n-tests)]
          (l/notenf :i "[%s]" ratio)
          (array/push f-paths path))
        #
        (e/emf b "unexpected result %p for: %s" desc path))))
  #
  (l/notenf :i (o/separator "="))
  (def n-f-paths (length f-paths))
  (def n-p-paths (length p-paths))
  #
  (if (empty? f-paths)
    (l/notenf :i "All tests successful in %d file(s)."
              n-p-paths)
    (l/notenf :i "Test failures in %d of %d file(s)."
              n-f-paths (+ n-f-paths n-p-paths)))
  #
  (def exit-code (if (zero? n-f-paths) 0 1))
  #
  [exit-code test-results])

########################################################################

(defn c/mru-single
  [input &opt opts]
  (def b @{:in "mru-single" :args {:input input :opts opts}})
  # try to make and run tests, then collect output
  (def [exit-code test-results _ _] (t/make-and-run input opts))
  (when (= :no-tests exit-code)
    (break [:no-tests nil test-results]))
  # successful run means no tests to update
  (when (zero? exit-code)
    (break [:no-updates nil test-results]))
  #
  (def fails (get test-results :fails))
  (def update-info
    (seq [f :in (if (get opts :update-first)
                  @[(get fails 0)]
                  fails)
          :let [{:line-no line-no :test-value test-value} f
                tv-str (string/format "%j" test-value)]]
      [line-no tv-str]))
  (def ret (r/patch input update-info))
  (when (not ret)
    (e/emf (merge b {:locals {:fails fails :update-info update-info}})
           "failed to patch: %n" input))
  #
  (def lines (map |(get $ 0) update-info))
  #
  (if (get opts :update-first)
    [:single-update lines test-results]
    [:multi-update lines test-results]))

(defn c/make-run-update
  [src-paths opts]
  (def b @{:in "make-run-update" :args {:src-paths src-paths :opts opts}})
  #
  (def excludes (get opts :excludes))
  (def upd-paths @[])
  (def test-results @[])
  # generate tests, run tests, and update
  (each path src-paths
    (when (and (not (has-value? excludes path))
               (= :file (os/stat path :mode)))
      (def single-result (c/mru-single path opts))
      (put b :locals @{:path path :single-result single-result})
      (def [desc data tr] single-result)
      (array/push test-results [path tr])
      (case desc
        :no-tests
        (l/noten :i "no tests found")
        #
        :no-updates
        (l/noten :i "no tests needed updating")
        #
        :multi-update
        (let [cs-lines (string/join (map |(string $) data) ", ")]
          (array/push upd-paths path)
          (l/notenf :i "Test(s) updated in: %s on lines: %s"
                    path cs-lines))
        #
        :single-update
        (let [first-line (get data 0)]
          (array/push upd-paths path)
          (l/notenf :i "Test updated in: %s on line: %d"
                    path first-line)
          (break))
        #
        (e/emf b "unexpected result %n for: %s" desc path))))
  #
  (l/notenf :i "Test(s) updated in %d file(s)." (length upd-paths))
  #
  (def exit-code 0)
  #
  [exit-code test-results])

