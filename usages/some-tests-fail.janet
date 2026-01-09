(import ../src/main)

(comment

  (let [buf @""]
    (with-dyns [:out buf]
      (main/main ""
                 (string "{:raw true\n"
                         " :no-color true\n"
                         " :no-exit true\n"
                         " :overwrite true}")
                 "data/some-tests-fail"))
    (parse buf))
  # =>
  '@[["data/some-tests-fail/a.janet"
      @{:fails @[@{:expected-form 11
                   :expected-status true
                   :expected-value 11
                   :line-no 4
                   :name ""
                   :passed false
                   :test-form (+ 2 8)
                   :test-status true
                   :test-value 10}]
        :num-tests 1}]
     ["data/some-tests-fail/b.janet"
      @{:fails @[@{:expected-form false
                   :expected-status true
                   :expected-value false
                   :line-no 4
                   :name ""
                   :passed false
                   :test-form (nan? (/ 0 0))
                   :test-status true
                   :test-value true}]
        :num-tests 1}]]

  )

