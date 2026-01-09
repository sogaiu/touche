(import ../src/main)

(comment

  (let [buf @""]
    (with-dyns [:out buf]
      (main/main ""
                 (string "{:raw true\n"
                         " :no-color true\n"
                         " :no-exit true}")
                 "data/all-tests-pass"))
    (parse buf))
  # =>
  @[["data/all-tests-pass/one.janet" @{:fails @[] :num-tests 2}]
    ["data/all-tests-pass/two.janet" @{:fails @[] :num-tests 2}]]

  )

