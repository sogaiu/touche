# this is meant to be executed from the repository root directory

# this path is relative to test dir
(import ../tche)

(when (= :jeep (dyn :test/runner)) (print))

(tche/main)

