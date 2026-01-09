(comment

  (+ 1 1)
  # =>
  2

  )

(comment

  # disabled because test indicator has been "damaged"
  (+ 2 8)
  # = >
  10

  )

# entire block is disabled because `comment` is not immediately after
# the opening paren
( comment

  (+ 2 7)
  # =>
  9

  )

