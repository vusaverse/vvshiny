test_that("limit_n_values_gantt works as expected", {
  df <- tibble(
    var1 = sample(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M"), 100, replace = TRUE),
    var2 = rnorm(100)
  )

  dfLimited <- limit_n_values_gantt(df, "var1", 10)

  # Check if the number of distinct values in var1 is less or equal to 10
  expect_true(n_distinct(dfLimited$var1) <= 10)
})
