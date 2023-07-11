test_that("keep_only_relevant_values works as expected", {
  dfFilters <- tibble(
    var1 = sample(c("A", "B", "C"), 100, replace = TRUE),
    var2 = sample(c("D", "E", "F"), 100, replace = TRUE),
    var3 = sample(c("G", "H", "I"), 100, replace = TRUE)
  )

  filters = list(c("var2", c("D", "E")))
  relevant_values <- keep_only_relevant_values(filters, "var1", dfFilters)

  # Check if the relevant values are only from the rows where var2 is "D" or "E"
  expected_values <- dfFilters$var1[dfFilters$var2 %in% c("D", "E")]
  expect_equal(sort(relevant_values), sort(expected_values))
})
