
test_that("prep_df works as expected", {
  df <- tibble(
    VIS_Groep = sample(c("Group1", "Group2", "Group3"), 100, replace = TRUE),
    VIS_Groep_naam = sample(c("Name1", "Name2", "Name3"), 100, replace = TRUE),
    var1 = sample(c("A", "B", "C"), 100, replace = TRUE),
    var2 = rnorm(100),
    color_var = sample(c("Red", "Blue", "Green"), 100, replace = TRUE)
  )

  lFilters = list(c("var1", c("A", "B")))
  lValues_for_naming = list("A", "B")
  color_var = "color_var"
  dfPrepared <- prep_df(lFilters, lValues_for_naming, df, color_var, facet = "right")

  expect_equal(nrow(dfPrepared), sum(df$var1 %in% c("A", "B")))
  expect_equal(unique(dfPrepared$VIS_Groep), "right")
  expect_s3_class(dfPrepared$color_var, "factor")
})
