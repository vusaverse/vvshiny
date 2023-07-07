test_that("basic_plot function test", {

  ## TODO Errors are often generated when print, so a print function as well
  print_basic_plot <- function(...) {
    print(basic_plot(...))
  }

  # Sample data for testing
  df <- data.frame(x_var = rnorm(100), y_var = rnorm(100), color_var = sample(c("Red", "Blue"), 100, replace = TRUE))
  xlab_setting <- ggplot2::xlab("x label")
  ylab_setting <- ggplot2::ylab("y label")
  ggplot_instellingen <- ggplot2::geom_point()
  scale_y <- ggplot2::scale_y_continuous()

  # Test 1: Check the basic functionality with all arguments
  test_plot1 <- basic_plot(df, "x_var", "y_var", "color_var", xlab_setting, ylab_setting, ggplot_instellingen, "none", scale_y)
  expect_type(test_plot1, "list")

  # Test 2: Check the functionality when scale_y is not provided
  test_plot2 <- basic_plot(df, "x_var", "y_var", "color_var", xlab_setting, ylab_setting, ggplot_instellingen, "none")
  expect_type(test_plot2, "list")


  # Test 3: Check if the function fails gracefully with incorrect input
  expect_error(
    print_basic_plot(df, "non_existent", "y_var", "color_var", xlab_setting, ylab_setting, ggplot_instellingen, "bottom", scale_y),
    "object 'non_existent' not found"
    )

#   # Test 4: Check if the function fails gracefully with NULL data frame
#   expect_error(basic_plot(NULL, "x_var", "y_var", "color_var", xlab_setting, ylab_setting, ggplot_instellingen, "bottom", scale_y), "data must be a data frame")
#
#   # Test 5: Check if the function fails gracefully with non-string input for x, y, color
#   expect_error(basic_plot(df, 1, "y_var", "color_var", xlab_setting, ylab_setting, ggplot_instellingen, "bottom", scale_y), "character argument expected")
#
#   # Test 6: Check if the function fails gracefully with non-ggplot object for xlab_setting, ylab_setting, ggplot_instellingen
#   expect_error(basic_plot(df, "x_var", "y_var", "color_var", "wrong input", ylab_setting, ggplot_instellingen, "bottom", scale_y), "ggplot object expected")
#
#   # Test 7: Check if the function fails gracefully with incorrect legend_position
#   expect_error(basic_plot(df, "x_var", "y_var", "color_var", xlab_setting, ylab_setting, ggplot_instellingen, "wrong_position", scale_y), "invalid legend position")
#
})

#
# # Test data
# df <- data.frame(x = 1:3, y = c(1, 2, 3), color = c("red", "blue", "green"))
# ggplot_instellingen <- list()
#
# # Test if plot is created
# test_that("basic_plot() returns a ggplot object", {
#   plot <- basic_plot(df, "x", "y", "color", ggplot2::xlab("x"), ggplot2::ylab("y"), ggplot_instellingen)
#   expect_is(plot, "ggplot")
# })
#
# # Test if xlab and ylab are set
# test_that("basic_plot() sets ggplot2::xlab and ylab", {
#   plot <- basic_plot(df, "x", "y", "color", ggplot2::xlab("x"), ggplot2::ylab("y"), ggplot_instellingen)
#   expect_equal(ggplot2::ggplot_build(plot)$layout$panel_params[[1]]$xlab, "x")
#   expect_equal(ggplot2::ggplot_build(plot)$layout$panel_params[[1]]$ylab, "y")
# })
#
# # Test if scale_y works
# test_that("basic_plot() can set scale_y", {
#   plot <- basic_plot(df, "x", "y", "color", ggplot2::xlab("x"), ggplot2::ylab("y"), ggplot_instellingen, scale_y = ggplot2::scale_y_discrete())
#   expect_equal(ggplot2::ggplot_build(plot)$layout$panel_params[[1]], "log10")
# })
#
# # Test legend position
# test_that("basic_plot() can set legend position", {
#   plot <- basic_plot(df, "x", "y", "color", ggplot2::xlab("x"), ggplot2::ylab("y"), ggplot_instellingen, legend_position = "none")
#   expect_equal(ggplot2::ggplot_build(plot)$layout$legend$position, "none")
# })

#1 / "a"

# test <- basic_plot(df, "non_existent", "y_var", "color_var", xlab_setting, ylab_setting, ggplot_instellingen, "bottom", scale_y)
#
# expect_error(
#   basic_plot(df, "non_existent", "y_var", "color_var", xlab_setting, ylab_setting, ggplot_instellingen, "bottom", scale_y) %>% print(),
#   "object 'non_existent' not found"
# )
#
#
# #  regexp = "object 'non_existent' not found"
# expect_error(
#   stop("foo"), "foo")
# )
