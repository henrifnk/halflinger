#####  Test autoplot.halfspaces  ###############################################
# load <faithful> data via generate_plot_faith  to test plotting

context("autoplot.halfspaces")
# Note: I'm aware of using an additional package here but this is the suggested
# to test on ggplot2 objects. If this is important it can be outcommented here
# and in Description. See for suggestion:
# https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html
test_that("output of ggplot() is stable", {
  vdiffr::expect_doppelganger(
    "contour-scaled-123",
    generate_plot_faith(points = FALSE, type = "contour",
                        seed = 123, scale = TRUE)
  )
  vdiffr::expect_doppelganger(
    "heat-unscaled-123", generate_plot_faith(seed = 123)
  )
  vdiffr::expect_doppelganger(
    "contour-depth-123", generate_plot_faith(seed = 123)
  )
})

test_that("structure classes", {
  faithful_heat <- generate_plot_faith()
  expect_s3_class(faithful_heat, class = "ggplot")
  expect_length(faithful_heat[["layers"]], 2)
  geom_layer <- faithful_heat[["layers"]][[1]][["geom"]]
  expect_s3_class(geom_layer, class = "GeomTile")

  faithful_contour <- generate_plot_faith(points = FALSE, type = "contour")
  expect_s3_class(faithful_contour, class = "ggplot")
  expect_length(faithful_contour[["layers"]], 1)
  geom_layer <- faithful_contour[["layers"]][[1]][["geom"]]
  expect_s3_class(geom_layer, class = "GeomPolygon")
})

test_that("test scaling", {
  faithful_heat <- generate_plot_faith(scale = TRUE)
  plot_data <- faithful_heat[["data"]]
  expect_lte(sd(plot_data[, "prediction"]), 1)
  faith_axis <-
    ggplot2::ggplot_build(faithful_heat)[["layout"]][["panel_params"]][[1]]
  expect_equal(mean(faith_axis[["x.range"]]),
               0, tol = 1)
  expect_equal(mean(faith_axis[["y.range"]]),
               0, tol = 1)

})

test_that("test Turkey depth & map labels", {
  faithful_depth <- generate_plot_faith(metric = "depth", type = "contour")
  expect_true(
    all(c("depth", "plotted in a Contour-Line-Plot", "Tukey Halfspace Depth")
        %in% unlist(faithful_depth$labels, use.names = FALSE))
    )
  faithful_mass <- generate_plot_faith(metric = "mass", type = "heatmap")
  expect_true(
    all(c("mass", "plotted in a Heatmap", "Halfspace Mass")
        %in% unlist(faithful_mass$labels, use.names = FALSE))
  )
})

test_that("test grid input", {
  expect_error(
    generate_plot_faith(grid = data.frame(replicate(10, sample(LETTERS, 10))))
  )
  grid <- expand.grid(
    V1 = seq(0, 6, length = 20),
    V2 = seq(30, 110, length = 20)
  )
  faithful_grid <- generate_plot_faith(grid = grid)
  expect_true(all(faithful_grid[["data"]][, 1:2] == grid))
})
