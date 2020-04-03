#' @title Generate test data
#'
#' @description This functions are helpers to unit tests.
#'
#' @inheritParams train_depth
#' @inheritParams autoplot.halfspaces
#' @param seed0 an additional RNG seed
#'
#' @importFrom stats rnorm
#'
#' @describeIn generate_train_data generates datasets in order to approve, the
#'     functionality of [train_depth()], [update.halfspaces()],
#'     [predict.halfspaces()]
generate_train_data <- function(n_halfspace = 1e3, subsample = 1, scope = 1,
                                seed = NULL, scale = FALSE, seed0 = 121133) {
  set.seed(as.integer(seed0))
  angles <- seq(0, 2*pi, length = 101)[-101]
  data_list <- list(
    circle = cbind(cos(angles), sin(angles)),
    gaussian = cbind(rnorm(200), rnorm(200)),
    clustered = cbind(rnorm(100, mean = rep(c(-1, 0, 1), l = 100), sd = .2),
                      rnorm(100, mean = rep(c(-1, 0.5, -1), l = 100), sd = .2)),
    grid = expand.grid(seq(-2, 2, l = 15), seq(-2, 2, l = 15))
  )
  lapply(data_list, function(x) {
    list(as.data.frame(x), n_halfspace = n_halfspace, subsample = subsample,
         scope = scope, seed = seed, scale = scale)
  })
}

#' @describeIn generate_train_data possibility to tune over all defaults of
#'     tain_depth and autoplot.halfspaces to approve the functionality of
#'     [autoplot.halfspaces()]
generate_plot_faith <- function(n_halfspace = 1e4, subsample = 1, scope = 1,
                                seed = NULL, scale = FALSE,
                                metric = c("mass", "depth"), points = TRUE,
                                type = c("heatmap", "contour"),
                                grid = NULL, gridlength = 70L) {

  faithful <- datasets::faithful
  hs_faithful <- train_depth(faithful, n_halfspace = n_halfspace,
                             subsample = subsample, scope = scope, seed = seed,
                             scale = scale)

  autoplot(hs_faithful, faithful, metric = metric, points = points, type = type,
           grid = grid, gridlength = gridlength)
}
