#' @title Plot 2D-Halfspace depth/mass
#'
#' @description Autoplot-Method to [train_depth()] for halfspaces objects.
#'     Plot Halfspace depth/mass in two-dimensional grid
#'
#'     Method generates a additive ggplot object by 2 Steps:
#'     * 1st Step [design_grid()]
#'     Creates grid, predicts on it and saves it as database in a ggplot object.
#'     * 2nd Step [add_tile()] or [add_contour()] and/or [add_points()]
#'     Adds layers objects tile, contour and points to the grid, by argument
#'     specification
#'     * 3rd Step Labs 'n beauty
#'     Add lab aspects depending on `metric` for intelligibility and theme for
#'     beauty to the grid
#'
#' @inheritParams predict.halfspaces
#' @param points logical; should datapoints be plotted? default = TRUE
#' @param type char; choice between "heatmap" and "contour lines"
#' @param grid numeric dataframe or matrix defining the grid to plot at;
#'     default = NULL
#' @param gridlength integer indicating grid length, to compute a grid on,
#'     is ignored if `grid` is not NULL; default =  70L
#' @describeIn autoplot.halfspaces ggplot2 object; either a heatmap od a
#'     contour-line-plot in range of the grid
#'
#' @examples
#'     library(halflinger)
#'
#'     data <- matrix(c(rnorm(100), rnorm(100, 1, 5)), ncol = 2)
#'     train_data <- train_depth(data, n_halfspace = 100, seed = 123)
#'     \dontrun{autoplot.halfspaces(train_data, data, type = "contour")}
#'
#' @seealso
#'     [train_depth()], [update.halfspaces()], [predict.halfspaces()]
#' @importFrom ggplot2 autoplot
#' @importFrom stats predict
#' @export
autoplot.halfspaces <- function(object, data, metric = c("mass", "depth"),
                                points = TRUE, type = c("heatmap", "contour"),
                                grid = NULL, gridlength = 70L) {

  checkmate::assert_logical(points, len = 1, any.missing = FALSE)
  checkmate::assert_integerish(gridlength, len = 1, any.missing = FALSE)
  type <- match.arg(type)
  metric <- match.arg(metric)
  caption <- NULL
  subtitle <- NULL


  if (attr(object, "scale")) {
    data <- prepare_data(data, attr(object, "scale"))
    caption <- "Due to training the data have been scaled"
  }
  gg_grid <- design_grid(grid = grid, data = data, gridlength = gridlength)

  prediction <- predict(object, gg_grid[["data"]], metric)
  gg_grid[["data"]] <- cbind(gg_grid[["data"]],
                             prediction)

  switch(type,
         "heatmap" = {
           gg_grid <- add_tile(gg_grid = gg_grid)
           subtitle <- "plotted in a Heatmap"
           },
         "contour" = {
           gg_grid <- add_contour(gg_grid = gg_grid)
           subtitle <- "plotted in a Contour-Line-Plot"
           }
  )

  if (points) gg_grid <- add_points(data, gg_grid)

  gg_grid <- switch(metric,
                    "mass" = gg_grid + ggplot2::ggtitle("Halfspace Mass"),
                    "depth" = gg_grid + ggplot2::ggtitle("Tukey Halfspace Depth")
  )

  gg_grid +
    ggplot2::theme_classic() +
    ggplot2::labs(x = colnames(data)[1], y = colnames(data)[2], fill = metric,
                  color = metric, caption = caption, subtitle = subtitle)

}

# utilities --------------------------------------------------------------------

#' @title Design the grid
#' @description Designs a 2 column grid as database for a gg_plot object, if
#'     `grid` is not supplied, generates a grid by `gridlength`
#' @inheritParams autoplot.halfspaces
#' @describeIn design_grid assigns a two dimensional grid as database of a
#'     ggplot object
design_grid <- function(grid, data, gridlength = 70L) {

  data <- prepare_data(data = data)

  if (is.null(grid)) {
    grid <- create_grid(data = data, gridlength = gridlength)
    } else checkmate::assert_named(grid, names(data)[1:2])

  ggplot2::ggplot(data = grid, ggplot2::aes(x = grid[, 1], y = grid[, 2]))
}

#' @describeIn design_grid creates a grid of `gridlength`x`gridlength` Pixels,
#'     where the range of the each axis corresponds to the range of the `data`
#'     columns. The first column is plotted on the x-axis, the second on the
#'     y-axis.
create_grid <- function(data, gridlength = 70L) {
  range_1 <- range(data[, 1])
  range_2 <- range(data[, 2])
  grid <- expand.grid(
    seq(range_1[1] - .2 * diff(range_1),
             range_1[2] + .2 * diff(range_1),
             length = gridlength
    ),
    seq(range_2[1] - .2 * diff(range_2),
             range_2[2] + .2 * diff(range_2),
             length = gridlength
    )
  )
  structure(grid,
            names = colnames(data)[1:2])
}

#' @title Add layers
#' @description Adds layers tile, contour and/or points to the grid.
#' @inheritParams autoplot.halfspaces
#' @param gg_grid A gg_plot object with grid as database.
#' @describeIn add_tile adds geom_tile layer to the plot with spectral colours
#'     Supplements a third dimension to the plot by fill and colour aesthetics
#' @importFrom rlang .data
add_tile <- function(gg_grid) {

  spectralcolors <- c(
    "darkblue", "blue", "cyan", "lightgreen",
    "yellow", "orange", "red", "darkred"
  )

  gg_grid +
    ggplot2::geom_tile(data = gg_grid[["data"]],
                       ggplot2::aes(fill = .data[["prediction"]],
                                    colour = .data[["prediction"]])) +
    ggplot2::scale_fill_gradientn(colors = spectralcolors) +
    ggplot2::scale_colour_gradientn(colors = spectralcolors)
}

#' @describeIn add_tile adds geom_contour_filled layer to the plot.
#'     Supplements a third dimension to the plot by vizualized in coloured
#'     contour-lines
#' @importFrom rlang .data
add_contour <- function(gg_grid) {

  gg_grid +
    ggplot2::geom_contour_filled(ggplot2::aes(z = .data[["prediction"]]))
}

#' @describeIn add_tile adds geom_point layer to the plot.
#'     Supplements the plot by vizualizing points from trained data in the grid.
add_points <- function(data, gg_grid) {

  if (inherits(data, "matrix")) data <- as.data.frame(data)
  gg_grid <- gg_grid +
    ggplot2::geom_point(data = data, ggplot2::aes(x = data[, 1], y = data[, 2]),
                        colour = "white", alpha = 0.5)
}
