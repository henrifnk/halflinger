#' @title Test algorithm of Chen et al.
#'
#' @description Predict-Method to [train_depth()] for halfspaces objects.
#'     Evaluates halfspace-mass or (approximate) -depth for points in
#'     data based
#'
#' @details Functionality splits by `metric` in in two main scenarios:
#'     ## Scenario [get_mass()]
#'     `metric = "mass"` computes approximate halfspace mass
#'     ## Scenario [get_depth()]
#'     `metric = "depth"` computes approximate halfspace depth ("Tukey depth")
#'
#' @inheritParams update.halfspaces
#' @param metric default = "mass", alternative "depth"
#' @param ... ellipsis
#' @return numeric vector with depth metric values for data: either halfspace
#' mass or halfspace depth
#'
#' @references
#'     Chen, B., Ting, K.M., Washio, T. et al.,
#'     Mach Learn (2015): 100(2):677--699
#'     Half-space mass a maximally robust and efficient data depth method
#'     \url{https://doi.org/10.1007/s10994-015-5524-x}
#' @seealso
#'     [train_depth()], [update.halfspaces()], [autoplot.halfspaces()]
#'
#' @export
predict.halfspaces <- function(object, data, metric = c("mass", "depth"), ...) {

  checkmate::assert_true(identical(validate_halfspaces(object), object))
  metric <- match.arg(metric)
  prediction_data <- prepare_data(data, scale = attr(object, "scale"))
  if (attr(object, "scale")) warning("prediction data are being scaled!")

  ####  calculate  ####
  switch(metric,
         "mass"  = get_mass(prediction_data, object),
         "depth" = get_depth(prediction_data, object))
}

# utilities --------------------------------------------------------------------

#' @title Calculate Halfspaces
#'
#' @description These functions calculate a prediction on depth/mass of data
#'
#' @inheritParams predict.halfspaces
#' @describeIn get_mass numeric vector with halfspace mass values for data
#'         for each combination of a halfspace and a data point, the algorithm
#'         uses the mass on the side of the split which the data point lies
#'         on, standardizes the input and takes the mean
get_mass <- function(data, object) {
  # init in 0 to iteratively add mean mass
  result <- numeric(NROW(data))
  for (halfspace in object) {
    projections <- project_scalar(data, halfspace$normal)

    result <- result + ifelse(
      projections >= halfspace$split,
      yes = halfspace$mass_above,
      no = 1 - halfspace$mass_above
      )
  }

  result / length(object)
}

#' @describeIn get_mass numeric vector with halfspace depth values for data
#'     initialized in 1 the algorithm iteratively searches in each halfspace
#'     for the minimal masses of each observation
get_depth <- function(data, object) {

  result <- rep(1, NROW(data))
  for (halfspace in object) {
    projections <- project_scalar(data, halfspace$normal)
    result <- pmin(result, ifelse(projections >= halfspace$split,
      yes = halfspace$mass_above,
      no = 1 - halfspace$mass_above
    ))
  }

  result * nrow(attr(object, "train_data")) * attr(object, "subsample")
}
