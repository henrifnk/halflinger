#' @title Define Class halfspaces
#'
#' @description Define a new S3 class `halfspaces`.
#'     Structure of class definition: interface, constructor, validator.
#'
#' @param object list of arbitrary length containing `halfspace` objects
#' @param train_data input data in [train_depth()]
#' @inheritParams train_depth
#' @describeIn halfspaces
#'     Attributes of this objects are all neccesary Arguments from
#'     [train_depth()] to reproduce the sample if a seed is supplied
#'
#' @seealso [train_depth()], [halfspace()], [update.halfspaces()],
#'     [autoplot.halfspaces()], [predict.halfspaces()]
halfspaces <- function(object, train_data, subsample, scope, seed, scale) {
  validate_halfspaces(
    new_halfspaces(object, train_data = train_data, subsample = subsample,
                   scope = scope, seed = seed, scale = scale)
  )
}

#' @describeIn halfspaces a low level constructor for class halfspace
new_halfspaces <-
  function(object, train_data, subsample, scope, seed, scale) {

    structure(object,
              train_data = train_data,
              subsample = subsample,
              scope = scope,
              seed = seed,
              scale = scale,
              class = c("halfspaces", class(object)))
  }

#' @describeIn halfspaces validate structur of halfspace object
validate_halfspaces <- function(object) {
  checkmate::assert_class(object, "halfspaces")
  data <- attr(object, which =  "train_data")
  wrong <- !sapply(object, is_halfspace, col_data = ncol(data))
  if (all(wrong)) stop("Halfspaces are not in correct format.")
  if (any(wrong)) {
    stop(
      "Halfspaces ", paste(which(wrong), collapse = ", "),
      " not in correct format."
    )
  }
  object
}
