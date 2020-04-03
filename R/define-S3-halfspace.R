#' @title Define Class halfspace
#' @description Define a new S3 class `halfspace`.
#'      Structure of class definition:
#'      * interface
#'      * constructor
#'      * validator
#' @param object a object of class halfspace
#' @inheritParams get_halfspace
#' @inheritParams compute_mass
#' @param mass_above a vector with relative data frequency "at or above" the
#'     split
#' @param col_data dimesions of the space that halfspace lives in
#' @describeIn halfspace a halfspace object containing a direction, a splitpoint
#'     and a measure of mass [train_depth()], [update.halfspaces()],
#'     [predict.halfspaces()]
#'
#' @seealso [get_halfspace()], [halfspaces()]
halfspace <- function(normal, split, mass_above, col_data) {
  validate_halfspace(new_halfspace(normal = normal,
                                   split = split,
                                   mass_above = mass_above),
                     col_data = col_data)
}

#' @describeIn halfspace a low level constructor for class halfspace
new_halfspace <- function(normal, split, mass_above) {
  object <- list(normal = normal, split = split, mass_above = mass_above)
  structure(
    object,
    class = c("halfspace", class(object)))
}

#' @describeIn halfspace check on structur of halfspace object w.r.t data
is_halfspace <- function(object, col_data) {
  checkmate::test_class(object, "halfspace")  &&
    checkmate::test_list(object, len = 3, types = rep("numeric", 3)) &&
    checkmate::test_names(names(object),
                          subset.of = c("normal", "split", "mass_above")) &&
    checkmate::test_numeric(object$normal, finite = TRUE,
                            any.missing = FALSE, len = col_data) &&
    checkmate::test_number(object$split) &&
    checkmate::test_number(object$mass_above, lower = 0, upper = 1)
}


#' @describeIn halfspace validate structur of halfspace object
validate_halfspace <- function(object, col_data) {
  if (!is_halfspace(object = object, col_data = col_data)) {
    stop("Halfspace is not in correct format.")
  }
  object
}
