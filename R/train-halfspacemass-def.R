#' @title Implements Training algorithm of Chen et al.
#'
#' @description
#' Creates a [halfspaces()] object of arbitrary length containing [halfspace()]
#' objects
#'
#' Functionality can be summed up in two main steps:
#' * 1st Step [get_directions()]:
#' draw n random directions in m-dimensional space where n is
#' defined by `n_halfspace` and m is the sum of columns in `data`
#'
#' * 2nd Step [get_halfspace()]:
#' Iterate over each direction: Project a subsample of `data`
#' on the direction, create a splitpoint in the projection and measure the
#' mass above the split point. The rresulting output- a direction, a
#' splitpoint and a measure of mass form a single [halfspace()]
#'
#' @param data numeric matrix or dataframe; **rows** are vectors to project
#' @param n_halfspace number of halfsspaces to draw, default = 1e4
#' @param subsample
#' what proportion of data to use for each [halfspace()] estimation, default = 1
#' @param scope
#' minimum 1; controls size of region of convexity for [halfspace()]
#' mass (\eqn{\lambda} in the paper) i.e., how far outside of sampled data
#' range the sampled hyperplanes can lie; default = 1
#' @param seed optional RNG seed; default = NULL
#' @param scale logical; wether data should be scaled; default = FALSE
#' @return
#' a [halfspaces()] object of `n_halfspace` [halfspace()] objects, defined by
#' their normal vector and offset from origin, with estimated data
#' frequencies above/below halfspace boundary. Object attributes contain the
#' input arguments for reproducability.
#'
#' @examples
#' library(halflinger)
#'
#' data <- matrix(c(rnorm(100), rnorm(100, 1, 5)), ncol = 2)
#' train_data <- train_depth(data, n_halfspace = 100, seed = 123)
#'
#' @references
#' Chen, B., Ting, K.M., Washio, T. et al.,
#' Mach Learn (2015): 100(2):677--699
#' Half-space mass A maximally robust and efficient data depth method
#' \url{https://doi.org/10.1007/s10994-015-5524-x}
#' @seealso
#' [update.halfspaces()], [autoplot.halfspaces()], [predict.halfspaces()]
#'
#' @export
train_depth <- function(data, n_halfspace = 1e4, subsample = 1, scope = 1,
                        seed = NULL, scale = FALSE) {

    train_data <- prepare_data(data, scale = scale)
    checkmate::assert_integerish(n_halfspace, lower = 1)
    checkmate::assert_number(subsample, lower = 1e-3, upper = 1)
    checkmate::assert_number(scope, lower = 1, na.ok = FALSE, finite = TRUE)
    if (!is.null(seed)) {
      checkmate::assert_integerish(seed, lower = 1L)
      set.seed(as.integer(seed))
    }
    dims <- ncol(data)
    normals <- get_directions(n_halfspace, dims = dims)

    halfspaces <- apply(normals, 2, get_halfspace,
                        data = train_data, subsample = subsample, scope = scope
                        )

    halfspaces(halfspaces, train_data = data, subsample = subsample,
               scope = scope, seed = seed, scale = scale
               )
}

# utilities --------------------------------------------------------------------

#' @title Function to check and prepare `data` for calculation
#' @description ensures `data` to be a numeric matrix & scales them if `scale`
#' @inheritParams train_depth
#' @return a numeric (scaled) matrix
#' @seealso
#' [train_depth()], [update.halfspaces()], [autoplot.halfspaces()] and
#' [predict.halfspaces()]
prepare_data <- function(data, scale = FALSE) {
  checkmate::assert_logical(scale, len = 1, any.missing = FALSE)
  if (inherits(data, "data.frame")) data <- as.matrix(data)
  checkmate::assert_matrix(data, mode = "numeric", any.missing = FALSE,
                           min.cols = 2, min.rows = 1
  )

  if (scale) return(scale(data))
  data
}

#' @title Draw directions in `dims`-dimnesional Space
#' @description
#' Uniformly sample `n_halfspace` `dims`-dimensional direction vectors
#' @inheritParams train_depth
#' @param dims dimensions of space to draw directions from
#' @importFrom stats rnorm
#' @return
#'  `dims` x `n_halfspace` matrix of `n_halfspace` `dims`-dimensional directions
#' @seealso [train_depth()]
get_directions <- function(n_halfspace, dims = 2) {
  checkmate::assert_integerish(dims, lower = 2)
  matrix(rnorm(dims * n_halfspace), nrow = dims)
}

#' @title Create a Halfspace
#' @description Project a subsample of `data` on
#'     the direction, create a splitpoint in the projection and measure the mass
#'     above the split point vectors
#' @param normal a vector indicating a direction in space
#' @inheritParams train_depth
#' @param projections output from [project_scalar()]
#' @param split output from [sample_split()]
#' @describeIn get_halfspace
#' a [halfspace()] object containing a direction, a splitpoint and a measure of
#' mass
#' @seealso [train_depth()]
get_halfspace <- function(normal, data, subsample, scope) {
  sampledata <- subsample(data, subsample)
  projections <- project_scalar(sampledata, normal)
  split <- sample_split(projections, scope)
  halfspace(
    normal = normal,
    split = split,
    mass_above = compute_mass(projections, split),
    col_data = ncol(data)
  )
}

#' @describeIn
#' get_halfspace subfunction to `get_halfspaces`;
#' creates subsample of `data`, defined by `subsample` proportion of rows in
#' `data`
subsample <- function(data, subsample = 1) {
  nrows <- NROW(data)
  subsample_size <- round(subsample * seq_len(nrows))
  subsample_size <- min(max(1, subsample_size), nrows)
  if (subsample_size == nrows) return(data)
  use <- sample(seq_len(nrows), subsample_size)
  data[use, ]
}

#' @describeIn get_halfspace
#' subfunction to `get_halfspaces`; vector:
#' projections of `data` on `normal` in units of "length of `normal`"
#' @seealso  \url{https://en.wikipedia.org/wiki/Scalar_projection}
project_scalar <- function(data, normal) {
  checkmate::assert_numeric(normal, any.missing = FALSE, min.len = 2)
  checkmate::assert_matrix(data,
                           any.missing = FALSE, ncols = length(normal),
                           mode = "numeric"
  )

  data %*% normal
}

#' @describeIn get_halfspace
#' subfunction to `get_halfspaces`; "offset" of the halfspace boundary along
#' its normal vector. Output is drawn from range. Configure range by `scope`.
#' @importFrom stats runif
sample_split <- function(projections, scope) {
  checkmate::assert_numeric(projections, any.missing = FALSE, finite = TRUE)
  minmax <- range(projections)
  span <- minmax[2] - minmax[1]
  mid <- mean(minmax)
  runif(1, min = mid - scope / 2 * span, max = mid + scope / 2 * span)
}

#' @describeIn get_halfspace
#' subfunction to `get_halfspaces`; a vector with relative data frequency
#' "at or above" the boundary in `projections` defined by `split`
compute_mass <- function(projections, split) {
  checkmate::assert_numeric(projections, any.missing = FALSE, finite = TRUE)
  checkmate::assert_number(split, finite = TRUE)
  mean(projections >= split)
}
