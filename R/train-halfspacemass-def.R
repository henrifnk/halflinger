#' @title Implements Training algorithm of Chen et al.
#'
#' @description Creates an halfspaces objetct of arbitrary length containing
#'  halfspace objects
#'
#' @details Functionality after input check can be summed up in two main steps:
#'     ## 1st Step [get_directions()]
#'     draw n random directions in m-dimensional space where n is
#'     defined by `n_halfspace` and m is the sum of columns in `data`
#'     ## 2nd Step [get_halfspace()]
#'     Iterate over each direction: Project a subsample of `data`
#'     on the direction, create a splitpoint in the projection and measure the
#'     mass above the split point (a direction, a splitpoint and a measure of
#'     mass form a single halfspace)
#'
#' @param data numeric matrix or dataframe; **rows** are vectors to project
#' @param n_halfspace number of halfsspaces to draw, default = 1e4
#' @param subsample what proportion of data to use for each halfspace
#'     estimation, default = 1
#' @param scope  minimum 1; controls size of region of convexity for halfspace
#'     mass (\eqn{\lambda} in the paper) i.e., how far outside of sampled data range
#'     the sampled hyperplanes can lie; default = 1
#' @param seed optional RNG seed; default = NULL
#' @param scale logical; wether data should be scaled; default = FALSE
#' @return a halfspace object of <n_halfspace> halfspaces, defined by their
#'     normal vector and offset from origin, with estimated data frequencies
#'     above/below halfspace boundary object attributes contain the input
#'     arguments for reproducability
#'
#' @references
#'     Chen, B., Ting, K.M., Washio, T. et al.,
#'     Mach Learn (2015): 100(2):677--699
#'     Half-space mass a maximally robust and efficient data depth method
#'     \url{https://doi.org/10.1007/s10994-015-5524-x}
#' @seealso
#'     [update.halfspaces()], [autoplot.halfspaces()], [predict.halfspaces()]
#'
#' @export
train_depth <- function(data, n_halfspace = 1e3, subsample = 1, scope = 1,
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

#' @title Function to check and prepare <data> for calculation
#' @description ensures <data> to be a numeric matrix & scales them if <scale>
#' @inheritParams train_depth
#' @return a numeric (scaled) matrix
prepare_data <- function(data, scale = FALSE) {
  checkmate::assert_logical(scale, len = 1, any.missing = FALSE)
  if (inherits(data, "data.frame")) data <- as.matrix(data)
  checkmate::assert_matrix(data, mode = "numeric", any.missing = FALSE,
                           min.cols = 2, min.rows = 1
  )

  if (scale) return(scale(data))
  data
}

#' @title Draw directions in <dim>-dimnesional Space
#' @description Uniformly sample <n_halfspace> <dim>-dimensional direction
#'     vectors
#' @inheritParams train_depth
#' @param dims dimensions of space to draw directions from
#' @importFrom stats rnorm
#' @return  a <dims> x <n_halfspace> matrix of <n_halfspace> <dims>-dimensional
#'     directions
get_directions <- function(n_halfspace, dims = 2) {
  checkmate::assert_integerish(dims, lower = 2)
  matrix(rnorm(dims * n_halfspace), nrow = dims)
}

#' @title Create a Halfspace
#' @description Project a subsample of <data> on
#'     the direction, create a splitpoint in the projection and measure the mass
#'     above the split point vectors
#' @param normal a vector of indicating a direction in space
#' @inheritParams train_depth
#' @return  a halfspace object containing a direction, a splitpoint and a
#' measure of mass
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

#' @title Create a Subsample
#' @description creates a subsample of <data>, containing a <subsample>
#' proportion of rows in data
#' @inheritParams get_halfspace
#' @return  a subsample of rows in data
subsample <- function(data, subsample = 1) {
  nrows <- NROW(data)
  subsample_size <- round(subsample * seq_len(nrows))
  subsample_size <- min(max(1, subsample_size), nrows)
  if (subsample_size == nrows) return(data)
  use <- sample(seq_len(nrows), subsample_size)
  data[use, ]
}

#' @title Scalar projection
#' @description project data points onto the normal vector defining the orientation
#'     of a halfspace
#' @inheritParams get_halfspace
#' @return  vector: projections of <data> on <direction> in units of
#'     "length of <direction>"
#' @seealso  \url{https://en.wikipedia.org/wiki/Scalar_projection}
project_scalar <- function(data, normal) {
  checkmate::assert_numeric(normal, any.missing = FALSE, min.len = 2)
  checkmate::assert_matrix(data,
                           any.missing = FALSE, ncols = length(normal),
                           mode = "numeric"
  )

  data %*% normal
}

#' @title Draw a Splitpoint
#' @description sample a splitpoint in span-range of data
#' @param projections vector: projections of <data> on <direction> in units of
#'     "length of <direction>"
#' @inheritParams get_halfspace
#' @importFrom stats runif
#' @return "offset" of the halfspace boundary along its normal vector
sample_split <- function(projections, scope) {
  checkmate::assert_numeric(projections, any.missing = FALSE, finite = TRUE)
  minmax <- range(projections)
  span <- minmax[2] - minmax[1]
  mid <- mean(minmax)
  runif(1, min = mid - scope / 2 * span, max = mid + scope / 2 * span)
}

#' @title Mass above split
#'
#' @description get proportion of observations above boundary separating 2
#'      halfspaces
#'
#' @inheritParams sample_split
#' @param split "offset" of the halfspace boundary along its normal vector
#' @return a vector with relative data frequency "at or above" the boundary
#' defined by split
compute_mass <- function(projections, split) {
  checkmate::assert_numeric(projections, any.missing = FALSE, finite = TRUE)
  checkmate::assert_number(split, finite = TRUE)
  mean(projections >= split)
}
