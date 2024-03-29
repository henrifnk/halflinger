% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/train-halfspacemass-def.R
\name{train_depth}
\alias{train_depth}
\title{Implements Training algorithm of Chen et al.}
\usage{
train_depth(
  data,
  n_halfspace = 10000,
  subsample = 1,
  scope = 1,
  seed = NULL,
  scale = FALSE
)
}
\arguments{
\item{data}{numeric matrix or dataframe; \strong{rows} are vectors to project}

\item{n_halfspace}{number of halfsspaces to draw, default = 1e4}

\item{subsample}{what proportion of data to use for each \code{\link[=halfspace]{halfspace()}} estimation, default = 1}

\item{scope}{minimum 1; controls size of region of convexity for \code{\link[=halfspace]{halfspace()}}
mass (\eqn{\lambda} in the paper) i.e., how far outside of sampled data
range the sampled hyperplanes can lie; default = 1}

\item{seed}{optional RNG seed; default = NULL}

\item{scale}{logical; wether data should be scaled; default = FALSE}
}
\value{
a \code{\link[=halfspaces]{halfspaces()}} object of \code{n_halfspace} \code{\link[=halfspace]{halfspace()}} objects, defined by
their normal vector and offset from origin, with estimated data
frequencies above/below halfspace boundary. Object attributes contain the
input arguments for reproducability.
}
\description{
Creates a \code{\link[=halfspaces]{halfspaces()}} object of arbitrary length containing \code{\link[=halfspace]{halfspace()}}
objects

Functionality can be summed up in two main steps:
\itemize{
\item 1st Step \code{\link[=get_directions]{get_directions()}}:
draw n random directions in m-dimensional space where n is
defined by \code{n_halfspace} and m is the sum of columns in \code{data}
\item 2nd Step \code{\link[=get_halfspace]{get_halfspace()}}:
Iterate over each direction: Project a subsample of \code{data}
on the direction, create a splitpoint in the projection and measure the
mass above the split point. The rresulting output- a direction, a
splitpoint and a measure of mass form a single \code{\link[=halfspace]{halfspace()}}
}
}
\examples{
library(halflinger)

data <- matrix(c(rnorm(100), rnorm(100, 1, 5)), ncol = 2)
train_data <- train_depth(data, n_halfspace = 100, seed = 123)

}
\references{
Chen, B., Ting, K.M., Washio, T. et al.,
Mach Learn (2015): 100(2):677--699
Half-space mass A maximally robust and efficient data depth method
\url{https://doi.org/10.1007/s10994-015-5524-x}
}
\seealso{
\code{\link[=update.halfspaces]{update.halfspaces()}}, \code{\link[=autoplot.halfspaces]{autoplot.halfspaces()}}, \code{\link[=predict.halfspaces]{predict.halfspaces()}}
}
