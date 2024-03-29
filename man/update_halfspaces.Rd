% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update-halfspacemass-def.R
\name{update_halfspaces}
\alias{update_halfspaces}
\title{Update Halfspaces}
\usage{
update_halfspaces(object, complete_data, n_halfspace, input)
}
\arguments{
\item{object}{a \code{\link[=halfspaces]{halfspaces()}} object containing a list arbitrary length of
\code{\link[=halfspace]{halfspace()}} objects.}

\item{complete_data}{A data set with all containing required unique
observations to \code{\link[=train_depth]{train_depth()}}}

\item{n_halfspace}{number of halfsspaces to draw, default = 1e4}

\item{input}{the parameter set an object was trained on}
}
\value{
updated halfspaces from \code{object}
}
\description{
This function updates the already drawn \code{halfspace} objects from input
\code{object}.

Functionality splits in two scenarios:
\itemize{
\item Scenario for an unchanged dataset:
if \code{data} are equal to train data in \code{object}, halfspace objects are
identical, too. And drawn halfspaces are a subset of \code{object}
\item Scenario for a changed dataset:
if \code{data} and train_data in \code{object} are not identical, new randomly drawn
subsamples are drawn which provide a new base for splitpoint criteria and
mass_above, therefore whole \code{\link[=train_depth]{train_depth()}} needs to be recalled on each
direction of the dataset
}
}
\seealso{
\code{\link[=update.halfspaces]{update.halfspaces()}}
}
