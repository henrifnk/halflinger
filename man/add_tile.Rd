% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoplot-halfspacemass-def.R
\name{add_tile}
\alias{add_tile}
\alias{add_contour}
\alias{add_points}
\title{Add layers}
\usage{
add_tile(gg_grid)

add_contour(gg_grid)

add_points(data, gg_grid)
}
\arguments{
\item{gg_grid}{A gg_plot object with grid as database.}

\item{data}{numeric matrix or dataframe; \strong{rows} are vectors to project}
}
\description{
Adds layers tile, contour and/or points to the grid.
}
\section{Functions}{
\itemize{
\item \code{add_tile}: adds geom_tile layer to the plot with spectral colours
Supplements a third dimension to the plot by fill and colour aesthetics

\item \code{add_contour}: adds geom_contour_filled layer to the plot.
Supplements a third dimension to the plot by vizualizing coloured
contour-lines

\item \code{add_points}: adds geom_point layer to the plot.
Supplements the plot by vizualizing points from trained data in the grid.
}}

\seealso{
\code{\link[=autoplot.halfspaces]{autoplot.halfspaces()}}
}
