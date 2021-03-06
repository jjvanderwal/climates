% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/request_bbox.R
\name{request_bbox}
\alias{request_bbox}
\title{An OPeNDAP wrapper for the Climates function CF_bbox_grid.}
\usage{
request_bbox(ncdf4_handle, rep_var, bbox_in)
}
\arguments{
\item{ncdf4_handle}{ncdf4 object.}

\item{rep_var}{a variable in the necdf4 source that is representative.}

\item{bbox_in}{a bounding box to be passwed to CF_bbox_grid.}
}
\value{
bbox_indices a set of indices from the source dataset for the bounding box.
}
\description{
This function takes an open ncdf4 data source, a representative variable and a bounding box.
It returns a set of indices in the source dataset for the bounding box.
}
\examples{
\dontrun{
Soon!
}
}

