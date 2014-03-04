\name{bioclim2geotiff}
\alias{bioclim2geotiff}

\title{ Bioclim to GeoTiff }

\description{
	This function wraps the bioclim function. It writes data to a geotiff file in the working directory.
}

\usage{ bioclim2geotiff(tmax_data,tmin_data,prcp_data,tave_data,bioclims, coords_master, prj, year) }

\arguments{
	\item{tmin}{a data.frame or matrix with 12 or 52 columns representing monthly or weekly minimum temperature data; rows represent different locations.}
  \item{tmax}{a data.frame or matrix as with \code{tmin} representing maximum temperature data.}
	\item{prec}{a data.frame or matrix as with \code{tmin} representing precipitation data.}
	\item{tmean}{a data.frame or matrix as with \code{tmin} representing mean temperature data.}
	\item{vois}{a vector of values between 1 & 19 defining the bioclimatic variables to be calculated; see details for variable description.}
  \item{coordinates}{Coordinates of grid cells as output by CF_bbox_grid.}
  \item{projection}{A Proj4 string for the spatial reference of the input string.}
  \item{year}{A year to use in labeling the output file.}
}

\value{ A character vector of the files that were created.}

\author{ David Blodgett \email{dblodgett@usgs.gov} }

\examples{

data(bioclim2geotiff_example, package='climates')
fileNames<-bioclim2geotiff(tmax_data,tmin_data,prcp_data,tave_data,bioclims, coords_master, prj, year)

}

