\name{CF_bbox_grid}
\alias{CF_bbox_grid}

\title{ NetCDF-CF Grid Bounding Box Intersection }

\description{
	This function takes a bounding box in WGS84 lat/lon and the spatial coordinate variable values for a NetCDF-CF grid. It will return a list containing the index positions in the grid that fully contain the requested bounding box and information needed to construct a SpatialPixelsDataFrame to be written to a spatial data format like GeoTiff.
}

\usage{ CF_bbox_grid(x_vals,y_vals,bbox_in,grid_mapping_name=NULL,grid_mapping_atts=NULL) }

\arguments{
	\item{x_vals}{A 1d array of the x (or longitude) values of the grid.}
  \item{y_vals}{A 1d array of the y (or latitude) values of the grid.}
  \item{bbox_in}{A four-values vector in the format min lat/lon max lat/lon}
  \item{grid_mapping_name}{The NetCDF-CF Grid Mapping Name if there is one, WGS84 Lat/Lon is assumes given no Grid Mapping.}
  \item{grid_mapping_atts}{A list of attributes of the grid mapping variable as returned by ncdf or ncdf4's get attributes functions.}
}

\details{
	This function has been tested to work with lat/lon and Lambert Conformal Conic grid mappings. Other grid mappings have not been implemented.
}
\value{ A named list containing the following variables 
       \enumerate{
  	      \item{x1}
            Index position on the x (or longitude) axis for the minimum longitude.
          \item{y1}
            Index position on the y (or latitude) axis for the minimum latitude.
          \item{x2}
            Index position on the x (or longitude) axis for the maximum latitude.
          \item{y2}
            Index position on the y (or latitude) axis for the maximum longitude.
          \item{x_index}
            The values of the x coordinate variable for the subset of the grid that satisfies the bounding box.
          \item{y_index}
            The values of the y coordinate variable for the subset of the grid that satisfies the bounding box.
          \item{prj}
            A proj4 string for the x/y coordinates for use in creating a SpatialPixelsDataFrame.
}

\author{ David Blodgett \email{dblodgett@usgs.gov} }

\examples{

Using ncdf4 and remote data:

library("ncdf4")
library("rgdal")
bbox_in <- c(-87,41,-89,43)
OPeNDAP_URI<-"http://cida.usgs.gov/thredds/dodsC/prism"
ncdf4_handle <- nc_open(OPeNDAP_URI)
x_vals<-ncdf4_handle$dim$lon$vals
y_vals<-ncdf4_handle$dim$lat$vals
CF_bbox_grid(x_vals,y_vals,bbox_in)

Using sample data that is projected:

data('CF_bbox_grid_example.Rdata')
bbox_indices<-CF_bbox_grid(x_vals,y_vals,bbox_in,grid_mapping_name,grid_mapping_atts)

}

