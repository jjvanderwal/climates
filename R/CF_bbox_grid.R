#' NetCDF-CF Grid Bounding Box Intersection
#' 
#' This function takes a bounding box in WGS84 lat/lon and the spatial
#' coordinate variable values for a NetCDF-CF grid. It will return a list
#' containing the index positions in the grid that fully contain the requested
#' bounding box and information needed to construct a SpatialPixelsDataFrame to
#' be written to a spatial data format like GeoTiff.
#' 
#' This function has been tested to work with lat/lon and Lambert Conformal
#' Conic grid mappings. Other grid mappings have not been implemented.
#' 
#' @param x_vals A 1d array of the x (or longitude) values of the grid.
#' @param y_vals A 1d array of the y (or latitude) values of the grid.
#' @param bbox_in A four-values vector in the format min lat/lon max lat/lon
#' @param grid_mapping_name The NetCDF-CF Grid Mapping Name if there is one,
#' WGS84 Lat/Lon is assumes given no Grid Mapping.
#' @param grid_mapping_atts A list of attributes of the grid mapping variable
#' as returned by ncdf or ncdf4's get attributes functions.
#' @return A named list containing the following variables 
#' \enumerate{
#'  \item{'x1'}{Index position on the x (or longitude) axis for the minimum longitude.}
#'  \item{'y1'}{Index position on the y (or latitude) axis for the minimum latitude.}
#'  \item{'x2'}{Index position on the x (or longitude) axis for the maximum latitude.} 
#'  \item{'y2'}{Index position on the y (or latitude) axis for the maximum longitude.}
#'  \item{'coords_master'}{The coordinates of the subset grid prepared for creation of an sp SpatialPoints object.}
#'  \item{'prj'}{A proj4 string for the x/y coordinates for use in creating a SpatialPixelsDataFrame.}
#' }
#' @author David Blodgett \email{dblodgett@@usgs.gov}
#' @export
#' @examples
#' 
#' 
#' Using ncdf4 and remote data:
#' 
#' library("ncdf4")
#' bbox_in <- c(-87,41,-89,43)
#' OPeNDAP_URI<-"http://cida.usgs.gov/thredds/dodsC/prism"
#' ncdf4_handle <- nc_open(OPeNDAP_URI)
#' x_vals<-ncdf4_handle$dim$lon$vals
#' y_vals<-ncdf4_handle$dim$lat$vals
#' CF_bbox_grid(x_vals,y_vals,bbox_in)
#' 
#' Using sample data that is projected:
#' 
#' data(CF_bbox_grid_example, package='climates')
#' bbox_indices<-CF_bbox_grid(x_vals,y_vals,bbox_in,grid_mapping_name,grid_mapping_atts)
#' 
#' 
CF_bbox_grid<-function(x_vals,y_vals,bbox_in,grid_mapping_name=NULL,grid_mapping_atts=NULL)
{
  if (!is.null(grid_mapping_name))
  {
    paste(grid_mapping_name)
    #Supports Lambert Conformal Conic from NetCDF-CF.
    if (grid_mapping_name=="lambert_conformal_conic")
    {
      # Get lambert conformal attributes.
      longitude_of_central_meridian<-grid_mapping_atts$longitude_of_central_meridian
      latitude_of_projection_origin<-grid_mapping_atts$latitude_of_projection_origin
      standard_parallel<-grid_mapping_atts$standard_parallel
      false_easting<-grid_mapping_atts$false_easting
      false_northing<-grid_mapping_atts$false_northing
      longitude_of_prime_meridian<-grid_mapping_atts$longitude_of_prime_meridian
      semi_major_axis<-grid_mapping_atts$semi_major_axis
      if (is.null(semi_major_axis)) { semi_major_axis<-6378137.0 }
      inverse_flattening<-grid_mapping_atts$inverse_flattening
      if (is.null(inverse_flattening)) { inverse_flattening<-298.257223563 }
      # Can have two or one standard parallels. Based on this, create a proj4 string.
      if (length(standard_parallel==2))
      {
        prj <- paste("+proj=lcc +lat_1=", standard_parallel[1],
                     " +lat_2=", standard_parallel[2],
                     " +lat_0=", latitude_of_projection_origin,
                     " +lon_0=", longitude_of_central_meridian,
                     " +x_0=", false_easting,
                     " +y_0=", false_northing,
                     " +a=", semi_major_axis,
                     " +f=", (1/inverse_flattening),
                     sep='')
      } else
      {
        prj <- paste("+proj=lcc +lat_1=", standard_parallel[1],
                     " +lat_2=", standard_parallel[1],
                     " +lat_0=", latitude_of_projection_origin,
                     " +lon_0=", longitude_of_central_meridian,
                     " +x_0=", false_easting,
                     " +y_0=", false_northing,
                     " +a=", semi_major_axis,
                     " +f=", (1/inverse_flattening),
                     sep='') 
      }
      # Project bbox and unproject data-source range to check intersection.
      # preparing bbox for projection.
      bbox_unproj<-data.frame(matrix(c(bbox_in, bbox_in[1],bbox_in[4],bbox_in[3],bbox_in[2]),ncol=2,byrow=TRUE))
      colnames(bbox_unproj)<-c("x","y")
      coordinates(bbox_unproj)<-c("x","y")
      proj4string(bbox_unproj) <- CRS("+init=epsg:4326")
      bbox_proj<-spTransform(bbox_unproj,CRS(prj)) # Project bbox.
      bbox_proj_coords<-coordinates(bbox_proj)
      # Get projected bounds
      min_dods_x<-min(x_vals)
      max_dods_x<-max(x_vals)
      min_dods_y<-min(y_vals)
      max_dods_y<-max(y_vals)
      # Prepare projected bounds to be unprojected.
      x_y_range<-data.frame(matrix(c(min_dods_x,min_dods_y,max_dods_x,max_dods_y,min_dods_x,max_dods_y,max_dods_x,min_dods_y),ncol=2,byrow=TRUE))
      colnames(x_y_range)<-c("x","y")
      coordinates(x_y_range)<-c("x","y")
      proj4string(x_y_range) <- CRS(prj)
      x_y_range_unproj<-spTransform(x_y_range,CRS("+init=epsg:4326"))
      x_y_range_unproj_coords<-coordinates(x_y_range_unproj)
      # Check lower left.
      if (bbox_proj_coords[1]<min_dods_x || bbox_proj_coords[1]>max_dods_x) stop(paste("Submitted minimum longitude",bbox_in[1], "is outside the dataset's minimum",x_y_range_unproj_coords[1]))
      if (bbox_proj_coords[3]<min_dods_y || bbox_proj_coords[3]>max_dods_y) stop(paste("Submitted minimum latitude",bbox_in[2], "is outside the dataset's minimum",x_y_range_unproj_coords[2]))
      # Check upper right.
      if (bbox_proj_coords[2]<min_dods_x || bbox_proj_coords[2]>max_dods_x) stop(paste("Submitted maximum longitude",bbox_in[3], "is outside the dataset's maximum",x_y_range_unproj_coords[3]))
      if (bbox_proj_coords[4]<min_dods_y || bbox_proj_coords[4]>max_dods_y) stop(paste("Submitted maximum latitude",bbox_in[4], "is outside the dataset's maximum",x_y_range_unproj_coords[4]))
      # Check upper left.
      if (bbox_proj_coords[5]<min_dods_x || bbox_proj_coords[5]>max_dods_x) stop(paste("Submitted minimum longitude",bbox_in[1], "is outside the dataset's minimum",x_y_range_unproj_coords[1]))
      if (bbox_proj_coords[6]<min_dods_y || bbox_proj_coords[6]>max_dods_y) stop(paste("Submitted minimum latitude",bbox_in[2], "is outside the dataset's minimum",x_y_range_unproj_coords[2]))
      # Check lower right.
      if (bbox_proj_coords[7]<min_dods_x || bbox_proj_coords[7]>max_dods_x) stop(paste("Submitted maximum longitude",bbox_in[3], "is outside the dataset's maximum",x_y_range_unproj_coords[3]))
      if (bbox_proj_coords[8]<min_dods_y || bbox_proj_coords[8]>max_dods_y) stop(paste("Submitted maximum latitude",bbox_in[4], "is outside the dataset's maximum",x_y_range_unproj_coords[4]))
      x1 <- which(abs(x_vals-bbox_proj_coords[1])==min(abs(x_vals-bbox_proj_coords[1])))
      y1 <- which(abs(y_vals-bbox_proj_coords[2])==min(abs(y_vals-bbox_proj_coords[2])))
      x2 <- which(abs(x_vals-bbox_proj_coords[3])==min(abs(x_vals-bbox_proj_coords[3])))                 
      y2 <- which(abs(y_vals-bbox_proj_coords[4])==min(abs(y_vals-bbox_proj_coords[4])))
      # Check to see if multiple indices were found and buffer out if they were.
      if(length(x1)==2) if((bbox_proj_coords[1]-x_vals[x1[1]])>(bbox_proj_coords[1]-x_vals[x1[2]])) x1<-x1[1] else x1<-x1[2]  
      if(length(y1)==2) if((bbox_proj_coords[2]-y_vals[y1[1]])>(bbox_proj_coords[2]-y_vals[y1[2]])) y1<-y1[1] else y1<-y1[2]
      if(length(x2)==2) if((bbox_proj_coords[3]-x_vals[x2[1]])>(bbox_proj_coords[3]-x_vals[x2[2]])) x2<-x2[1] else x2<-x2[2]
      if(length(y2)==2) if((bbox_proj_coords[4]-y_vals[y2[1]])>(bbox_proj_coords[4]-y_vals[y2[2]])) y2<-y2[1] else y2<-y2[2]
      x_index<-x_vals[x1:x2]
      y_index<-y_vals[y1:y2]
      
      #Doesn't support any other grid mappings.
    } else
    {
      stop('Unsupported Projection Found in Source Data.')
    }
  } else #Given no grid mapping, assume WGS84. Should check if grid mapping is WGS84, not manye datasets like this.
  {
    prj<-"+init=epsg:4326"
    #Check if data uses 0:360 lat/lon
    if (max(x_vals)>180 || max(y_vals)>180) 
    {
      bbox_in[1]=bbox_in[1]+360
      bbox_in[3]=bbox_in[3]+360
    }
    if (bbox_in[1]<min(x_vals)) stop(paste("Submitted minimum longitude",bbox_in[1], "is outside the dataset's minimum",min(x_vals)))
    if (bbox_in[2]<min(y_vals)) stop(paste("Submitted minimum latitude",bbox_in[2], "is outside the dataset's minimum",min(y_vals)))
    if (bbox_in[3]>max(x_vals)) stop(paste("Submitted maximum longitude",bbox_in[3], "is outside the dataset's maximum",max(x_vals)))
    if (bbox_in[4]>max(y_vals)) stop(paste("Submitted maximum latitude",bbox_in[4], "is outside the dataset's maximum",max(y_vals)))
    # Search for x/y indices
    lon1_index <- which(abs(x_vals-bbox_in[1])==min(abs(x_vals-bbox_in[1])))
    lat1_index <- which(abs(y_vals-bbox_in[2])==min(abs(y_vals-bbox_in[2])))
    lon2_index <- which(abs(x_vals-bbox_in[3])==min(abs(x_vals-bbox_in[3])))                 
    lat2_index <- which(abs(y_vals-bbox_in[4])==min(abs(y_vals-bbox_in[4])))
    # Check to see if multiple indices were found and buffer out if they were.
    if(length(lon1_index)==2) if((bbox_in[1]-x_vals[lon1_index[1]])>(bbox_in[1]-x_vals[lon1_index[2]])) lon1_index<-lon1_index[1] else lon1_index<-lon1_index[2]  
    if(length(lat1_index)==2) if((bbox_in[2]-y_vals[lat1_index[1]])>(bbox_in[2]-y_vals[lat1_index[2]])) lat1_index<-lat1_index[1] else lat1_index<-lat1_index[2]
    if(length(lon2_index)==2) if((bbox_in[3]-x_vals[lon2_index[1]])>(bbox_in[3]-x_vals[lon2_index[2]])) lon2_index<-lon2_index[1] else lon2_index<-lon2_index[2]
    if(length(lat2_index)==2) if((bbox_in[4]-y_vals[lat2_index[1]])>(bbox_in[4]-y_vals[lat2_index[2]])) lat2_index<-lat2_index[1] else lat2_index<-lat2_index[2]
    x_index<-x_vals[lon1_index:lon2_index]
    y_index<-y_vals[lat1_index:lat2_index]
    x1<-lon1_index
    y1<-lat1_index
    x2<-lon2_index
    y2<-lat2_index
  }
  # Check for regular grid.
  dif_xs = mean(diff(x_index))
  dif_ys = mean(diff(y_index))
  if (abs(abs(dif_ys)-abs(dif_xs))>0.00001) stop('The data source appears to be an irregular grid, this datatype is not supported.')
  
  # Create x/y points for cells for geotiff files to be written.
  coords_master <- array(dim=c(length(x_index)*length(y_index),2))
  coords_master[,1]<-rep(rev(x_index)+dif_ys/2,length(y_index))
  coords_master[,2]<-rep(rev(y_index)-dif_ys/2,each=length(x_index))
  return(list(x1=x1,y1=y1,x2=x2,y2=y2,coords_master=coords_master,prj=prj))
}
