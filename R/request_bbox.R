#' An OPeNDAP wrapper for the Climates function CF_bbox_grid.
#'
#' This function takes an open ncdf4 data source, a representative variable and a bounding box.
#' It returns a set of indices in the source dataset for the bounding box.
#'
#' @param ncdf4_handle ncdf4 object.
#' @param rep_var a variable in the necdf4 source that is representative.
#' @param bbox_in a bounding box to be passwed to CF_bbox_grid.
#' @return bbox_indices a set of indices from the source dataset for the bounding box.
#' @export
#' @examples
#' \dontrun{
#' Soon!
#' }
request_bbox<-function(ncdf4_handle,rep_var,bbox_in) 
{
  grid_mapping<-ncatt_get(ncdf4_handle, rep_var,'grid_mapping')
  if (!is.null(grid_mapping) && !is.null(ncdf4_handle$dim$x$vals))
  {
    grid_mapping_name<-grid_mapping$value
    grid_mapping_atts<-ncatt_get(ncdf4_handle, grid_mapping_name)
    x_vals<-ncdf4_handle$dim$x$vals
    y_vals<-ncdf4_handle$dim$y$vals
    bbox_indices<-CF_bbox_grid(x_vals,y_vals,bbox_in,grid_mapping_name,grid_mapping_atts)
    # Supports lat/lon that is NOT a 2D coordinate variable.
  } else if (!is.null(ncdf4_handle$dim$lon$vals) && length(dim(ncdf4_handle$dim$lon$vals)==1))
  {
    x_vals<-ncdf4_handle$dim$lon$vals
    y_vals<-ncdf4_handle$dim$lat$vals
    bbox_indices<-CF_bbox_grid(x_vals,y_vals,bbox_in)
  }
  return(bbox_indices)
}