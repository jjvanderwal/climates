#' A wrapper function to execute bioclim in the climates package.
#'
#' This function takes an input spatiotemporal domain and an OPeNDAP datasource.
#' It returns a list of geotiff filenames that have been written to the current working directory.
#'
#' @param start a start year
#' @param end an end year
#' @param bbox_in a bounding box to be passed to CF_bbox_in
#' @param bioclims a list of bioclims to be passed to the bioclim function.
#' @param OPeNDAP_URI an OPeNDAP resource without any service parameters.
#' @param tmax_var a string of the tmax variable id
#' @param tmin_var a string of the tmin variable id
#' @param prcp_var a string of the prcp variable id
#' @param tave_var optional: a string of the tave variable id
#' @return A list of filenames that have been written to disk.
#' @export
#' @examples
#' Soon!
#' 
dap_bioclim<-function(start,end,bbox_in,bioclims,OPeNDAP_URI,tmax_var,tmin_var,tave_var,prcp_var)
{
  #Check if Bioclims in allowable set
  valid_bioclims<-c(1:19)
  if (any(!bioclims %in% valid_bioclims)) stop("Invalid Bioclim ids were submitted.")
  
  te<-init_dap(OPeNDAP_URI,tmax_var,tmin_var,prcp_var,tave_var)
  ncdf4_handle<-te$ncdf4_handle; temp_unit_func<-te$temp_unit_func
  
  te2<-request_bbox(ncdf4_handle,tmax_var,bbox_in); x1<-te2$x1; y1<-te2$y1; x2<-te2$x2; 
  y2<-te2$y2; coords_master<-te2$coords_master; prj<-te2$prj
  
  fileNames<-c()
  for (year in as.numeric(start):(as.numeric(end)))
  {
    #Call for time indices for this year
    te3<-request_time_bounds(ncdf4_handle,year,year+1); t_ind1 <- te3$t_ind1; t_ind2<-te3$t_ind2; 
    time_indices<-te3$time; origin<-te3$origin
    
    #Get the dap data
    te4<-get_dap_data(ncdf4_handle,x1,y1,x2,y2,t_ind1,t_ind2,tmax_var,tmin_var,prcp_var,tave_var=NULL,temp_unit_func)
    tmax_data<-te4$tmax_data; tmin_data<-te4$tmin_data; prcp_data<-te4$prcp_data; tave_data<-te4$tave_data
    if (dim(time_indices)>12)
    {
      # Convert daily data to monthly in preperation for bioclim functions.
      time_indices<-floor(time_indices)
      tmax_data<-daily2monthly(tmax_data, time_indices, origin)
      tmin_data<-daily2monthly(tmin_data, time_indices, origin)
      prcp_data<-daily2monthly(prcp_data, time_indices, origin)
      tave_data<-daily2monthly(tave_data, time_indices, origin)
    }
    else
    {
      #Transpose
      tmax_data<-t(tmax_data)
      tmin_data<-t(tmin_data)
      prcp_data<-t(prcp_data)
      tave_data<-t(tave_data)
    }
    #Run Bioclim and write to geotiff.
    fileNames<-append(fileNames,bioclim2geotiff(tmax_data,tmin_data,prcp_data,tave_data,bioclims, coords_master, prj, year))
  }
  return(fileNames)
}