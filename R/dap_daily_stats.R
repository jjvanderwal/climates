#' A wrapper function to execute daily_indices2geotiff in the climates package.
#'
#' This function takes an input spatiotemporal domain and an OPeNDAP datasource.
#' It returns a list of geotiff filenames that have been written to the current working directory.
#'
#' @param start a start year
#' @param end an end year
#' @param bbox_in a bounding box to be passed to CF_bbox_in
#' @param thresholds a named list to be passed to daily_indices
#' @param OPeNDAP_URI an OPeNDAP resource without any service parameters.
#' @param tmax_var a string of the tmax variable id
#' @param tmin_var a string of the tmin variable id
#' @param prcp_var a string of the prcp variable id
#' @param tave_var optional: a string of the tave variable id
#' @param NetCDF_output: Set to TRUE if NetCDF files are desired as output, otherwise will be geotiff.
#' @param Cells that are NA in only some time steps will be filled with the average of their neighbors.
#' @return A list of filenames that have been written to disk.
#' @export
#' @examples
#' Soon!
#' 
dap_daily_stats<-function(start,end,bbox_in,thresholds,OPeNDAP_URI,tmax_var,tmin_var,tave_var,prcp_var,NetCDF_output=FALSE, fill_nas=FALSE)
{
  
  te<-init_dap(OPeNDAP_URI,tmax_var,tmin_var,prcp_var, tave_var)
  ncdf4_handle<-te$ncdf4_handle; temp_unit_func<-te$temp_unit_func
  
  te<-request_bbox(ncdf4_handle,tmax_var,bbox_in); x1<-te$x1; y1<-te$y1; x2<-te$x2; 
  y2<-te$y2; coords_master<-te$coords_master; prj<-te$prj
  
  if(NetCDF_output==TRUE) init_NetCDF<-TRUE
  
  fileNames<-c()
  year_id<-1
  for (year in as.numeric(start):(as.numeric(end)))
  {
    #Call for time indices for this year
    te3<-request_time_bounds(ncdf4_handle,year,year+1); t_ind1 <- te3$t_ind1; t_ind2<-te3$t_ind2; 
    time_indices<-te3$time; origin<-te3$origin; time_PCICt<-as.PCICt(te3$time_posix,cal="gregorian")
    #Get the dap data
    te4<-get_dap_data(ncdf4_handle,x1,y1,x2,y2,t_ind1,t_ind2,tmax_var,tmin_var,prcp_var,tave_var=NULL,temp_unit_func,fill_nas)
    tmax_data<-te4$tmax_data; tmin_data<-te4$tmin_data; prcp_data<-te4$prcp_data; tave_data<-te4$tave_data
    
    mask<-is.na(prcp_data[1,])
    null_cells<-which(mask, arr.ind=TRUE)
     
    if(NetCDF_output==TRUE)
    {
      # Only Supports Lat/Lon with a 1d coordinate axis.
      if (!is.null(ncdf4_handle$dim$lon$vals) && length(dim(ncdf4_handle$dim$lon$vals)==1))
      {
        x_vals<-ncdf4_handle$dim$lon$vals[x1:x2]
        y_vals<-ncdf4_handle$dim$lat$vals[y1:y2]
      } else {
        stop('Unsupported Projection Found in Source Data. NetCDF output only supports Lat/Lon 1D axes.')
      }
      
      # Make sure the output uses normal lon conventions.
      x_vals[x_vals > 180] = x_vals[x_vals > 180] - 360
      
      # Something strange about the ordering of longitude and how ndf4 handles it.
      if(x_vals[1]-x_vals[2]>0) x_vals<-rev(x_vals)
      
      # Run Stats
      statsout<-data.frame(daily_indices(tmin=tmin_data, tmax=tmax_data, prec=prcp_data, tmean=tave_data, thresholds,time_PCICt))
      
      # Initialize NetCDF files on initial year.
      if(year_id==1)
      {
        te<-initialize_NetCDF(ncdf4_handle, thresholds, start, end, tmax_var, prcp_var, x_vals, y_vals)
        fileNames<-te$fileNames; x_vals<-te$x_vals; y_vals<-te$y_vals
      }
      
      # Iterate through files and write stats into them.
      for (fileName in fileNames)
      {
        ncdf_handle<-nc_open(fileName,write=TRUE)
        # Statistic name for whole file.
        stat<-gsub(".nc","",fileName)
        # Threshold list from input thresholds.
        threshold_list<-unlist(thresholds[paste(stat,'_thresh',sep='')])
        # Empty NetCDF data array.
        ncdf_data<-array(dim=c(length(x_vals),length(y_vals),length(threshold_list)))
        # Empty NetCDF thresholds array.
        ncdf_threshholds<-array(dim=c(length(thresholds[paste(stat,'_thresh',sep='')])))
        thresh_id<-1
        for (thresh in threshold_list)
        {
          # Populate part of NetCDF Array from statsout.
          ncdf_data_chunk<-unlist(statsout[paste(stat,'_',gsub('-','.',thresh),'_C',sep="")])
          ncdf_data_chunk[null_cells]<-NA
          ncdf_data[,,thresh_id]<-t(matrix(ncdf_data_chunk,ncol=abs(y2-y1)+1,nrow=abs(x2-x1)+1,byrow=TRUE))
          ncdf_threshholds[thresh_id]<-thresh
          thresh_id<-thresh_id+1
        }
        # Put data into NetCDF file at the appropriate year position.
        t<-ncvar_put(ncdf_handle,stat,as.integer(ncdf_data),c(1,1,1,year_id),c(-1,-1,-1,1))
        t<-ncvar_put(ncdf_handle,'threshold',ncdf_threshholds)
        t<-nc_close(ncdf_handle)
      }
      year_id<-year_id+1
    }
    else{
      #Run stats and write to geotiff.
      fileNames<-append(fileNames,daily_indices2geotiff(tmax_data,tmin_data,prcp_data,tave_data,thresholds, coords_master, prj, year, time_PCICt))
    }
  }
  return(fileNames)
}