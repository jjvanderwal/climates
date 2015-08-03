#' A function to initialize a NetCDF file for daily indices.
#'
#' This function initializes a set of NetCDF files for daily_indices. If start and end are given, it returns annual time steps.
#' If period is given, it creates time steps that span the years given in period. It returns a set of file names and the x/y coordinates.
#'
#' @param ncdf4_handle an initialized ncdf4 object.
#' @param thresholds a named list to be passed to daily_indices
#' @param start a start year
#' @param end an end year
#' @param tmax_var a string of the tmax variable id
#' @param prcp_var a string of the prcp variable id
#' @param periods a vector of years to use as time steps. (optional)
#' @return A named list of including fileNames, x_vals, and y_vals for the NetCDF files that have been initiated.
#' @export
#' @examples
#' \dontrun{
#' Soon!
#' }
#' 
initialize_NetCDF<-function(ncdf4_handle, thresholds, start=FALSE, end=FALSE, tmax_var=FALSE, prcp_var=FALSE, x_vals, y_vals, periods=list(), t_units=FALSE, p_units=FALSE){
  # Create shell of a netCDF file.
  latDim<-ncdim_def('lat','degrees_north',y_vals,longname='lat')
  lonDim<-ncdim_def('lon','degrees_east',x_vals,longname='lon')
  fileNames<-c()
  for (stat in names(thresholds))
  {
    varName<-gsub("_thresh","",stat)
    fileName<-paste(varName,'.nc',sep='')
    fileNames<-append(fileNames,fileName)
    #Create Time Coordinate Variables
    time_units<-get_time_dim(ncdf4_handle)$time_units
    if(length(periods)==0){
      time_var<-c(1:(as.numeric(end)-as.numeric(start)))
      time_bounds_2<-c(1:(as.numeric(end)-as.numeric(start)))
      ind=1
      for (year_var in as.numeric(start):(as.numeric(end)))
      {
        time_var[ind]<-as.integer(julian(strptime(paste(year_var,'-01-01 00:00',sep=''), '%Y-%m-%d %H:%M'), origin<-strptime(paste(time_units[3],'00:00:00',sep=' '), '%Y-%m-%d %H:%M:%S')))
        time_bounds_2[ind]<-as.integer(julian(strptime(paste(year_var+1,'-01-01 00:00',sep=''), '%Y-%m-%d %H:%M'), origin<-strptime(paste(time_units[3],'00:00:00',sep=' '), '%Y-%m-%d %H:%M:%S')))
        ind<-ind+1
      }
    }
    else {
      time_var<-c(1:(length(periods)-1))
      time_bounds_2<-c(1:(length(periods)-1))
      for(year_ind in 1:(length(periods)-1))
      {
        time_var[year_ind]<-as.integer(julian(strptime(paste(periods[year_ind],'-01-01 00:00',sep=''), '%Y-%m-%d %H:%M'), origin<-strptime(paste(time_units[3],'00:00:00',sep=' '), '%Y-%m-%d %H:%M:%S')))
        time_bounds_2[year_ind]<-as.integer(julian(strptime(paste(periods[year_ind+1],'-01-01 00:00',sep=''), '%Y-%m-%d %H:%M'), origin<-strptime(paste(time_units[3],'00:00:00',sep=' '), '%Y-%m-%d %H:%M:%S')))
      }
    }
    time_bounds<-array(dim=c(2,length(time_var)))
    time_bounds[1,]<-time_var
    time_bounds[2,]<-time_bounds_2
    # Create Dimensions and Coordinate Variables for Time
    time_units_str<-paste(time_units[1],time_units[2],time_units[3],sep=' ')
    timeDim <- ncdim_def('time', time_units_str, time_var)
    timeBoundsDim<-ncdim_def('time_bounds','', 1:2,create_dimvar=FALSE)
    timeBoundsVar<-ncvar_def('time_bounds','',list(timeDim,timeBoundsDim),prec='integer')
    threshDim<-ncdim_def('threshold','',1:length(unlist(thresholds[stat])),create_dimvar=FALSE)
    if(grepl('tmax',stat) | grepl('tmin',stat) | grepl('degree',stat) | grepl('season',stat)) { 
      if(t_units==FALSE) {
        threshVarUnits<-ncatt_get(ncdf4_handle, tmax_var,'units')$value 
      }
      else { 
        threshVarUnits=t_units
      }
      threshVarStandName<-'air_temperature'
    } else if(grepl('prcp',stat)) { 
      if(p_units==FALSE) {
        threshVarUnits<-ncatt_get(ncdf4_handle, prcp_var,'units')$value
      } 
      else {
        threshVarUnits=p_units
      }
      threshVarStandName<-'lwe_thickness_of_precipitation_amount'
    } else { 
      stop('This should not have happened. A threshold id that should not exist snuck in somehow.') }
    threshVar<-ncvar_def('threshold',threshVarUnits,threshDim,prec='float')
    # Create An Actual Data Variable
    dataVar<-ncvar_def(varName,'days',list(lonDim,latDim,threshDim,timeDim),-1,prec='integer')
    # Create file
    ncdf_handle<-nc_create(fileName,list(timeBoundsVar,threshVar,dataVar))
    # Add Attributes
    t<-ncatt_put(ncdf_handle,'threshold','positive','up',prec='text')
    t<-ncatt_put(ncdf_handle,'time','climatology','time_bounds',prec='text')
    t<-ncatt_put(ncdf_handle,varName,'missing_value','-1',prec='integer')
    t<-ncatt_put(ncdf_handle,varName,'coordinates','time, threshold, lat, lon',prec='text')
    t<-ncatt_put(ncdf_handle,'threshold','standard_name',threshVarStandName,prec='text')
    t<-ncatt_put(ncdf_handle,'lat','standard_name','latitude',prec='text')
    t<-ncatt_put(ncdf_handle,'lon','standard_name','longitude',prec='text')
    t<-ncatt_put(ncdf_handle,0,'Conventions','CF-1.0', prec='text')
    # Add Time_bounds values
    t<-ncvar_put(ncdf_handle,'time_bounds',time_bounds)
    t<-nc_close(ncdf_handle)
  }
  return(list(fileNames=fileNames, x_vals=x_vals, y_vals=y_vals))
}