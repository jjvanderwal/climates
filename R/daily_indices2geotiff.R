daily_indices2geotiff<-function(tmax_data,tmin_data,prcp_data,tave_data,thresholds,coords_master,prj,year,time_PCICt)
{
  tmax_data<-t(tmax_data)
  tmin_data<-t(tmin_data)
  prcp_data<-t(prcp_data)
  tave_data<-t(tave_data)
  #remove cells that are NaNs.
  mask<-!is.na(prcp_data[,1])
  masked_coords<-coords_master[mask,]
  tmax_data<-tmax_data[mask,]
  tmin_data<-tmin_data[mask,]
  prcp_data<-prcp_data[mask,]
  tave_data<-tave_data[mask,]
  tmax_data<-t(tmax_data)
  tmin_data<-t(tmin_data)
  prcp_data<-t(prcp_data)
  tave_data<-t(tave_data)
  # Run Stats
  statsout<-data.frame(daily_indices(tmin=tmin_data, tmax=tmax_data, prec=prcp_data, tmean=tave_data, thresholds,time_PCICt))
  fileNames<-c()
  for (stat in colnames(statsout))
  {
    file_name<-paste(stat,'_',as.character(year),'.tif',sep='') 
    fileNames<-append(fileNames,file_name)
    writeGDAL(SpatialPixelsDataFrame(SpatialPoints(masked_coords, proj4string = CRS(prj)), statsout[stat], tolerance=0.0001),file_name)
  }
  return(fileNames)
}