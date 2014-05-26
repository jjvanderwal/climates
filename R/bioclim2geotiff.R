bioclim2geotiff<-function(tmax_data,tmin_data,prcp_data,tave_data,bioclims, coords_master, prj, year)
{
  #remove cells that are NaNs.
  mask<-!is.na(prcp_data[,1])
  masked_coords<-coords_master[mask,]
  tmax_data<-tmax_data[mask,]
  tmin_data<-tmin_data[mask,]
  prcp_data<-prcp_data[mask,]
  tave_data<-tave_data[mask,]
  # Run BioClim
  bioclim_out<-data.frame(bioclim(tmin=tmin_data, tmax=tmax_data, prec=prcp_data, tmean=tave_data, bioclims))
  colnames(bioclim_out)<-paste('bioclim_',bioclims, sep='')
  fileNames<-c()
  for (bclim in names(bioclim_out))
  {
    file_name<-paste(bclim,'_',as.character(year),'.tif',sep='') 
    fileNames<-append(fileNames,file_name)
    writeGDAL(SpatialPixelsDataFrame(SpatialPoints(masked_coords, proj4string = CRS(prj)), bioclim_out[bclim], tolerance=0.0001),file_name)
  }
  return(fileNames)
}