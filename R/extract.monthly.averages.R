#need a function to create monthy averages given a set of years
#give it the nc file from retrieve.nc, min year and max year
extract.monthly.averages <- function(nc,min.year,max.year) {
	#ensure nc1 & nc2 are from retrieve.nc
	if (all(c('dat','tim','yy','mm', 'lat', 'lon') %in% names(nc))==FALSE) stop('nc must have objects named dat, lat, lon, tim, yy and mm as from retrieve.nc of clim.pact package')
	#do the work
	#track lat, long and time
	lat = nc$lat
	lon = nc$lon
	mm = 1:12
	#create an output array
	out = array(0,dim=c(length(mm),length(lat),length(lon)))
	#get the array subset which is from the start year to end year
	years = which(nc$yy %in% min.year:max.year)
	#cycle through all the data in those years
	for (ii in years) out[nc$mm[ii],,] = out[nc$mm[ii],,] + nc$dat[ii,,]
	#now get the average by deviding by the number of months
	for (ii in mm) out[ii,,] = out[ii,,] / (max.year-min.year+1)
	#return the information
	out = list(dat=out,lon=nc$lon,lat=nc$lat,tim=mm)
	class(out) = unique(c(class(out),'nc'))
	return(out)
}
