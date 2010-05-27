#convert the anomoly work to a dataframe with xyz where the z columns are the different time values
#input is a object from append.nc or extract.monthly.averages or retrieve.nc
nc2dataframe = function (nc) {
	#ensure nc is from retrieve.nc or similar
	if (all(c('dat','tim','lat', 'lon') %in% names(nc))==FALSE) stop('nc must have objects named dat, lat, lon and tim as from retrieve.nc of clim.pact package, append.nc or extract.monthly.averages')
	#do the work
	out = expand.grid(lon = nc$lon,lat = nc$lat)
	for (tim in nc$tim) out[[paste('tim.',tim,sep='')]] = as.vector(nc$dat[tim,,])
	return(out)
}
