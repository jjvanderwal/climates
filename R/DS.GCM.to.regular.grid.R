##need a function to downscale data to regular grid
#nc should be from extend.global.data
DS.GCM.to.regular.grid = function(nc,cellsize=0.2,type=2) {
	#ensure nc is from retrieve.nc or similar
	if (all(c('dat','tim','lat', 'lon') %in% names(nc))==FALSE) stop('nc must have objects named dat, lat, lon and tim as from retrieve.nc of clim.pact package, append.nc or extract.monthly.averages')
	if (!(type %in% 1:3)) stop('type must be 1,2 or 3... see help file')
	#do the work
	lat = seq(-90+0.5*cellsize,90-0.5*cellsize,cellsize)
	lon = seq(-180+05*cellsize,180-0.5*cellsize,cellsize)
	out = array(0,dim=c(length(nc$tim),length(lat),length(lon)))
	for (ii in 1:length(nc$tim)) { out[ii,,] = interp2grid(nc$dat[ii,,],lon,lat,nc$lon,nc$lat,type=type) }
	return(list(dat=out,lon=lon,lat=lat,tim=nc$tim))
}
