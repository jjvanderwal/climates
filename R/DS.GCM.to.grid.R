##need a function to downscale data to regular grid
#nc should be from extend.global.data
DS.GCM.to.grid = function(nc,lon=c(-180,180),lat=c(-90,90),cellsize=0.2,type=2) {
	#ensure nc is from retrieve.nc or similar
	if (all(c('dat','tim','lat', 'lon') %in% names(nc))==FALSE) stop('nc must have objects named dat, lat, lon and tim as from retrieve.nc of clim.pact package, append.nc or extract.monthly.averages')
	if (!(type %in% 1:3)) stop('type must be 1,2 or 3... see help file')
	#do the work
	lat = seq(lat[1]+0.5*cellsize,lat[2]-0.5*cellsize,cellsize)
	lon = seq(lon[1]+0.5*cellsize,lon[2]-0.5*cellsize,cellsize)
	out = array(0,dim=c(length(nc$tim),length(lat),length(lon)))
	for (ii in 1:length(nc$tim)) { out[ii,,] = interp2grid(nc$dat[ii,,],lon,lat,nc$lon,nc$lat,type=type) }
	out = list(dat=out,lon=lon,lat=lat,tim=nc$tim)
	class(out) = unique(c(class(out),'nc'))
	return(out)
}

DS.GCM.to.pnt = function(nc,lon,lat,type=2) {
	#ensure nc is from retrieve.nc or similar
	if (all(c('dat','tim','lat', 'lon') %in% names(nc))==FALSE) stop('nc must have objects named dat, lat, lon and tim as from retrieve.nc of clim.pact package, append.nc or extract.monthly.averages')
	if (!(type %in% 1:3)) stop('type must be 1,2 or 3... see help file')
	#do the work
	out = data.frame(lon=lon,lat=lat)
	for (ii in 1:length(nc$tim)) { out[paste('tim.',nc$tim[ii],sep='')] = interp2pnt(nc$dat[ii,,],lon,lat,nc$lon,nc$lat,type=type) }
	return(out)
}
