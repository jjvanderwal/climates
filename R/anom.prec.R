#define a function to extract anomolies for temperature and precipitaiton
#nc1 & nc2 are current and future (respectively)
#inputs are from retrieve.nc, append.nc or extract.monthly.averages
anom.prec <- 
function(nc1,nc2) { #precipitation of future (nc2) as a proportion of current (nc1)
	#ensure nc1 & nc2 are from retrieve.nc
	if (all(c('dat','tim','lat', 'lon') %in% names(nc1))==FALSE) stop('nc1 must have objects named dat, lat, lon and tim as from retrieve.nc of clim.pact package, append.nc or extract.monthly.averages')
	if (all(c('dat','tim','lat', 'lon') %in% names(nc2))==FALSE) stop('nc2 must have objects named dat, lat, lon and tim as from retrieve.nc of clim.pact package, append.nc or extract.monthly.averages')
	#do the work
	out = nc1; out$dat = nc2$dat / nc1$dat
	class(out) = unique(c(class(out),'nc'))
	return(out)
}
