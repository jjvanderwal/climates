#this is a function to recreate bioclim variables

bioclim <-
function(tmin=NULL, tmax=NULL, prec=NULL, tmean=NULL, vois=1:19, cov=FALSE, t.as.int=TRUE, period="month") {
	tmin.vois = c(2,3,5,6)
	tmax.vois = c(2,3,5,7)
	prec.vois = c(8,9,12:19)
	tmean.vois = c(1,4:6,8:11,18:19)
	tsize = NULL
	#check if data is available for vois defined
	if (any(vois %in% tmin.vois)) { #tmin is needed so check it
		if (is.null(tmin)) {
			stop('tmin is needed for the variables selected')
		} else if (is.data.frame(tmin) | is.matrix(tmin)) { #check if tmin is a dataframe or matrix
			tmin = as.matrix(tmin) #convert it to a matrix
			if (!dim(tmin)[2] %in% c(12,52)) stop('tmin must have 12 columns -- one for each month')	
			tsize = c(tsize,dim(tmin)[1])
		} else { stop('tmin must be a data.frame or matrix') }
	}
	if (any(vois %in% tmax.vois)) { #tmax is needed so check it
		if (is.null(tmax)) {
			stop('tmax is needed for the variables selected')
		} else if (is.data.frame(tmax) | is.matrix(tmax)) { #check if tmax is a dataframe or matrix
			tmax = as.matrix(tmax) #convert it to a matrix
			if (!dim(tmax)[2] %in% c(12,52)) stop('tmax must have 12 columns -- one for each month')	
			tsize = c(tsize,dim(tmax)[1])
		} else { stop('tmax must be a data.frame or matrix') }	
	}
	if (any(vois %in% prec.vois)) { #prec is needed so check it
		if (is.null(prec)) {
			stop('prec is needed for the variables selected')
		} else if (is.data.frame(prec) | is.matrix(prec)) { #check if prec is a dataframe or matrix
			prec = as.matrix(prec) #convert it to a matrix
			if (!dim(prec)[2] %in% c(12,52)) stop('prec must have 12 columns -- one for each month')	
			tsize = c(tsize,dim(prec)[1])
		} else { stop('prec must be a data.frame or matrix') }
	}
	if (any(vois %in% tmean.vois)) { #tmean is needed so check it
		if (is.null(tmean) & (is.null(tmin) | is.null(tmax))) {
			stop('tmean (or both tmin & tmax) is needed for the variables selected')
		} else if (is.null(tmean)) { #if tmean is still null create it
			tmean = (tmax+tmin)/2 
		} else if (is.data.frame(tmean) | is.matrix(tmean)) { #check if tmean is a dataframe or matrix
			tmean = as.matrix(tmean) #convert it to a matrix
			if (!dim(tmean)[2] %in% c(12,52)) stop('tmean must have 12 columns -- one for each month')
			tsize = c(tsize,dim(tmean)[1])			
		} else { stop('tmean must be null, a data.frame or a matrix') }
	}
	#check number of rows of input data
	if (!all(tsize==mean(tsize))) stop('all input data must be of the same length')
	
	#define the output
	out = matrix(NA,nr=mean(tsize),nc=19)
	
	###calculate the bioclim variables
	#Annual Mean Temperature -- 1
	if (any(vois %in% c(1,4))) { out[,1] = rowMeans(tmean,na.rm=T) }
	#Mean Diurnal Range -- 2
	if (any(vois == 2)) { out[,2] = rowMeans(tmax-tmin,na.rm=T) }
	#temperature seasonality --4
	if (any(vois == 4)) {
		out[,4] = apply(tmean,1,function(x) { return(sd(x,na.rm=T)) }) #this is as worldclim
		if (cov) { out[,4] = out[,4] / (rowMeans(tmean,na.rm=T)+273.15) * 100 } #calculate it as anuclim does (cov)
	}
	#max temperature of the warmest period -- 5
	if (any(vois %in% c(5,3,7))) { out[,5] = apply(tmax,1,function(x) { return(max(x,na.rm=T)) }) }
	#min temperature of the warmest period -- 6
	if (any(vois %in% c(6,3,7))) { out[,6] = apply(tmin,1,function(x) { return(min(x,na.rm=T)) }) }
	#temperatre annual range (5-6) -- 7
	if (any(vois == 7)) { out[,7] = out[,5] - out[,6] }
	#isothermality (2/7) -- 3
	if (any(vois == 3)) { out[,3] = out[,2] / out[,7] }
	#annual precipitation -- 12
	if (any(vois == 12)) { out[,12] = rowSums(prec,na.rm=T) }
	#precipitation of the wettest period -- 13
	if (any(vois == 13)) { out[,13] = apply(prec,1,function(x) { return(max(x,na.rm=T)) }) }
	#precipitation of the driest period -- 14
	if (any(vois == 14)) { out[,14] = apply(prec,1,function(x) { return(min(x,na.rm=T)) }) }
	#precipitation seasonality (cov) -- 15
	if (any(vois == 15)) { out[,15] = apply(prec,1,function(x) { return(sd(x,na.rm=T)) }) / rowMeans(prec,na.rm=T) }
	#work with warmest / coolest quarters
	if (any(vois %in% c(10:11,18:19))) {
		if (period=="month") {
			tt1 = matrix(NA,nr=nrow(out),nc=12)
			for (ii in 1:12) { tt1[,ii] = rowMeans(tmean[,c(ii,(ii)%%12+1,(ii+1)%%12+1)],na.rm=T) }
		} else {
			tt1 = matrix(NA,nr=nrow(out),nc=52)
			for (ii in 1:12) { tt1[,ii] = rowMeans(tmean[,c(ii,(ii)%%52+1,(ii+1)%%52+1,(ii+2)%%52+1,
			(ii+3)%%52+1),(ii+4)%%52+1,(ii+5)%%52+1,(ii+6)%%52+1,
			(ii+7)%%52+1,(ii+8)%%52+1,(ii+9)%%52+1,(ii+10)%%52+1,(ii+11)%%52+1],na.rm=T) }
		}
		#mean temperature of the warmest quarter -- 10
		if (any(vois %in% c(10,18))) { out[,10] = apply(tt1,1,function(x) { return(max(x,na.rm=T)) }) }
		#mean temperature of the coolest quarter -- 11
		if (any(vois %in% c(11,19))) { out[,11] = apply(tt1,1,function(x) { return(min(x,na.rm=T)) }) }
		#precipitation of the warmest or wettest quarters
		if (any(vois %in% 18:19)) {
			if (period=="month") {
				tt2 = matrix(NA,nr=nrow(out),nc=12)
				for (ii in 1:12) { tt2[,ii] = rowSums(prec[,c(ii,(ii)%%12+1,(ii+1)%%12+1)],na.rm=T) }
			} else {
				tt2 = matrix(NA,nr=nrow(out),nc=52)
				for (ii in 1:12) { tt2[,ii] = rowSums(prec[,c(ii,(ii)%%52+1,(ii+1)%%52+1,(ii+2)%%52+1,
				(ii+3)%%52+1),(ii+4)%%52+1,(ii+5)%%52+1,(ii+6)%%52+1,
				(ii+7)%%52+1,(ii+8)%%52+1,(ii+9)%%52+1,(ii+10)%%52+1,(ii+11)%%52+1],na.rm=T) }
			}
			#precipitation of the warmest quarter -- 18
			if (any(vois == 18)) { 
				tt = rep(0,nrow(tt2)); for (ii in 1:nrow(tt2)) { tt[ii] = which(tt1[ii,]==out[ii,10])[1] }
				out[,18] = tt2[cbind(1:nrow(tt2),tt)]
			}
			#precipitation of the coolest quarter -- 19
			if (any(vois == 19)) { 
				tt = rep(0,nrow(tt2)); for (ii in 1:nrow(tt2)) { tt[ii] = which(tt1[ii,]==out[ii,11])[1] }
				out[,19] = tt2[cbind(1:nrow(tt2),tt)]
			}
			rm(tt2);rm(tt)
		}
		rm(tt1)
	}
	#work with wettest / driest quarters
	if (any(vois %in% c(8:9,16:17))) {
		if (period=="month") {
			tt1 = matrix(NA,nr=nrow(out),nc=12)
			for (ii in 1:12) { tt1[,ii] = rowSums(prec[,c(ii,(ii)%%12+1,(ii+1)%%12+1)],na.rm=T) }
		} else {
			tt1 = matrix(NA,nr=nrow(out),nc=52)
			for (ii in 1:12) { tt1[,ii] = rowSums(prec[,c(ii,(ii)%%52+1,(ii+1)%%52+1,(ii+2)%%52+1,
			(ii+3)%%52+1),(ii+4)%%52+1,(ii+5)%%52+1,(ii+6)%%52+1,
			(ii+7)%%52+1,(ii+8)%%52+1,(ii+9)%%52+1,(ii+10)%%52+1,(ii+11)%%52+1],na.rm=T) }
		}
		#precipitation of the wettest quarter -- 16
		if (any(vois %in% c(8,16))) { out[,16] = apply(tt1,1,function(x) { return(max(x,na.rm=T)) }) }
		#precipitation of the driest quarter -- 17
		if (any(vois %in% c(9,17))) { out[,17] = apply(tt1,1,function(x) { return(min(x,na.rm=T)) }) }
		#temperature of the wettest & driest quarters
		if (any(vois %in% 8:9)) {
			if (period=="month") {
				tt2 = matrix(NA,nr=nrow(out),nc=12)
				for (ii in 1:12) { tt2[,ii] = rowMeans(tmean[,c(ii,(ii)%%12+1,(ii+1)%%12+1)],na.rm=T) }
			} else {
				tt2 = matrix(NA,nr=nrow(out),nc=52)
				for (ii in 1:12) { tt2[,ii] = rowMeans(tmean[,c(ii,(ii)%%52+1,(ii+1)%%52+1,(ii+2)%%52+1,
				(ii+3)%%52+1),(ii+4)%%52+1,(ii+5)%%52+1,(ii+6)%%52+1,
				(ii+7)%%52+1,(ii+8)%%52+1,(ii+9)%%52+1,(ii+10)%%52+1,(ii+11)%%52+1],na.rm=T) }
			}
			#mean temperature of the wettest quarter -- 8
			if (any(vois == 8)) { 
				tt = rep(0,nrow(tt2)); for (ii in 1:nrow(tt2)) { tt[ii] = which(tt1[ii,]==out[ii,16])[1] }
				out[,8] = tt2[cbind(1:nrow(tt2),tt)]
			}
			#mean temperature of the driest quarter -- 9
			if (any(vois == 9)) { 
				tt = rep(0,nrow(tt2)); for (ii in 1:nrow(tt2)) { tt[ii] = which(tt1[ii,]==out[ii,17])[1] }
				out[,9] = tt2[cbind(1:nrow(tt2),tt)]
			}
			rm(tt2);rm(tt)
		}
		rm(tt1)
	}
	#do the necessary rounding
	if (t.as.int) {
		out[,c(1:3,5:11)] = round(out[,c(1:3,5:11)]*10)
	} else {
		out[,c(1:3,5:11)] = round(out[,c(1:3,5:11)],1)
	}
	out[,c(4,15)] = round(out[,c(4,15)]*100)
	out[,c(12:14,16:19)] = round(out[,c(12:14,16:19)])
	
	#define the colnames
	colnames(out) = paste('bioclim',sprintf('%02i',1:19),sep='_')
	
	#return the output
	return(out[,vois])
}

