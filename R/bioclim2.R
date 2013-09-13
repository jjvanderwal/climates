bioclim=function (tmin = NULL, tmax = NULL, prec = NULL, tmean = NULL, cov = FALSE, files.as.inputs=TRUE) 
{
	# Fuction that calculates all of the 19 BIOCLIM variables (not all subsets possible)
	# 	tmin, tmax and prec must be specified in the function call; if you have tmean you may pass
	# 	it, else it will be caculated as (tmax+tmin)/2
	# 	THIS uses monthly data only see ?bioclim for other vars
	# this function will read in csv or RData files if tmin,tmax,prec,tmean are file locations
	#ASSUMES no missing data!!!!
	
	#check valid inputs
	if (any(c(is.null(tmin),is.null(tmax),is.null(prec)))) stop('tmin, tmax & prec must have inputs') #stop if missing input
	
	#function to check and load data
	loaddata = function(x) {
		if (files.as.inputs) {
			if (length(grep('.csv',x,ignore.case=TRUE))>0) { #read in csv
				x = read.csv(x,as.is=TRUE)
			} else if (length(grep('.RData',x,ignore.case=TRUE))>0) { #read in the Rdata file
				aa=new.env() #load a new environment
				load(x,envir=aa) #load hte data
				x = aa[[ls(aa)[1]]] #move the data to a new name in the parent envornment
				rm(aa); gc() #remove the extra environment and data
			}
		} else if (is.data.frame(x)) {
				x = as.matrix(x) #convert to a matrix				
		}
		if (!dim(x)[2]==12) stop('not 12 columns for the inputs')
		return(x)
	}

	
	#R CMD SHLIB /home/jc165798/SCRIPTS/R_package_dev/climates/src/bioclimate.c
	dyn.load("/home/jc165798/SCRIPTS/R_package_dev/climates/src/bioclimate.so") #load the c functions into memory

	
	# tmin = matrix(abs(rnorm(1e7)),1e6,12)
	# save(tmin,file='delete.RData')
	# tmax = matrix(abs(rnorm(1e7)),1e6,12)
	# save(tmax,file='delete2.RData')
	# prec = matrix(abs(rnorm(1e7)),1e6,12)
	# save(prec,file='delete3.RData')
	
	# tmin = matrix(abs(rnorm(1e7)),1e8,12)
	# tmax = matrix(abs(rnorm(1e7)),1e8,12)
	# prec = matrix(abs(rnorm(1e7)),1e8,12)
	
	tmin='delete.RData'
	tmax='delete2.RData'
	tmean=NULL
	files.as.inputs=TRUE
	prec='delete3.RData'
	cov=FALSE
	
	#start creating bioclim variables
	out=list() #define the output
	tmin=loaddata(tmin) #deal with tmin
	out$bio06 = .Call('rowMin',tmin) # 6 Min Temperature of Coldest month
	tmax=loaddata(tmax) #deal with tmax
	out$bio05 = .Call('rowMax',tmin) # 5 Max Temperature of Warmest month
	out$bio07 = out$bio05 - out$bio06 # 7 Temperature Annual Range (5-6)
	out$bio02 = .Call('createBio02',tmax,tmin) # 2 Mean Diurnal Range(Mean(month max-min))
	out$bio03 = out$bio02 / out$bio07 # 3 Isothermality 2/7
	if (is.null(tmean)) { 
		tmean = .Call('createTmean',tmax,tmin) #create tmean
		rm(tmin); rm(tmax); gc() #remove unnecessary files
	} else {
		rm(tmin); rm(tmax); gc() #remove unnecessary files
		tmean=loaddata(tmean) #load the data
	}
	out$bio01 = .Call('rowMean',tmean) # 1 Annual Mean Temperature for each location
	out$bio04 = .Call('rowSD',tmean,out$bio01) # 4 Temperature Seasonality ### as per WORLCLIM
	if (cov) out$bio04 = (out$bio04 /(out$bio01 + 273.15)) * 100 #to make this comparable to ANUCLIM
	prec = loaddata(prec) #load in the precip
	out$bio12 = .Call('rowSum',prec) # 12 Annual Precipitation
	out$bio13 = .Call('rowMax',prec) # 13 Precipitation of Wettest week/month
	out$bio14 = .Call('rowMin',prec) # 14 Precipitation of Driest week/month
	out$bio15 = .Call('rowCov',prec,out$bio12) # 15 Precipitation Seasonality(Coef. of Variation)
	tout = .Call('createWWCD',tmean,prec) 
	out$bio10 = tout[,1] # 10 Mean Temperature of Warmest quarter
	out$bio11 = tout[,2] # 11 Mean Temperature of Coldest quarter
	out$bio18 = tout[,3] # 18 Precipitation of Warmest Quarter
	out$bio19 = tout[,4] # 19 Precipitation of Coldest Quarter
	out$bio16 = tout[,5] # 16 Precipitation of Wettest Quarter
	out$bio17 = tout[,6] # 17 Precipitation of Driest Quarter
	out$bio08 = tout[,7] # 8 Mean Temperature of Wettest Quarter
	out$bio09 = tout[,8] # 9 Mean Temperature of Driest Quarter
	rm(tout); gc()
	out=do.call('cbind',out[c('bio01','bio02','bio03','bio04','bio05','bio06','bio07','bio08','bio09','bio10','bio11','bio12','bio13','bio14','bio15','bio16','bio17','bio18','bio19')]) #bind as a matrix
    colnames(out) = gsub('bio','bioclim_',colnames(out))
    return(out[, vois])
}

