bioclim2=function (tmin = NULL, tmax = NULL, prec = NULL, tmean = NULL, cov = FALSE, files.as.inputs=TRUE) 
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

	#start creating bioclim variables
	out=list() #define the output
	tmin=loaddata(tmin) #deal with tmin
	out$bioclim_06 = .Call('rowMin',tmin) # 6 Min Temperature of Coldest month
	tmax=loaddata(tmax) #deal with tmax
	out$bioclim_05 = .Call('rowMax',tmin) # 5 Max Temperature of Warmest month
	out$bioclim_07 = out$bioclim_05 - out$bioclim_06 # 7 Temperature Annual Range (5-6)
	out$bioclim_02 = .Call('createBio02',tmax,tmin) # 2 Mean Diurnal Range(Mean(month max-min))
	out$bioclim_03 = out$bioclim_02 / out$bioclim_07 # 3 Isothermality 2/7
	if (is.null(tmean)) { 
		tmean = .Call('createTmean',tmax,tmin) #create tmean
		rm(tmin); rm(tmax); gc() #remove unnecessary files
	} else {
		rm(tmin); rm(tmax); gc() #remove unnecessary files
		tmean=loaddata(tmean) #load the data
	}
	out$bioclim_01 = .Call('rowMean',tmean) # 1 Annual Mean Temperature for each location
	out$bioclim_04 = .Call('rowSD',tmean,out$bioclim_01) # 4 Temperature Seasonality ### as per WORLCLIM
	if (cov) out$bioclim_04 = (out$bioclim_04 /(out$bioclim_01 + 273.15)) * 100 #to make this comparable to ANUCLIM
	prec = loaddata(prec) #load in the precip
	out$bioclim_12 = .Call('rowSum',prec) # 12 Annual Precipitation
	out$bioclim_13 = .Call('rowMax',prec) # 13 Precipitation of Wettest week/month
	out$bioclim_14 = .Call('rowMin',prec) # 14 Precipitation of Driest week/month
	out$bioclim_15 = .Call('rowCov',prec,out$bioclim_12) # 15 Precipitation Seasonality(Coef. of Variation)
	tout = .Call('createWWCD',tmean,prec) 
	out$bioclim_10 = tout[,1] # 10 Mean Temperature of Warmest quarter
	out$bioclim_11 = tout[,2] # 11 Mean Temperature of Coldest quarter
	out$bioclim_18 = tout[,3] # 18 Precipitation of Warmest Quarter
	out$bioclim_19 = tout[,4] # 19 Precipitation of Coldest Quarter
	out$bioclim_16 = tout[,5] # 16 Precipitation of Wettest Quarter
	out$bioclim_17 = tout[,6] # 17 Precipitation of Driest Quarter
	out$bioclim_08 = tout[,7] # 8 Mean Temperature of Wettest Quarter
	out$bioclim_09 = tout[,8] # 9 Mean Temperature of Driest Quarter
	rm(tout); rm(prec); rm(tmean); gc()
	out=do.call('cbind',out[c('bioclim_01','bioclim_02','bioclim_03','bioclim_04','bioclim_05',
		'bioclim_06','bioclim_07','bioclim_08','bioclim_09','bioclim_10','bioclim_11',
		'bioclim_12','bioclim_13','bioclim_14','bioclim_15','bioclim_16','bioclim_17',
		'bioclim_18','bioclim_19')]); gc() #bind as a matrix
    return(out[, vois])
}

