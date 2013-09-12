bioclim=function (tmin = NULL, tmax = NULL, prec = NULL, tmean = NULL,
    vois = 1:19, cov = FALSE, files.as.inputs=TRUE) 
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
	out$bio02 = .Call('rowMean',tmax[,] - tmin[,]) # 2 Mean Diurnal Range(Mean(month max-min))
	out$bio03 = out$bio02 / out$bio07 # 3 Isothermality 2/7
	if (is.null(tmean)) { 
		tmean = (tmax[,] + tmin[,]) / 2 #create tmean
		rm(tmin); rm(tmax); gc() #remove unnecessary files
	} else {
		rm(tmin); rm(tmax); gc() #remove unnecessary files
		tmean=loaddata(tmean) #load the data
	}
	out$bio01 = .Call('rowMean',tmean) # 1 Annual Mean Temperature for each location
	out$bio04 = .Call('rowSD',tmean,out$bio01) # 4 Temperature Seasonality ### as per WORLCLIM
	if (cov) out$bio04 = (out$bio04 /(out[, 1] + 273.15)) * 100 #to make this comparable to ANUCLIM
	prec = loaddata(prec) #load in the precip
	out$bio12 = .Call('rowSum',prec) # 12 Annual Precipitation
	out$bio13 = .Call('rowMax',prec) # 13 Precipitation of Wettest week/month
	out$bio14 = .Call('rowMin',prec) # 14 Precipitation of Driest week/month
	out$bio15 = .Call('rowCov',prec,out$bio12) # 15 Precipitation Seasonality(Coef. of Variation)
	
	
	
	
	out=do.call('cbind',out[c('bio01','bio02','bio03','bio04','bio05','bio06','bio07')]) #bind as a matrix
	
	
# Start calculating BIOCLIM variables and putting them in output matrix

if (any(vois == 12)) # 12 Annual Precipitation
        out[, 12] = rowSums(prec, na.rm = T)
    if (any(vois == 13)) { # 13 Precipitation of Wettest week/month
        out[, 13] = apply(prec, 1, function(x) {return(max(x, na.rm = T))})
    }
    if (any(vois == 14)) { # 14 Precipitation of Driest week/month
        out[, 14] = apply(prec, 1, function(x) {return(min(x, na.rm = T))})
    }
    if (any(vois == 15)) { # 15 Precipitation Seasonality(Coef. of Variation)
        out[, 15] = apply(prec, 1, function(x) {return(sd(x, na.rm = T))})/rowMeans(prec, na.rm = T)
    }
    if (any(vois %in% c(8:11, 16:19))) { # Warmest/coldest and wettest/driest quarter sums and means
        if (period == "month") {
            tt1 = matrix(NA, nr = tsize, nc = 12)
            tt2 = matrix(NA, nr = tsize, nc = 12)
            for (ii in 1:12) {  # Find temperature means for 3 month quarters
                tt1[, ii] = rowMeans(tmean[, m.per.indx(ii)], na.rm = T)
                tt2[, ii] = rowSums(prec[, m.per.indx(ii)], na.rm = T)
            } # These are time consuming, so I do them once here
        } # May be able to add some if statements to only do them when needed
        else {
            tt1 = matrix(NA, nr = tsize, nc = 52)
            tt2 = matrix(NA, nr = tsize, nc = 52)
            for (ii in 1:52) {  # Find temperature means for 13 week quarters
                tt1[, ii] = rowMeans(tmean[, w.per.indx(ii)], na.rm = T)
                tt2[, ii] = rowSums(prec[, w.per.indx(ii)], na.rm = T)
            }
		}
	}
    if (any(vois %in% c(10:11, 18:19))) { # Warmest and coldest quarter stuff
		
        if (any(vois %in% c(10, 18))) { # 10 Mean Temperature of Warmest quarter; might be better to use which.max
            out[, 10] = apply(tt1, 1, function(x) {return(max(x, na.rm = T))})
        }
        if (any(vois %in% c(11, 19))) { # 11 Mean Temperature of Coldest quarter
            out[, 11] = apply(tt1, 1, function(x) {return(min(x, na.rm = T))})
		}
	
        if (any(vois %in% 18:19)) {
            if (any(vois == 18)) { # 18 Precipitation of Warmest Quarter
                tt = NULL
                for (ii in 1:tsize) {
					tt = which(tt1[ii, ] == out[ii, 10]) # Finds index of warmest Quarters for each location
					if(length(tt)>1) tt=tt[1] # Ignores ties, uses first; could use max.col or which.is.max in nnet
					out[ii,18]=tt2[ii,tt]
			  } # Index tt indicates month or week of first warmest Quarter.
            }
            if (any(vois == 19)) { # 19 Precipitation of Coldest Quarter
				tt = NULL
				for (ii in 1:tsize) {
					tt = which(tt1[ii, ] == out[ii, 11]) # Finds indexes of coldest Quarters
					if(length(tt)>1) tt=tt[1] # Ignores ties, uses first
					out[ii, 19] = tt2[ii, tt]
                }
            }
       }
    }
    if (any(vois %in% c(8:9, 16:17))) { # Wettest and driest Quarter stuff
        if (any(vois %in% c(8, 16))) { # 16 Precipitation of Wettest Quarter
            out[, 16] = apply(tt2, 1, function(x) {return(max(x, na.rm = T))})
        }
        if (any(vois %in% c(9, 17))) { # 17 Precipitation of Driest Quarter
            out[, 17] = apply(tt2, 1, function(x) {return(min(x, na.rm = T))})
        }
        if (any(vois %in% 8:9)) {
            if (any(vois == 8)) { # 8 Mean Temperature of Wettest Quarter
                tt = NULL
                for (ii in 1:tsize) {
                  tt =which(tt2[ii, ] == out[ii, 16])
				  if(length(tt)>1) tt=tt[1] # Ignores ties, uses first
				  out[ii, 8] = tt1[ii, tt]
                }
            }
            if (any(vois == 9)) { # 9 Mean Temperature of Driest Quarter
                tt = NULL
                for (ii in 1:tsize) {
                  tt =which(tt2[ii, ] == out[ii, 17])
				  if(length(tt)>1) tt=tt[1] # Ignores ties, uses first
				  out[ii, 9] = tt1[ii, tt]
                }
            }
        }
	}
    colnames(out) = paste("bioclim", 1:19, sep = "_")
    return(out[, vois])
}

