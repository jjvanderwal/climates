#' Bioclim - Bioclimatic Variables -- simplified
#' 
#' \code{bioclim2} recreates the standard 19 bioclimatic variables (BIOCLIM),
#' as does related command \code{bioclim}, that were orginally generated using
#' ANUCLIM
#' \url{http://fennerschool.anu.edu.au/publications/software/anuclim.php} or
#' used by Worldclim \url{http://www.worldclim.org/}. \cr \cr These measures
#' are described below but include annual and quarterly summaries of
#' temperature and precipitation.\cr \cr \bold{NOTE:} the key differences
#' between this and \code{bioclim} is that: \cr 1. this is much faster due to C
#' implementation;\cr 2. this can only use monthly data;\cr 3. will only
#' generate all 19 variables; and\cr 4. can use file names rather than objects
#' in R to save on memory (not all things need to be loaded into memory at
#' once).\cr \cr !!!ASSUMES no missing data!!!
#' 
#' The variables created here are based primarily on BIOCLIM variables created
#' by ANUCLIM
#' \url{http://fennerschool.anu.edu.au/publications/software/anuclim.php}.
#' Below is a description of the variables and how they are calculated from the
#' ANUCLIM website, with Temperature Seasonality description modified for the
#' different method Worldclim uses.\cr\cr The descriptions below assume you are
#' using a weekly time step. If you are using months, the monthly values rather
#' than the weekly values will be used when calculating these parameters.\cr\cr
#' The quarterly parameters are not aligned to any calendar quarters. BIOCLIM's
#' definition of a quarter is any 13 consecutive weeks, (or any consecutive 3
#' months if running with a monthly time step). For example, the driest quarter
#' will be the 13 consecutive weeks that are drier than any other set of 13
#' consecutive weeks.\cr\cr \enumerate{ \item \bold{Annual Mean Temperature}\cr
#' The mean of all the weekly mean temperatures. Each weekly mean temperature
#' is the mean of that week's maximum and minimum temperature.  \item
#' \bold{Mean Diurnal Range(Mean(period max-min))}\cr The mean of all the
#' weekly diurnal temperature ranges. Each weekly diurnal range is the
#' difference between that week's maximum and minimum temperature.  \item
#' \bold{Isothermality 2/7}\cr The mean diurnal range (parameter 2) divided by
#' the Annual Temperature Range (parameter 7).  \item \bold{Temperature
#' Seasonality}\cr ANUCLIM (\code{cov}=TRUE) returns the temperature
#' Coefficient of Variation (C of V) as the standard deviation of the weekly
#' mean temperatures expressed as a percentage of the mean of those
#' temperatures (i.e. the annual mean). For this calculation, the mean in
#' degrees Kelvin is used. This avoids the possibility of having to divide by
#' zero, but does mean that the values are usually quite small.\cr Worldclim
#' (\code{cov}=FALSE) returns the the standard deviation of the weekly mean
#' temperatures.  \item \bold{Max Temperature of Warmest Period}\cr The highest
#' temperature of any weekly maximum temperature.  \item \bold{Min Temperature
#' of Coldest Period}\cr The lowest temperature of any weekly minimum
#' temperature.  \item \bold{Temperature Annual Range (5-6)}\cr The difference
#' between the Max Temperature of Warmest Period and the Min Temperature of
#' Coldest Period.  \item \bold{Mean Temperature of Wettest Quarter}\cr The
#' wettest quarter of the year is determined (to the nearest week), and the
#' mean temperature of this period is calculated.  \item \bold{Mean Temperature
#' of Driest Quarter}\cr The driest quarter of the year is determined (to the
#' nearest week), and the mean temperature of this period is calculated.  \item
#' \bold{Mean Temperature of Warmest Quarter}\cr The warmest quarter of the
#' year is determined (to the nearest week), and the mean temperature of this
#' period is calculated.  \item \bold{Mean Temperature of Coldest Quarter}\cr
#' The coldest quarter of the year is determined (to the nearest week), and the
#' mean temperature of this period is calculated.  \item \bold{Annual
#' Precipitation}\cr The sum of all the monthly precipitation estimates.  \item
#' \bold{Precipitation of Wettest Period}\cr The precipitation of the wettest
#' week or month, depending on the time step.  \item \bold{Precipitation of
#' Driest Period}\cr The precipitation of the driest week or month, depending
#' on the time step.  \item \bold{Precipitation Seasonality(C of V)}\cr The
#' Coefficient of Variation (C of V) is the standard deviation of the weekly
#' precipitation estimates expressed as a percentage of the mean of those
#' estimates (i.e. the annual mean).  \item \bold{Precipitation of Wettest
#' Quarter}\cr The wettest quarter of the year is determined (to the nearest
#' week), and the total precipitation over this period is calculated.  \item
#' \bold{Precipitation of Driest Quarter}\cr The driest quarter of the year is
#' determined (to the nearest week), and the total precipitation over this
#' period is calculated.  \item \bold{Precipitation of Warmest Quarter}\cr The
#' warmest quarter of the year is determined (to the nearest week), and the
#' total precipitation over this period is calculated.  \item
#' \bold{Precipitation of Coldest Quarter}\cr The coldest quarter of the year
#' is determined (to the nearest week), and the total precipitation over this
#' period is calculated.  }
#' 
#' @param tmin a data.frame or matrix or filename of a csv or RData object
#' having 12 columns representing monthly or weekly minimum temperature data;
#' rows represent different locations.
#' @param tmax a data.frame or matrix or filename as with \code{tmin}
#' representing maximum temperature data.
#' @param prec a data.frame or matrix or filename as with \code{tmin}
#' representing precipitation data.
#' @param tmean a data.frame or matrix or filename as with \code{tmin}
#' representing mean temperature data.
#' @param cov a boolean value where TRUE represents Temperature seasonality (4)
#' is calculated as in ANUCLIM and FALSE is calclated as with Worldclim; see
#' details for further information.
#' @param files.as.inputs defines if the inputs are files.
#' @return a matrix with columns representing variables requested and the
#' number of rows(and order of them) the same as the input \code{tmin},
#' \code{tmax}, \code{prec} or \code{tmean}.
#' @author Jeremy VanDerWal \email{jjvanderwal@@gmail.com}
#' @export
#' @examples
#' 
#' 
#' #need to fill in
#' 
#' 
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
			if (length(grep('csv',substr(x,nchar(x)-4,nchar(x)),ignore.case=TRUE))>0) { #read in csv
				x = read.csv(x,as.is=TRUE)
			} else if (length(grep('RData',substr(x,nchar(x)-4,nchar(x)),ignore.case=TRUE))>0) { #read in the Rdata file
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

	#start creating bioclim variables
	out=list() #define the output
	tmin=loaddata(tmin) #deal with tmin
	out$bioclim_06 = .Call('rowMin',tmin) # 6 Min Temperature of Coldest month
	tmax=loaddata(tmax) #deal with tmax
	out$bioclim_05 = .Call('rowMax',tmax) # 5 Max Temperature of Warmest month
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
    return(out)
}

