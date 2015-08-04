#' Daily Climate Indices
#' 
#' \code{daily_indices} implements a suite of basic climate indices based on
#' daily records It is written to run one year of data at a time for
#' scalability to large gridded domains.
#' 
#' 
#' @param tmin a data.frame or matrix with greater than 359 and less than 367
#' columns representing daily minimum temperature data; rows represent
#' different locations.
#' @param tmax a data.frame or matrix as with \code{tmin} representing maximum
#' temperature data.
#' @param prec a data.frame or matrix as with \code{tmin} representing
#' precipitation data.
#' @param tmean a data.frame or matrix as with \code{tmin} representing mean
#' temperature data.
#' @param thresholds A named list with the following structure:\cr
#' thresholds=list(days_tmax_abv_thresh=c(),\cr days_tmin_blw_thresh=c(),\cr
#' days_prcp_abv_thresh=c(),\cr longest_run_tmax_abv_thresh=c(),\cr
#' longest_run_prcp_blw_thresh=c(),\cr growing_degree_day_thresh=c(),\cr
#' heating_degree_day_thresh=c(),\cr cooling_degree_day_thresh=c(),\cr
#' growing_season_lngth_thresh=c())\cr where thresholds are in celsius or mm
#' for temperature or precipitation respectively. Multiple thresholds can be
#' specified as a vector and entire statistics can be omitted.
#' @param time_PCICt The time vector cooresponding to the data.frames or
#' matrices in PCICt format.
#' @return a matrix with columns representing variables requested and the
#' number of rows(and order of them) the same as the input \code{tmin},
#' \code{tmax}, \code{prec} or \code{tmean}.
#' @author David Blodgett \email{dblodgett@@usgs.gov}
#' @export
#' @importFrom climdex.pcic number.days.op.threshold spell.length.max growing.season.length
#' @examples
#' 
#' \dontrun{
#' data(daily_indices_example, package='climates')
#' output<-daily_indices(tmin=tmin_data, tmax=tmax_data, prec=prcp_data, 
#' tmean=tave_data, thresholds,time_PCICt)
#' }
#' 
daily_indices=function (tmin = NULL, tmax = NULL, prec = NULL, tmean = NULL, thresholds=NULL, time_PCICt=NULL) 
{
  # Fuctions that calculate all or subsets of 9 daily climate indices. 1) days with tmax above. 2) days with tmin below
  # 3) days with precip above 4) Longest run with precip above 5) longest run with precip below 6) growing degree days
  # 7) Heating degree days 8) Cooling degree days 9) Growing season length.
  tmin.thresh = c(thresholds$days_tmin_blw_thresh)
  tmax.thresh = c(thresholds$days_tmax_abv_thresh,thresholds$longest_run_tmax_abv_thresh)
  tmean.thresh = c(thresholds$growing_degree_day_thresh,thresholds$heating_degree_day_thresh,thresholds$cooling_degree_day_thresh,thresholds$growing_season_lngth_thresh)
  prec.thresh = c(thresholds$days_prcp_abv_thresh,thresholds$longest_run_prcp_blw_thresh)
  
  # Function to check for various input errors, stop program and print an error message
  error.check=function(datum,datum.name){
    if (is.null(datum))
      stop(paste(datum.name,"is needed for the variables selected"))
    else if (is.data.frame(datum) | is.matrix(datum)) {
      if (dim(datum)[1] < 360) # check for correct number of columns
        stop(paste(datum.name,"must be a full year of data and not more than a full year of data."))
      if (dim(datum)[1] > 366)
        stop(paste(datum.name,"must be a full year of data and not more than a full year of data."))
    }
    else 
      stop(paste(datum.name,"must be a data.frame or matrix"))
    return(list(grid_size=dim(datum)[2],time_size=dim(datum)[1]))
  }
  
  # Check for valid input data
  if (!is.null(tmin.thresh))
    check=error.check(tmin,"tmin")
  if (!is.null(tmax.thresh))
    check=error.check(tmax,"tmax")
  if (!is.null(prec.thresh))
    check=error.check(prec,"prec")
  if (!is.null(tmean.thresh))
    check=error.check(tmean,"tmean")
  if (!all(check$time_size == mean(check$time_size))) # Check all input vars are the same length
    stop("all input data must be of the same length") # redundant?
  
  time_size=mean(check$time_size)
  grid_size=mean(check$grid_size)
  
  num_stats<-0
  
  for(i in thresholds) {num_stats<-num_stats+length(i)}
  
  out = matrix(NA, nrow = grid_size, ncol = num_stats)  # Set up output matrix
  
  colnames(out)=paste("thresh", 1:num_stats, sep = "_")
  
  date_factor=factor(rep(2000,time_size))
  
  stat=1
  
  for (days_tmax_abv_thresh in thresholds$days_tmax_abv_thresh)
  {
    colnames(out)[stat]=paste("days_tmax_abv",days_tmax_abv_thresh,"C",sep="_")
    out[,stat]<-apply(tmax, 2, number.days.op.threshold, date.factor=date_factor, threshold=days_tmax_abv_thresh, op=">")
    stat=stat+1
  }
  
  for (days_tmin_blw_thresh in thresholds$days_tmin_blw_thresh)
  {
    colnames(out)[stat]=paste("days_tmin_blw",days_tmin_blw_thresh,"C",sep="_")
    out[,stat]<-apply(tmin, 2, number.days.op.threshold, date.factor=date_factor, threshold=days_tmin_blw_thresh, op="<")
    stat=stat+1
  }
  
  for (days_prcp_abv_thresh in thresholds$days_prcp_abv_thresh)
  {
    colnames(out)[stat]=paste("days_prcp_abv",days_prcp_abv_thresh,"C",sep="_")
    out[,stat]<-apply(prec, 2, number.days.op.threshold, date.factor=date_factor, threshold=days_prcp_abv_thresh, op=">")
    stat=stat+1
  }
  
  for (longest_run_tmax_abv_thresh in thresholds$longest_run_tmax_abv_thresh)
  {
    colnames(out)[stat]=paste("longest_run_tmax_abv",longest_run_tmax_abv_thresh,"C",sep="_")
    out[,stat]<-apply(tmax, 2, spell.length.max, date.factor=date_factor, threshold=longest_run_tmax_abv_thresh, op=">",spells.can.span.years=FALSE)
    stat=stat+1
  }
  
  for (longest_run_prcp_blw_thresh in thresholds$longest_run_prcp_blw_thresh)
  {
    colnames(out)[stat]=paste("longest_run_prcp_blw",longest_run_prcp_blw_thresh,"C",sep="_")
    out[,stat]<-apply(prec, 2, spell.length.max, date.factor=date_factor, threshold=longest_run_prcp_blw_thresh, op="<",spells.can.span.years=FALSE)
    stat=stat+1
  }
  
  trunc_0 <- function(x) { x[x<0] <- 0; x }
  growing_degree_days_fun <- function(tave, growing_degree_day_thresh=10) { degree_days_out <- sum(trunc_0(tave-growing_degree_day_thresh)); degree_days_out}
  
  for (growing_degree_day_thresh in thresholds$growing_degree_day_thresh)
  {
    colnames(out)[stat]=paste("growing_degree_day",growing_degree_day_thresh,"C",sep="_")
    out[,stat]<-apply(tmean, 2, growing_degree_days_fun, growing_degree_day_thresh=growing_degree_day_thresh)
    stat=stat+1
  }
  
  heating_degree_days_fun <- function(tave, heating_degree_day_thresh=18.333) { degree_days_out <- sum(trunc_0(heating_degree_day_thresh-tave)); degree_days_out}
  
  for (heating_degree_day_thresh in thresholds$heating_degree_day_thresh)
  {
    colnames(out)[stat]=paste("heating_degree_day",heating_degree_day_thresh,"C",sep="_")
    out[,stat]<-apply(tmean, 2, heating_degree_days_fun,heating_degree_day_thresh=heating_degree_day_thresh)
    stat=stat+1
  }
  
  cooling_degree_days_fun <- function(tave, cooling_degree_day_thresh=18.333) { degree_days_out <- sum(trunc_0(tave-cooling_degree_day_thresh)); degree_days_out}
  
  for (cooling_degree_day_thresh in thresholds$cooling_degree_day_thresh)
  {
    colnames(out)[stat]=paste("cooling_degree_day",cooling_degree_day_thresh,"C",sep="_")
    out[,stat]<-apply(tmean, 2, cooling_degree_days_fun,cooling_degree_day_thresh=cooling_degree_day_thresh)
    stat=stat+1
  }
  
  for (growing_season_lngth_thresh in thresholds$growing_season_lngth_thresh)
  {
    colnames(out)[stat]=paste("growing_season_lngth",growing_season_lngth_thresh,"C",sep="_")
    out[,stat]<-apply(tmean, 2, growing.season.length, date.factor=date_factor, dates=time_PCICt, northern.hemisphere=TRUE, t.thresh=growing_season_lngth_thresh)
    stat=stat+1
  }
  return(out)
}
