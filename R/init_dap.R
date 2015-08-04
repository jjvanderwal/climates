#' An function to initialize an OPeNDAP resource for the climates package.
#'
#' This function takes an OPeNDAP web service uri and a list of variables of interest.
#' It returns a named list containing the ncdf4 object's handle, and a temperature conversion function.
#'
#' @param OPeNDAP_URI an OPeNDAP resource without any service parameters.
#' @param tmax_var a string of the tmax variable id
#' @param tmin_var a string of the tmin variable id
#' @param prcp_var a string of the prcp variable id
#' @return ncdf4_init a named list of an ncdf4 object and a temperature conversion function.
#' @export
#' @examples
#' \dontrun{
#' Soon!
#' }
#' 
init_dap<-function(OPeNDAP_URI,tmax_var,tmin_var,prcp_var,tave_var)
{
  if(!require("ncdf4")){
    print("trying to install ncdf4")
    install.packages("ncdf4")
    if(require(ncdf4)){
      print("ncdf4 installed and loaded")
    } else {
      stop("could not install ncdf4")
    }
  }
  tryCatch(ncdf4_handle <- nc_open(OPeNDAP_URI), error = function(e) 
  {
    cat("An error was encountered trying to open the OPeNDAP resource."); print(e)
  })
  
  variables<-as.character(sapply(ncdf4_handle$var,function(x) x$name))
  
  #Check if variables exist.
  if (!tmax_var %in% variables) stop(paste("The given tmax variable wasn't found in the OPeNDAP dataset"))
  if (!tmin_var %in% variables) stop(paste("The given tmin variable wasn't found in the OPeNDAP dataset"))
  if (!prcp_var %in% variables) stop(paste("The given prcp variable wasn't found in the OPeNDAP dataset"))
  if (!is.null(tave_var)) if (!tmax_var %in% variables) stop(paste("The given tave variable wasn't found in the OPeNDAP dataset"))
  
  #Set temperature unit conversion to 1 unless units are K or F.
  temp_unit_func<-function(t) {t}
  if (grepl('k',ncatt_get(ncdf4_handle, tmax_var,'units')$value, ignore.case = TRUE)) {temp_unit_func <- function(t) {t-273} }
  if (grepl('f',ncatt_get(ncdf4_handle, tmax_var,'units')$value, ignore.case = TRUE)) {temp_unit_func <- function(t) {(t-32)*(5/9)} }
  
  return(list(ncdf4_handle=ncdf4_handle,temp_unit_func=temp_unit_func))
}