#' A function to retrieve OPeNDAP data for the climates package.
#'
#' This function takes an ncdf4 object, indices to request, variables to request, 
#' and a temperature convversion function. It returns cooresponding data.
#'
#' @param ncdf4_handle ncdf4 object.
#' @param x1 x index from request_bbox function
#' @param x2 x index from request_bbox function
#' @param y1 y index from request_bbox function
#' @param y2 y index from request_bbox function
#' @param t_ind1 t index from request_time_bounds function
#' @param t_ind2 t index from request_time_bounds function
#' @param tmax_var a string of the tmax variable id
#' @param tmin_var a string of the tmin variable id
#' @param prcp_var a string of the prcp variable id
#' @param tave_var optional: a string of the tave variable id
#' @param temp_unit_func function returned by init_dap function
#' @param Cells that are NA in only some time steps will be filled with the average of their neighbors.
#' @return A named list of data according to inputs.
#' @export
#' @examples
#' \dontrun{
#' Soon!
#' }
#' 
get_dap_data<-function(ncdf4_handle,x1,y1,x2,y2,t_ind1,t_ind2,tmax_var,tmin_var,prcp_var,tave_var=NULL,temp_unit_func=NULL, fill_nas=FALSE)
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
  #Can optionally pass in a function that will convert temperature on the fly.
  if (!is.null(temp_unit_func)) temp_unit_func<-function(t) {t}
  # Go one timestep at a time if data is daily and array is larger than ~10MB/year
  if (t_ind2-t_ind1>13 && (abs(x1-x2)+1)*(abs(y1-y2)+1)>900){
    tmax_data <- array(dim=c((abs(x1-x2)+1),(abs(y1-y2)+1),(t_ind2-t_ind1)+1))
    tmin_data <- tmax_data
    prcp_data <- tmax_data
    tave_data <- tmax_data
    step<-1
    for (t_step in t_ind1:t_ind2){
      tmax_data[,,step] <- temp_unit_func(ncvar_get(ncdf4_handle, tmax_var, c(min(x1,x2),min(y1,y2),t_step),c((abs(x1-x2)+1),(abs(y1-y2)+1),(1))))
      tmin_data[,,step] <- temp_unit_func(ncvar_get(ncdf4_handle, tmin_var, c(min(x1,x2),min(y1,y2),t_step),c((abs(x1-x2)+1),(abs(y1-y2)+1),(1))))
      prcp_data[,,step] <- ncvar_get(ncdf4_handle, prcp_var, c(min(x1,x2),min(y1,y2),t_step),c((abs(x1-x2)+1),(abs(y1-y2)+1),(1)))
      if (!is.null(tave_var)) tave_data[,,step] <- temp_unit_func(ncvar_get(ncdf4_handle, tave_var, c(min(x1,x2),min(y1,y2),t_step),c((abs(x1-x2)+1),(abs(y1-y2)+1),(1)))) else tave_data[,,step] <- (tmax_data[,,step]+tmin_data[,,step])/2
      step<-step+1
    }
    tmax_data <- tmax_data[,ncol(tmax_data):1,]
    tmin_data <- tmin_data[,ncol(tmin_data):1,]
    prcp_data <- prcp_data[,ncol(prcp_data):1,]
    tave_data <- tave_data[,ncol(tave_data):1,]
  }
  else {
  # !!! Make sure this is robust for network failures. !!!
  tmax_data <- temp_unit_func(ncvar_get(ncdf4_handle, tmax_var, c(min(x1,x2),min(y1,y2),t_ind1),c((abs(x1-x2)+1),(abs(y1-y2)+1),(t_ind2-t_ind1))))
  tmin_data <- temp_unit_func(ncvar_get(ncdf4_handle, tmin_var, c(min(x1,x2),min(y1,y2),t_ind1),c((abs(x1-x2)+1),(abs(y1-y2)+1),(t_ind2-t_ind1))))
  prcp_data <- ncvar_get(ncdf4_handle, prcp_var, c(min(x1,x2),min(y1,y2),t_ind1),c((abs(x1-x2)+1),(abs(y1-y2)+1),(t_ind2-t_ind1)))
  if (!is.null(tave_var)) tave_data <- temp_unit_func(ncvar_get(ncdf4_handle, tave_var, c(min(x1,x2),min(y1,y2),t_ind1),c((abs(x1-x2)+1),(abs(y1-y2)+1),(t_ind2-t_ind1)))) else tave_data <- (tmax_data+tmin_data)/2
  }
  if(fill_nas)
  {
    tmax_data<-fill_nans(tmax_data)
    tmin_data<-fill_nans(tmin_data)
    if (!is.null(tave_var)) tave_data<-fill_nans(tave_data) else tave_data <- (tmax_data+tmin_data)/2 
  }
  cells<-nrow(tmax_data)*ncol(tmax_data)
  #Convert result to matrix.
  tmax_data <- matrix(tmax_data,t_ind2-t_ind1,cells,byrow = TRUE)
  tmin_data <- matrix(tmin_data,t_ind2-t_ind1,cells,byrow = TRUE)
  prcp_data <- matrix(prcp_data,t_ind2-t_ind1,cells,byrow = TRUE)
  tave_data <- matrix(tave_data,t_ind2-t_ind1,cells,byrow = TRUE)
  return(list(tmax_data=tmax_data,tmin_data=tmin_data,prcp_data=prcp_data,tave_data=tave_data))
}

ave_of_neighbors<-function(temp_data, ind_change){
  row<-ind_change[1]
  col<-ind_change[2]
  slice<-ind_change[3]
  neighbors<-list(up=c(row+1,col),down=c(row-1,col),right=c(row,col+1),left=c(row,col-1))
  #figure out which neighbors to select for an average.
  if(row==1) { neighbors$down<-NULL }
  if(col==1) { neighbors$left<-NULL }
  if(row==dim(temp_data)[1]) { neighbors$up<-NULL }
  if(col==dim(temp_data)[2]) { neighbors$right<-NULL }
  vals<-c()
  for(neighbor in neighbors) { vals<-append(vals,temp_data[neighbor[1],neighbor[2],slice]) }
  return(mean(vals, na.rm = TRUE))
}

fill_nans<-function(temp_data){
  no_data_cells<-is.na(temp_data)
  data_cells<-array(dim=dim(no_data_cells))
  data_cells[,,1:dim(no_data_cells)[3]]<-!apply(is.na(temp_data), c(1,2), all)
  inds_change<-which(data_cells & no_data_cells,arr.ind=TRUE)
  # pass temp_data and inds_change to a function using apply over margins of inds_change
  rep_vals<-apply(inds_change,1,ave_of_neighbors,temp_data=temp_data)
  rep_cell<-1
  for(rep in rep_vals) { 
    temp_data[inds_change[rep_cell,1],inds_change[rep_cell,2],inds_change[rep_cell,3]]<-rep 
    rep_cell<-rep_cell+1
  }
  return(temp_data)
}