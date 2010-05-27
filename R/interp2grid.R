interp2grid <-
function(mat,xout,yout,xin=NULL,yin=NULL,type=2) {
	#check input for class for returning info
	if (class(mat) == 'asc') { 
		attrib = 'asc'
	} else if (any(class(mat) %in% 'RasterLayer')) {
		attrib = 'raster'; mat = asc.from.raster(mat)
	} else if (any(class(mat) == 'SpatialGridDataFrame')) {
		attrib = 'sp'; mat = asc.from.sp(mat)
	} else {
		attrib = NULL
	}
	
	#check to ensure matrix
	mat = try(as.matrix(mat))
	if (!is.matrix(mat)) stop('objects must be a matrix')
	if (length(which(is.na(mat)))>0) warning('missing values in matrix can cause interpolation to stop if value used in interpolation')
	#ensure type is 1, 2 or 3
	if (!(type %in% 1:3)) stop('type must be a single numeric value of 1, 2 or 3. See help file')
	#ensure xout & yout are in order and cell sizes are square
	xout = sort(xout); yout = sort(yout)
	if (mean(diff(xout))!=mean(diff(yout))) stop('differences in xout/yout must be consistent & identical')
	
	#get the xy of mat
	if (is.null(xin) | is.null(yin)){
		if (any(class(mat) == 'asc')) { #check to ensure mat is of class asc & use getXYcoords
			mat.x = getXYcoords(mat)$x; mat.y = getXYcoords(mat)$y
		} else { #use the dimensions of the matrix
			mat.x = 1:dim(mat)[2]-1; mat.y = 1:dim(mat)[1]-1
		}
	} else { mat.x = xin; mat.y = yin }
	
	#check to ensure all point fall within the boundaries of the matrix
	extents.x = range(mat.x); extents.y = range(mat.y)
	if (!(min(xout,na.rm=TRUE)>=extents.x[1] & max(xout,na.rm=TRUE)<=extents.x[2])) stop('interpolation data falls outside the input data boundaries')
	if (!(min(yout,na.rm=TRUE)>=extents.y[1] & max(yout,na.rm=TRUE)<=extents.y[2])) stop('interpolation data falls outside the input data boundaries')
	
	#if attrib is null ... return a basic matrix
	if (is.null(attrib)) {
		#do the interpolation
		out = .Call('interp2grid',mat,mat.x,mat.y,xout,yout,as.integer(type))
		out = matrix(out,nr=length(yout),byrow=FALSE)
		rownames(out)=yout;colnames(out)=xout
	} else { #if of a different raster type
		#do the interpolation
		out = .Call('interp2grid',mat,mat.y,mat.x,yout,xout,as.integer(type))
		#convert to asc file
		out = as.asc(matrix(out,nr=length(xout),byrow=FALSE),xll=min(xout),yll=min(yout),cellsize=mean(diff(xout)),type="numeric")
		if (attrib=='raster') out = raster.from.asc(out)
		if (attrib=='sp') out = sp.from.asc(out)
	}
	
	#return the value
	return(out)
}

