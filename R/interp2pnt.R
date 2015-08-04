#' Bilinear and Bicubic Interpolation to Points
#' 
#' This code was includes bicubic interpolation and bilinear interpolation
#' adapted from Numerical Recipes in C: The are of scientific computing
#' \url{http://www.nrbook.com/nr3/} (chapter 3... bicubic interpolation) and a
#' bicubic interpolation from \url{http://www.paulinternet.nl/?page=bicubic} in
#' java code.\cr\cr Inputs are a list of points to interpolate to from raster
#' objects of class 'asc' (adehabitat package), 'RasterLayer' (raster package)
#' or 'SpatialGridDataFrame' (sp package).
#' 
#' 
#' @param mat a matrix of data that can be a raster matrix of class 'asc'
#' (adehabitat package), 'RasterLayer' (raster package) or
#' 'SpatialGridDataFrame' (sp package)\cr NA values are not permitted.. data
#' must be complete.
#' @param xout a vector of data representing x coordinates of the output grid.
#' Resulting grid must have square cell sizes if mat is of class 'asc',
#' 'RasterLayer' or 'SpatialGridDataFrame'.
#' @param yout a vector of data representing x coordinates of the output grid.
#' Resulting grid must have square cell sizes if mat is of class 'asc',
#' 'RasterLayer' or 'SpatialGridDataFrame'.
#' @param xin a vector identifying the locations of the columns of the input
#' data matrix. These are automatically populated if mat is of class 'asc',
#' 'RasterLayer' or 'SpatialGridDataFrame'.
#' @param yin a vector identifying the locations of the rows of the input data
#' matrix. These are automatically populated if mat is of class 'asc',
#' 'RasterLayer' or 'SpatialGridDataFrame'.
#' @param type an integer value representing the type of interpolation method
#' used.\cr 1 - bilinear adapted from Numerical Recipes in C\cr 2 - bicubic
#' adapted from Numerical Recipes in C\cr 3 - bicubic adapted from online java
#' code
#' @return A vector of values in the same order and length of the inputs
#' \code{xout} and \code{yout}.
#' @author Jeremy VanDerWal \email{jjvanderwal@@gmail.com}
#' @seealso \code{\link{interp2grid}}
#' @importFrom SDMTools getXYcoords
#' @export
#' @examples
#' \dontrun{
#' 
#' #create some data
#' tx = seq(0,3,0.1)
#' ty = seq(0,3,0.1)
#' txy = data.frame(x=tx,y=ty)
#' 
#' #create a matrix
#' tmat = matrix(runif(16,1,16),nr=4)
#' 
#' #do the interpolations
#' txy$bilinear1 = interp2pnt(tmat,tx,ty,type=1)
#' txy$bicubic2 = interp2pnt(tmat,tx,ty,type=2)
#' txy$bicubic3 = interp2pnt(tmat,tx,ty,type=3)
#' 
#' #look at the data
#' head(txy)
#' }
#' 
interp2pnt <-
function(mat,xout,yout,xin=NULL,yin=NULL,type=2) {
	warning('THIS FUNCTION IS NOT WORKING PROPOERLY... C Code must be checked!!!')

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
	#check the lengths of xout & yout to be the same length
	if (length(xout) != length(yout)) stop('xout & yout must be of the same lengths')
	#ensure type is 1, 2 or 3
	if (!(type %in% 1:3)) stop('type must be a single numeric value of 1, 2 or 3. See help file')
	
	#ensure order of yout & xout is y ... this is needed for the c code...
	torder = order(yout); yout = yout[torder]; xout = xout[torder]
	
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
		out = .Call('interp2pnts',mat,mat.x,mat.y,xout,yout,as.integer(type))
	} else { #if of a different raster type
		#do the interpolation
		out = .Call('interp2pnts',mat,mat.y,mat.x,yout,xout,as.integer(type))
	}
	
	#put the order back to the original
	out = out[order(torder)]
	
	#return the value
	return(out)
}

