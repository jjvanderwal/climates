#' Bilinear and Bicubic Interpolation to Grid
#' 
#' This code was includes bicubic interpolation and bilinear interpolation
#' adapted from Numerical Recipes in C: The are of scientific computing
#' \url{http://www.nrbook.com/nr3/} (chapter 3... bicubic interpolation) and a
#' bicubic interpolation from \url{http://www.paulinternet.nl/?page=bicubic} in
#' java code.\cr\cr Inputs are a list of points representing grid coordinates
#' to interpolate to from raster objects of class 'asc' (adehabitat package),
#' 'RasterLayer' (raster package) or 'SpatialGridDataFrame' (sp package).
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
#' @return Returns a matrix of the originating class.
#' @author Jeremy VanDerWal \email{jjvanderwal@@gmail.com}
#' @seealso \code{\link{interp2pnt}}
#' @export
#' @examples
#' \dontrun{
#' 
#' #create some data
#' tx = seq(0,3,0.1)
#' ty = seq(0,3,0.1)
#' 
#' #create a matrix
#' tmat = matrix(runif(16,1,16),nr=4)
#' 
#' #do the interpolations
#' bilinear1 = interp2grid(tmat,tx,ty,type=1)
#' bicubic2 = interp2grid(tmat,tx,ty,type=2)
#' bicubic3 = interp2grid(tmat,tx,ty,type=3)
#' 
#' #show the plots
#' #png(filename = "trials1.png",width=480*4,height=480*4,pointsize=20)
#' 	par(mfrow=c(2,2),cex=1)
#' 	image(tmat,main='base',zlim=c(0,16),col=heat.colors(100))	 
#' 	image(bilinear1,main='bilinear',zlim=c(0,16),col=heat.colors(100))	 
#' 	image(bicubic2,main='bicubic2',zlim=c(0,16),col=heat.colors(100))	 
#' 	image(bicubic3,main='bicubic3',zlim=c(0,16),col=heat.colors(100))	 
#' #dev.off()
#' }
#' 
#' 
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
	#if (mean(diff(xout))!=mean(diff(yout))) stop('differences in xout/yout must be consistent & identical')
	
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
		out = matrix(out,nrow=length(yout),byrow=FALSE)
		rownames(out)=yout;colnames(out)=xout
	} else { #if of a different raster type
		#do the interpolation
		out = .Call('interp2grid',mat,mat.y,mat.x,yout,xout,as.integer(type))
		#convert to asc file
		out = as.asc(matrix(out,nrow=length(xout),byrow=FALSE),xll=min(xout),yll=min(yout),cellsize=mean(diff(xout)),type="numeric")
		if (attrib=='raster') out = raster.from.asc(out)
		if (attrib=='sp') out = sp.from.asc(out)
	}
	
	#return the value
	return(out)
}

