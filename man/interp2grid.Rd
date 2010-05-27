\name{interp2grid}
\alias{interp2grid}

\title{ Bilinear and Bicubic Interpolation to Grid }

\description{
	This code was includes bicubic interpolation and bilinear interpolation adapted from Numerical Recipes in C: The are of scientific computing \url{http://www.nrbook.com/nr3/} (chapter 3... bicubic interpolation) and a bicubic interpolation from \url{http://www.paulinternet.nl/?page=bicubic} in java code.\cr\cr	
	Inputs are a list of points representing grid coordinates to interpolate to from raster objects of class 'asc' (adehabitat package), 'RasterLayer' (raster package) or 'SpatialGridDataFrame' (sp package).
}

\usage{ interp2grid(mat,xout,yout,xin=NULL,yin=NULL,type=2) }

\arguments{
	\item{mat}{a matrix of data that can be a raster matrix of class 'asc' (adehabitat package), 'RasterLayer' (raster package) or 'SpatialGridDataFrame' (sp package)\cr
	NA values are not permitted.. data must be complete.}
	\item{xout}{a vector of data representing x coordinates of the output grid. Resulting grid must have square cell sizes if mat is of class 'asc', 'RasterLayer' or 'SpatialGridDataFrame'.}
	\item{yout}{a vector of data representing x coordinates of the output grid. Resulting grid must have square cell sizes if mat is of class 'asc', 'RasterLayer' or 'SpatialGridDataFrame'.}
	\item{xin}{a vector identifying the locations of the columns of the input data matrix. These are automatically populated if mat is of class 'asc', 'RasterLayer' or 'SpatialGridDataFrame'.}
	\item{yin}{a vector identifying the locations of the rows of the input data matrix. These are automatically populated if mat is of class 'asc', 'RasterLayer' or 'SpatialGridDataFrame'.}
	\item{type}{an integer value representing the type of interpolation method used.\cr
	1 - bilinear adapted from Numerical Recipes in C\cr
	2 - bicubic adapted from Numerical Recipes in C\cr
	3 - bicubic adapted from online java code}
}

\value{ Returns a matrix of the originating class. }

\author{ Jeremy VanDerWal \email{jjvanderwal@gmail.com} }

\seealso{ \code{\link{interp2pnt}} }  

\examples{

#create some data
tx = seq(0,3,0.1)
ty = seq(0,3,0.1)

#create a matrix
tmat = matrix(runif(16,1,16),nr=4)

#do the interpolations
bilinear1 = interp2grid(tmat,tx,ty,type=1)
bicubic2 = interp2grid(tmat,tx,ty,type=2)
bicubic3 = interp2grid(tmat,tx,ty,type=3)

#show the plots
#png(filename = "trials1.png",width=480*4,height=480*4,pointsize=20)
	par(mfrow=c(2,2),cex=1)
	image(tmat,main='base',zlim=c(0,16),col=heat.colors(100))	 
	image(bilinear1,main='bilinear',zlim=c(0,16),col=heat.colors(100))	 
	image(bicubic2,main='bicubic2',zlim=c(0,16),col=heat.colors(100))	 
	image(bicubic3,main='bicubic3',zlim=c(0,16),col=heat.colors(100))	 
#dev.off()


}
