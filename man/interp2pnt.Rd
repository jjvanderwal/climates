\name{interp2pnt}
\alias{interp2pnt}

\title{ Bilinear and Bicubic Interpolation to Points }

\description{
	This code was includes bicubic interpolation and bilinear interpolation adapted from Numerical Recipes in C: The are of scientific computing \url{http://www.nrbook.com/nr3/} (chapter 3... bicubic interpolation) and a bicubic interpolation from \url{http://www.paulinternet.nl/?page=bicubic} in java code.\cr\cr	
	Inputs are a list of points to interpolate to from raster objects of class 'asc' (adehabitat package), 'RasterLayer' (raster package) or 'SpatialGridDataFrame' (sp package).
}

\usage{ interp2grid(mat,xout,yout,xin=NULL,yin=NULL,type=2) }

\arguments{
	\item{mat}{a matrix of data that can be a raster matrix of class 'asc' (adehabitat package), 'RasterLayer' (raster package) or 'SpatialGridDataFrame' (sp package)\n
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

\value{ A vector of values in the same order and length of the inputs \code{xout} and \code{yout}. }

\author{ Jeremy VanDerWal \email{jjvanderwal@gmail.com} }

\examples{

#create some data
tx = seq(0,3,0.1)
ty = seq(0,3,0.1)
txy = data.frame(x=tx,y=ty)

#create a matrix
tmat = matrix(runif(16,1,16),nr=4)

#do the interpolations
txy$bilinear1 = interp2pnt(tmat,tx,ty,type=1)
txy$bicubic2 = interp2pnt(tmat,tx,ty,type=2)
txy$bicubic3 = interp2pnt(tmat,tx,ty,type=3)

#look at the data
head(txy)

}
