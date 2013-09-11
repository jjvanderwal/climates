/* Fuction that calculates the 19 BIOCLIM variables (not all subsets possible)
# 	tmin, tmax and prec must be specified in the function call; if you have tmean you may pass
# 	it, else it will be caculated as (tmax+tmin)/2
# 	Period can be either "month" or "week" see ?bioclim for other vars
*/

#include <R.h> 
#include <Rinternals.h>

//function to get row minimums from a matrix
SEXP rowMin(SEXP tdata) {
	//define the pointers for the data
	PROTECT(tdata = coerceVector(tdata, REALSXP));
	double *data = REAL(tdata); //this is a binary matrix of data
	int *dims = INTEGER(coerceVector(getAttrib(tdata, R_DimSymbol), INTSXP)); //get the dimension of the input matrix
    int nrow = dims[0]; int ncol = dims[1]; //assign the number of rows and columns in the matrix
	
	//setup the output matrix
	SEXP ans; PROTECT(ans = allocVector(REALSXP, nrow));
	double *out = REAL(ans); //pointer to output dataset
	
	//cycle through and copy data to out
	int row, col;
	for (row=0; row<nrow; row++) {
		out[row] = data[row]; // set the first value
		for (col=1; col<ncol; col++) {
			if (out[row] > data[row+nrow*col]) {
				out[row] = data[row+nrow*col]; //keep the minimum
			};
		}	
	}
	
	//return the output data
	UNPROTECT(2);
    return(ans); 
}

//function to get row maximums from a matrix
SEXP rowMax(SEXP tdata) {
	//define the pointers for the data
	PROTECT(tdata = coerceVector(tdata, REALSXP));
	double *data = REAL(tdata); //this is a binary matrix of data
	int *dims = INTEGER(coerceVector(getAttrib(tdata, R_DimSymbol), INTSXP)); //get the dimension of the input matrix
    int nrow = dims[0]; int ncol = dims[1]; //assign the number of rows and columns in the matrix
	
	//setup the output matrix
	SEXP ans; PROTECT(ans = allocVector(REALSXP, nrow));
	double *out = REAL(ans); //pointer to output dataset
	
	//cycle through and copy data to out
	int row, col;
	for (row=0; row<nrow; row++) {
		out[row] = data[row]; // set the first value
		for (col=1; col<ncol; col++) {
			if (out[row] < data[row+nrow*col]) {
				out[row] = data[row+nrow*col]; //keep the max
			};
		}	
	}

	//return the output data
	UNPROTECT(2);
    return(ans); 
}

//function to get row maximums from a matrix
SEXP rowMean(SEXP tdata) {
	//define the pointers for the data
	PROTECT(tdata = coerceVector(tdata, REALSXP));
	double *data = REAL(tdata); //this is a binary matrix of data
	int *dims = INTEGER(coerceVector(getAttrib(tdata, R_DimSymbol), INTSXP)); //get the dimension of the input matrix
    int nrow = dims[0]; int ncol = dims[1]; //assign the number of rows and columns in the matrix
	
	//setup the output matrix
	SEXP ans; PROTECT(ans = allocVector(REALSXP, nrow));
	double *out = REAL(ans); //pointer to output dataset
	
	//cycle through and copy data to out
	int row, col;
	for (row=0; row<nrow; row++) {
		out[row] = data[row]; // set the first value
		for (col=1; col<ncol; col++) {
			out[row] += data[row+nrow*col];
		}
		out[row] /= 12; // set the first value
	}

	//return the output data
	UNPROTECT(2);
    return(ans); 
}

