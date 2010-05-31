/* File: interpolate.c */ 
/*
this code was includes bicubic interpolation and bilinear interpolation adapted from 
Numerical Recipes in C: The are of scientific computing http://www.nrbook.com/nr3/ 
(chapter 3... bicubic interpolation) and a bicubic interpolation from 
http://www.paulinternet.nl/?page=bicubic in java code

*/
#include <R.h> 
#include <Rinternals.h>
/* 
tdata is a matrix of data values which are being used to interpolate from
mwx and mwy are vectors of points representing locations to be interpolated to
type is 1 for bilinear, 2 for bicubic (numeric recipes) and 3 for bicubic (java code)

interp2grid assumes outX & outY represent a output grid to write to...
*/
SEXP interp2grid(SEXP tdata, SEXP inX, SEXP inY, SEXP outX, SEXP outY, SEXP type) {
	//define the pointers for the data
	PROTECT(tdata = coerceVector(tdata, REALSXP));
	double *data = REAL(tdata); //this is the full data basis for interpolations
	double *Xin = REAL(coerceVector(inX,REALSXP)), *Yin = REAL(coerceVector(inY,REALSXP)); //location of input data to interpolate from
	double *Xout = REAL(coerceVector(outX,REALSXP)), *Yout = REAL(coerceVector(outY,REALSXP)); //points to interpolate to
	int nXout = length(outX), nYout = length(outY); //get the length of the number of points to interpolate
	int nXin=length(inX), nYin=length(inY);//get the length of the vectors of the input points
	int ttype = INTEGER(coerceVector(type,INTSXP))[0]; //identify the type of interpolation
	
	//define some other common variables
	int xx,yy,ii,jj,kk;
	double t,u;
	
	//setup the output vector and allocate everything as NA to begin with
	double *out; SEXP ans;
	PROTECT(ans = allocVector(REALSXP, nXout*nYout));
	out = REAL(ans); //pointer to output dataset
	
	//if bilinear interpolation
	if (ttype==1){ //if bilinear
		double y1,y2,y3,y4; //define necessary variables
		for (yy=0;yy<nYin-1;yy++){//start cycling through each of the latitudes
			for(xx=0;xx<nXin-1;xx++){//start cycling through each of the longitudes
				//define the 4 points of interest
				y1=data[yy+nYin*xx]; y2=data[(yy+1)+nYin*xx]; y3=data[(yy+1)+nYin*(xx+1)]; y4=data[yy+nYin*(xx+1)];
				for (jj=0;jj<nYout;jj++){ //cycle through each of the output y values
					if(Yout[jj]>=Yin[yy] && Yout[jj]<=Yin[yy+1]){//if the output y values are within the range of data of interest
						for (ii=0;ii<nXout;ii++){ //cycle through each of the output x values
							if(Xout[ii]>=Xin[xx] && Xout[ii]<=Xin[xx+1]){ //if the output x values are within the range of data of interest
								u = (Yout[jj]-Yin[yy])/(Yin[yy+1]-Yin[yy]); //Calulate t & u
								t = (Xout[ii]-Xin[xx])/(Xin[xx+1]-Xin[xx]);
								out[jj+ii*nYout] = (1.-t)*(1.-u)*y1 + t*(1.-u)*y4 + (1.-t)*u*y2 + t*u*y3;//work out the interpolation							
							}
						}
					}					
				}				
			}
		}
	}
	//if either of the bicubic interopolations
	if (ttype==2 || ttype==3){
		//define the weights matrix
		static int wt[16][16] = {{1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
			{0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0},
			{-3,0,0,3,0,0,0,0,-2,0,0,-1,0,0,0,0},
			{2,0,0,-2,0,0,0,0,1,0,0,1,0,0,0,0},
			{0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0},
			{0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0},
			{0,0,0,0,-3,0,0,3,0,0,0,0,-2,0,0,-1},
			{0,0,0,0,2,0,0,-2,0,0,0,0,1,0,0,1},
			{-3,3,0,0,-2,-1,0,0,0,0,0,0,0,0,0,0},
			{0,0,0,0,0,0,0,0,-3,3,0,0,-2,-1,0,0},
			{9,-9,9,-9,6,3,-3,-6,6,-6,-3,3,4,2,1,2},
			{-6,6,-6,6,-4,-2,2,4,-3,3,3,-3,-2,-1,-1,-2},
			{2,-2,0,0,1,1,0,0,0,0,0,0,0,0,0,0},
			{0,0,0,0,0,0,0,0,2,-2,0,0,1,1,0,0},
			{-6,6,-6,6,-3,-3,3,3,-4,4,2,-2,-2,-2,-1,-1},
			{4,-4,4,-4,2,2,-2,-2,2,-2,-2,2,1,1,1,1}};
	
		//define a matrix of 16 points for the interpolations
		double p[4][4];
		for (yy=0;yy<nYin-1;yy++){//start cycling through each of the latitudes
			for(xx=0;xx<nXin-1;xx++){//start cycling through each of the longitudes
				int check = 0; //check if any points fall within this cell
				for (jj=0;jj<nYout;jj++){ //cycle through each of the output y values
					if(Yout[jj]>=Yin[yy]) { //if the output y values are within the range of data of interest
						if(Yout[jj]>Yin[yy+1]){ break; }
						if(Xout[jj]>=Xin[xx] && Xout[jj]<=Xin[xx+1]){ check=1;break; } //stop if it hits a point within the cell
					}
				}
				if (check==1) { 
					//populate the matrix of 16 values
					//define the 4 key values
					p[1][1] = data[yy+nYin*xx];
					p[2][1] = data[(yy+1)+nYin*xx];
					p[2][2] = data[(yy+1)+nYin*(xx+1)];
					p[1][2] = data[yy+nYin*(xx+1)];
					//define the points in outer corners
					if(yy==0){ //if first row of data
						p[0][0] = data[yy+nYin*xx]; p[0][1] = data[yy+nYin*xx];
						p[0][2] = data[yy+nYin*(xx+1)]; p[0][3] = data[yy+nYin*(xx+1)];
					} else { //not first row of data
						if (xx==0) {p[0][0] = p[1][1];} else {p[0][0] = data[(yy-1)+nYin*(xx-1)];}
						p[0][1] = data[(yy-1)+nYin*xx]; p[0][2] = data[(yy-1)+nYin*(xx+1)]; 
						if (xx==nXin-2) {p[0][3] = p[1][2];} else {p[0][3] = data[(yy-1)+nYin*(xx+2)];}
					}
					if(yy==nYin-2){ //if last row of data
						p[3][0] = data[(yy+1)+nYin*xx]; p[3][1] = data[(yy+1)+nYin*xx];
						p[3][2] = data[(yy+1)+nYin*(xx+1)]; p[3][3] = data[(yy+1)+nYin*(xx+1)];
					} else { //not last row of data
						if (xx==0) {p[3][0] = p[2][1];} else {p[3][0] = data[(yy+2)+nYin*(xx-1)];}
						p[3][1] = data[(yy+2)+nYin*xx]; p[3][2] = data[(yy+2)+nYin*(xx+1)]; 
						if (xx==nXin-2) {p[3][3] = p[2][2];} else {p[3][3] = data[(yy+2)+nYin*(xx+2)];}
					}				
					if(xx==0){ //if last first column of data
						p[1][0] = p[1][1]; p[2][0] = p[2][1];
					} else { //if not first column
						p[1][0] = data[yy+nYin*(xx-1)]; p[2][0] = data[(yy+1)+nYin*(xx-1)];
					}
					if(xx==nXin-2){ //if last first column of data
						p[1][3] = p[1][2]; p[2][3] = p[2][2];
					} else { //if not first column
						p[1][3] = data[yy+nYin*(xx+2)]; p[2][3] = data[(yy+1)+nYin*(xx+2)];
					}
					
					//work out the issues given the different interpolation
					if (ttype==2){ //if bicubic from numeric recipes
						int xpoi[4] = {1,2,2,1}, ypoi[4] = {1,1,2,2}; //get the 4 points bounding the middle square
						double y[4],y1[4],y2[4],y12[4]; //storing values, derivitives and cross derivitives
						double ansy;//a variable for storing output
						for (ii=0;ii<4;ii++){
							y[ii] = p[ypoi[ii]][xpoi[ii]];
							y1[ii] = (p[ypoi[ii]+1][xpoi[ii]]-p[ypoi[ii]-1][xpoi[ii]])/2;
							y2[ii] = (p[ypoi[ii]][xpoi[ii]+1]-p[ypoi[ii]][xpoi[ii]-1])/2;
							y12[ii] = (p[ypoi[ii]+1][xpoi[ii]+1]-p[ypoi[ii]+1][xpoi[ii]-1]-p[ypoi[ii]-1][xpoi[ii]+1]+p[ypoi[ii]-1][xpoi[ii]-1])/4;
						}
						//get the cell sizes in each direction
						double d1 = Xin[xx+1]-Xin[xx], d2 = Yin[yy+1]-Yin[yy];
						//define some other values... c will be used later
						double c[4][4], d1d2, cl[16], x[16], xxx;
						d1d2=d1*d2;
						for (ii=0;ii<4;ii++){ //pack a temporary vector
							x[ii] = y[ii];
							x[ii+4] = y1[ii]*d1;
							x[ii+8] = y2[ii]*d2;
							x[ii+12] = y12[ii]*d1d2;
						}
						for (ii=0;ii<16;ii++){//matrix multiply the data
							xxx=0.0;
							for (jj=0;jj<16;jj++) xxx += wt[ii][jj]*x[jj];
							cl[ii]=xxx;
						}
						int l = 0;
						for (ii=0;ii<4;ii++) for (jj=0;jj<4;jj++) c[jj][ii] = cl[l++]; //unpack cl into matrix c
						//start cycleing though the points to be output
						for (jj=0;jj<nYout;jj++){ //cycle through each of the output y values
							if(Yout[jj]>=Yin[yy]) && Yout[jj]>Yin[yy+1]){ 
								for (ii=0;ii<nXout;ii++){ //cycle through each of the output x values
									if(Xout[ii]>=Xin[xx] && Xout[ii]<=Xin[xx+1]){
										//define the x & y values as proportions of the distance
										t = (Yout[jj]-Yin[yy])/(Yin[yy+1]-Yin[yy]);
										u = (Xout[ii]-Xin[xx])/(Xin[xx+1]-Xin[xx]);
										// do the interpolation
										ansy=0.0;
										for (kk=3;kk>=0;kk--){ ansy=t*ansy+((c[kk][3]*u+c[kk][2])*u+c[kk][1])*u+c[kk][0]; }
										out[jj+ii*nYout] = ansy;	
									}
								}
							}
						}
					}
					
					if (ttype==3){ //if other bicubic from java
						//do the necessary calulations
						double a00 = p[1][1];
						double a01 = .5*p[1][2] - .5*p[1][0];
						double a02 = p[1][0] - 2.5*p[1][1] + 2*p[1][2] - .5*p[1][3];
						double a03 = -.5*p[1][0] + 1.5*p[1][1] - 1.5*p[1][2] + .5*p[1][3];
						double a10 = .5*p[2][1] - .5*p[0][1];
						double a11 = .25*p[0][0] - .25*p[0][2] - .25*p[2][0] + .25*p[2][2];
						double a12 = -.5*p[0][0] + 1.25*p[0][1] - p[0][2] + .25*p[0][3] + .5*p[2][0] - 1.25*p[2][1] + p[2][2] - .25*p[2][3];
						double a13 = .25*p[0][0] - .75*p[0][1] + .75*p[0][2] - .25*p[0][3] - .25*p[2][0] + .75*p[2][1] - .75*p[2][2] + .25*p[2][3];
						double a20 = p[0][1] - 2.5*p[1][1] + 2*p[2][1] - .5*p[3][1];
						double a21 = -.5*p[0][0] + .5*p[0][2] + 1.25*p[1][0] - 1.25*p[1][2] - p[2][0] + p[2][2] + .25*p[3][0] - .25*p[3][2];
						double a22 = p[0][0] - 2.5*p[0][1] + 2*p[0][2] - .5*p[0][3] - 2.5*p[1][0] + 6.25*p[1][1] - 5*p[1][2] + 1.25*p[1][3] + 2*p[2][0] - 5*p[2][1] + 4*p[2][2] - p[2][3] - .5*p[3][0] + 1.25*p[3][1] - p[3][2] + .25*p[3][3];
						double a23 = -.5*p[0][0] + 1.5*p[0][1] - 1.5*p[0][2] + .5*p[0][3] + 1.25*p[1][0] - 3.75*p[1][1] + 3.75*p[1][2] - 1.25*p[1][3] - p[2][0] + 3*p[2][1] - 3*p[2][2] + p[2][3] + .25*p[3][0] - .75*p[3][1] + .75*p[3][2] - .25*p[3][3];
						double a30 = -.5*p[0][1] + 1.5*p[1][1] - 1.5*p[2][1] + .5*p[3][1];
						double a31 = .25*p[0][0] - .25*p[0][2] - .75*p[1][0] + .75*p[1][2] + .75*p[2][0] - .75*p[2][2] - .25*p[3][0] + .25*p[3][2];
						double a32 = -.5*p[0][0] + 1.25*p[0][1] - p[0][2] + .25*p[0][3] + 1.5*p[1][0] - 3.75*p[1][1] + 3*p[1][2] - .75*p[1][3] - 1.5*p[2][0] + 3.75*p[2][1] - 3*p[2][2] + .75*p[2][3] + .5*p[3][0] - 1.25*p[3][1] + p[3][2] - .25*p[3][3];
						double a33 = .25*p[0][0] - .75*p[0][1] + .75*p[0][2] - .25*p[0][3] - .75*p[1][0] + 2.25*p[1][1] - 2.25*p[1][2] + .75*p[1][3] + .75*p[2][0] - 2.25*p[2][1] + 2.25*p[2][2] - .75*p[2][3] - .25*p[3][0] + .75*p[3][1] - .75*p[3][2] + .25*p[3][3];

						//work through all points to interpolate
						double x1,x2,x3,y1,y2,y3;
						//start cycleing though the points to be output
						for (jj=0;jj<nYout;jj++){ //cycle through each of the output y values
							if(Yout[jj]>=Yin[yy]) { //if the output y values are within the range of data of interest
								if(Yout[jj]>Yin[yy+1]){ break; }
								for (ii=0;ii<nXout;ii++){ //cycle through each of the output x values
									if(Xout[ii]>=Xin[xx] && Xout[ii]<=Xin[xx+1]){
										//define the x & y values
										x1 = (Yout[jj]-Yin[yy])/(Yin[yy+1]-Yin[yy]); x2 = x1 * x1; x3 = x2 * x1;
										y1 = (Xout[ii]-Xin[xx])/(Xin[xx+1]-Xin[xx]); y2 = y1 * y1; y3 = y2 * y1;
										out[jj+ii*nYout] = a00 + a01 * y1 + a02 * y2 + a03 * y3 +
											a10 * x1 + a11 * x1 * y1 + a12 * x1 * y2 + a13 * x1 * y3 +
											a20 * x2 + a21 * x2 * y1 + a22 * x2 * y2 + a23 * x2 * y3 + 
											a30 * x3 + a31 * x3 * y1 + a32 * x3 * y2 + a33 * x3 * y3;
									}
								}
							}
						}
					}								
				}
			}
		}
	}
	//return the output data
	UNPROTECT(2);
    return(ans); 
}

/* 
interp2pnts assumes outX & outY represent a output set of points...
*/
SEXP interp2pnts(SEXP tdata, SEXP inX, SEXP inY, SEXP outX, SEXP outY, SEXP type) {
	//define the pointers for the data
	PROTECT(tdata = coerceVector(tdata, REALSXP));
	double *data = REAL(tdata); //this is the full data basis for interpolations
	double *Xin = REAL(coerceVector(inX,REALSXP)), *Yin = REAL(coerceVector(inY,REALSXP)); //location of input data to interpolate from
	double *Xout = REAL(coerceVector(outX,REALSXP)), *Yout = REAL(coerceVector(outY,REALSXP)); //points to interpolate to
	int nXout = length(outX), nYout = length(outY); //get the length of the number of points to interpolate
	int nXin=length(inX), nYin=length(inY);//get the length of the vectors of the input points
	int ttype = INTEGER(coerceVector(type,INTSXP))[0]; //identify the type of interpolation
	
	//define some other common variables
	int xx,yy,ii,jj,kk;
	double t,u;
	
	//setup the output vector and allocate everything as NA to begin with
	double *out; SEXP ans;
	PROTECT(ans = allocVector(REALSXP, nXout));
	out = REAL(ans); //pointer to output dataset
	
	//if bilinear interpolation
	if (ttype==1){ //if bilinear
		double y1,y2,y3,y4; //define necessary variables
		for (yy=0;yy<nYin-1;yy++){//start cycling through each of the latitudes
			for(xx=0;xx<nXin-1;xx++){//start cycling through each of the longitudes
				//define the 4 points of interest
				y1=data[yy+nYin*xx]; y2=data[(yy+1)+nYin*xx]; y3=data[(yy+1)+nYin*(xx+1)]; y4=data[yy+nYin*(xx+1)];
				for (jj=0;jj<nYout;jj++){ //cycle through each of the output y values
					if(Yout[jj]>=Yin[yy] && Yout[jj]<=Yin[yy+1]){//if the output y values are within the range of data of interest
						if(Xout[jj]>=Xin[xx] && Xout[jj]<=Xin[xx+1]){ //if the output x values are within the range of data of interest
							u = (Yout[jj]-Yin[yy])/(Yin[yy+1]-Yin[yy]); //Calulate t & u
							t = (Xout[jj]-Xin[xx])/(Xin[xx+1]-Xin[xx]);
							out[jj] = (1.-t)*(1.-u)*y1 + t*(1.-u)*y4 + (1.-t)*u*y2 + t*u*y3;//work out the interpolation							
						}
					}					
				}				
			}
		}
	}
	//if either of the bicubic interopolations
	if (ttype==2 || ttype==3){
		//define the weights matrix
		static int wt[16][16] = {{1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
			{0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0},
			{-3,0,0,3,0,0,0,0,-2,0,0,-1,0,0,0,0},
			{2,0,0,-2,0,0,0,0,1,0,0,1,0,0,0,0},
			{0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0},
			{0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0},
			{0,0,0,0,-3,0,0,3,0,0,0,0,-2,0,0,-1},
			{0,0,0,0,2,0,0,-2,0,0,0,0,1,0,0,1},
			{-3,3,0,0,-2,-1,0,0,0,0,0,0,0,0,0,0},
			{0,0,0,0,0,0,0,0,-3,3,0,0,-2,-1,0,0},
			{9,-9,9,-9,6,3,-3,-6,6,-6,-3,3,4,2,1,2},
			{-6,6,-6,6,-4,-2,2,4,-3,3,3,-3,-2,-1,-1,-2},
			{2,-2,0,0,1,1,0,0,0,0,0,0,0,0,0,0},
			{0,0,0,0,0,0,0,0,2,-2,0,0,1,1,0,0},
			{-6,6,-6,6,-3,-3,3,3,-4,4,2,-2,-2,-2,-1,-1},
			{4,-4,4,-4,2,2,-2,-2,2,-2,-2,2,1,1,1,1}};
	
		//define a matrix of 16 points for the interpolations
		double p[4][4];
		for (yy=0;yy<nYin-1;yy++){//start cycling through each of the latitudes
			for(xx=0;xx<nXin-1;xx++){//start cycling through each of the longitudes
				int check = 0; //check if any points fall within this cell
				for (jj=0;jj<nYout;jj++){ //cycle through each of the output y values
					if(Yout[jj]>=Yin[yy]) { //if the output y values are within the range of data of interest
						if(Yout[jj]>Yin[yy+1]){ break; }
						if(Xout[jj]>=Xin[xx] && Xout[jj]<=Xin[xx+1]){ check=1;break; } //stop if it hits a point within the cell
					}
				}
				if (check==1) { 
					//populate the matrix of 16 values
					//define the 4 key values
					p[1][1] = data[yy+nYin*xx];
					p[2][1] = data[(yy+1)+nYin*xx];
					p[2][2] = data[(yy+1)+nYin*(xx+1)];
					p[1][2] = data[yy+nYin*(xx+1)];
					//define the points in outer corners
					if(yy==0){ //if first row of data
						p[0][0] = data[yy+nYin*xx]; p[0][1] = data[yy+nYin*xx];
						p[0][2] = data[yy+nYin*(xx+1)]; p[0][3] = data[yy+nYin*(xx+1)];
					} else { //not first row of data
						if (xx==0) {p[0][0] = p[1][1];} else {p[0][0] = data[(yy-1)+nYin*(xx-1)];}
						p[0][1] = data[(yy-1)+nYin*xx]; p[0][2] = data[(yy-1)+nYin*(xx+1)]; 
						if (xx==nXin-2) {p[0][3] = p[1][2];} else {p[0][3] = data[(yy-1)+nYin*(xx+2)];}
					}
					if(yy==nYin-2){ //if last row of data
						p[3][0] = data[(yy+1)+nYin*xx]; p[3][1] = data[(yy+1)+nYin*xx];
						p[3][2] = data[(yy+1)+nYin*(xx+1)]; p[3][3] = data[(yy+1)+nYin*(xx+1)];
					} else { //not last row of data
						if (xx==0) {p[3][0] = p[2][1];} else {p[3][0] = data[(yy+2)+nYin*(xx-1)];}
						p[3][1] = data[(yy+2)+nYin*xx]; p[3][2] = data[(yy+2)+nYin*(xx+1)]; 
						if (xx==nXin-2) {p[3][3] = p[2][2];} else {p[3][3] = data[(yy+2)+nYin*(xx+2)];}
					}				
					if(xx==0){ //if last first column of data
						p[1][0] = p[1][1]; p[2][0] = p[2][1];
					} else { //if not first column
						p[1][0] = data[yy+nYin*(xx-1)]; p[2][0] = data[(yy+1)+nYin*(xx-1)];
					}
					if(xx==nXin-2){ //if last first column of data
						p[1][3] = p[1][2]; p[2][3] = p[2][2];
					} else { //if not first column
						p[1][3] = data[yy+nYin*(xx+2)]; p[2][3] = data[(yy+1)+nYin*(xx+2)];
					}
					
					//work out the issues given the different interpolation
					if (ttype==2){ //if bicubic from numeric recipes
						int xpoi[4] = {1,2,2,1}, ypoi[4] = {1,1,2,2}; //get the 4 points bounding the middle square
						double y[4],y1[4],y2[4],y12[4]; //storing values, derivitives and cross derivitives
						double ansy;//a variable for storing output
						for (ii=0;ii<4;ii++){
							y[ii] = p[ypoi[ii]][xpoi[ii]];
							y1[ii] = (p[ypoi[ii]+1][xpoi[ii]]-p[ypoi[ii]-1][xpoi[ii]])/2;
							y2[ii] = (p[ypoi[ii]][xpoi[ii]+1]-p[ypoi[ii]][xpoi[ii]-1])/2;
							y12[ii] = (p[ypoi[ii]+1][xpoi[ii]+1]-p[ypoi[ii]+1][xpoi[ii]-1]-p[ypoi[ii]-1][xpoi[ii]+1]+p[ypoi[ii]-1][xpoi[ii]-1])/4;
						}
						//get the cell sizes in each direction
						double d1 = Xin[xx+1]-Xin[xx], d2 = Yin[yy+1]-Yin[yy];
						//define some other values... c will be used later
						double c[4][4], d1d2, cl[16], x[16], xxx;
						d1d2=d1*d2;
						for (ii=0;ii<4;ii++){ //pack a temporary vector
							x[ii] = y[ii];
							x[ii+4] = y1[ii]*d1;
							x[ii+8] = y2[ii]*d2;
							x[ii+12] = y12[ii]*d1d2;
						}
						for (ii=0;ii<16;ii++){//matrix multiply the data
							xxx=0.0;
							for (jj=0;jj<16;jj++) xxx += wt[ii][jj]*x[jj];
							cl[ii]=xxx;
						}
						int l = 0;
						for (ii=0;ii<4;ii++) for (jj=0;jj<4;jj++) c[jj][ii] = cl[l++]; //unpack cl into matrix c
						//start cycleing though the points to be output
						for (jj=0;jj<nYout;jj++){ //cycle through each of the output y values
							if(Yout[jj]>=Yin[yy]) { //if the output y values are within the range of data of interest
								if(Yout[jj]>Yin[yy+1]){ break; }
								if(Xout[jj]>=Xin[xx] && Xout[jj]<=Xin[xx+1]){
									//define the x & y values as proportions of the distance
									t = (Yout[jj]-Yin[yy])/(Yin[yy+1]-Yin[yy]);
									u = (Xout[jj]-Xin[xx])/(Xin[xx+1]-Xin[xx]);
									// do the interpolation
									ansy=0.0;
									for (kk=3;kk>=0;kk--){ ansy=t*ansy+((c[kk][3]*u+c[kk][2])*u+c[kk][1])*u+c[kk][0]; }
									out[jj] = ansy;	
								}
							}
						}
					}
					
					if (ttype==3){ //if other bicubic from java
						//do the necessary calulations
						double a00 = p[1][1];
						double a01 = .5*p[1][2] - .5*p[1][0];
						double a02 = p[1][0] - 2.5*p[1][1] + 2*p[1][2] - .5*p[1][3];
						double a03 = -.5*p[1][0] + 1.5*p[1][1] - 1.5*p[1][2] + .5*p[1][3];
						double a10 = .5*p[2][1] - .5*p[0][1];
						double a11 = .25*p[0][0] - .25*p[0][2] - .25*p[2][0] + .25*p[2][2];
						double a12 = -.5*p[0][0] + 1.25*p[0][1] - p[0][2] + .25*p[0][3] + .5*p[2][0] - 1.25*p[2][1] + p[2][2] - .25*p[2][3];
						double a13 = .25*p[0][0] - .75*p[0][1] + .75*p[0][2] - .25*p[0][3] - .25*p[2][0] + .75*p[2][1] - .75*p[2][2] + .25*p[2][3];
						double a20 = p[0][1] - 2.5*p[1][1] + 2*p[2][1] - .5*p[3][1];
						double a21 = -.5*p[0][0] + .5*p[0][2] + 1.25*p[1][0] - 1.25*p[1][2] - p[2][0] + p[2][2] + .25*p[3][0] - .25*p[3][2];
						double a22 = p[0][0] - 2.5*p[0][1] + 2*p[0][2] - .5*p[0][3] - 2.5*p[1][0] + 6.25*p[1][1] - 5*p[1][2] + 1.25*p[1][3] + 2*p[2][0] - 5*p[2][1] + 4*p[2][2] - p[2][3] - .5*p[3][0] + 1.25*p[3][1] - p[3][2] + .25*p[3][3];
						double a23 = -.5*p[0][0] + 1.5*p[0][1] - 1.5*p[0][2] + .5*p[0][3] + 1.25*p[1][0] - 3.75*p[1][1] + 3.75*p[1][2] - 1.25*p[1][3] - p[2][0] + 3*p[2][1] - 3*p[2][2] + p[2][3] + .25*p[3][0] - .75*p[3][1] + .75*p[3][2] - .25*p[3][3];
						double a30 = -.5*p[0][1] + 1.5*p[1][1] - 1.5*p[2][1] + .5*p[3][1];
						double a31 = .25*p[0][0] - .25*p[0][2] - .75*p[1][0] + .75*p[1][2] + .75*p[2][0] - .75*p[2][2] - .25*p[3][0] + .25*p[3][2];
						double a32 = -.5*p[0][0] + 1.25*p[0][1] - p[0][2] + .25*p[0][3] + 1.5*p[1][0] - 3.75*p[1][1] + 3*p[1][2] - .75*p[1][3] - 1.5*p[2][0] + 3.75*p[2][1] - 3*p[2][2] + .75*p[2][3] + .5*p[3][0] - 1.25*p[3][1] + p[3][2] - .25*p[3][3];
						double a33 = .25*p[0][0] - .75*p[0][1] + .75*p[0][2] - .25*p[0][3] - .75*p[1][0] + 2.25*p[1][1] - 2.25*p[1][2] + .75*p[1][3] + .75*p[2][0] - 2.25*p[2][1] + 2.25*p[2][2] - .75*p[2][3] - .25*p[3][0] + .75*p[3][1] - .75*p[3][2] + .25*p[3][3];
					
						//work through all points to interpolate
						double x1,x2,x3,y1,y2,y3;
						//start cycleing though the points to be output
						for (jj=0;jj<nYout;jj++){ //cycle through each of the output y values
							if(Yout[jj]>=Yin[yy]) { //if the output y values are within the range of data of interest
								if(Yout[jj]>Yin[yy+1]){ break; }
								if(Xout[jj]>=Xin[xx] && Xout[jj]<=Xin[xx+1]){
									//define the x & y values
									x1 = (Yout[jj]-Yin[yy])/(Yin[yy+1]-Yin[yy]); x2 = x1 * x1; x3 = x2 * x1;
									y1 = (Xout[jj]-Xin[xx])/(Xin[xx+1]-Xin[xx]); y2 = y1 * y1; y3 = y2 * y1;
									out[jj] = a00 + a01 * y1 + a02 * y2 + a03 * y3 +
										a10 * x1 + a11 * x1 * y1 + a12 * x1 * y2 + a13 * x1 * y3 +
										a20 * x2 + a21 * x2 * y1 + a22 * x2 * y2 + a23 * x2 * y3 + 
										a30 * x3 + a31 * x3 * y1 + a32 * x3 * y2 + a33 * x3 * y3;
								}
							}
						}
					}	
				}								
			}
		}
	}
		
	//return the output data
	UNPROTECT(2);
    return(ans); 
}

