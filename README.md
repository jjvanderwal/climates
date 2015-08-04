Climates
===========

Methods for working with, downscaling climate, recreating bioclim variables, extracting extreme event information, etc.

Required Packages: sp, rgdal, chron, zoo, climdex.pcic, PCICt, raster
The goal here is to use Imports explicitly, but while the package is in development, Depends will be used as a transition to Imports is made.

Suggested Packages:ncdf, ncdf4, SDMTools, clim.pact
The goal with these packages is to 'require' them at run time. This is against R package checking guidance, but will allow users to use the package without installing ncdf4 and other hard to install packages.

Functions that interact with data require ncdf4. They are:
* dap\_bioclim
* init\_dap
* request\_bbox
* request\_time\_bounds.R
* dap\_daily\_stats
* get\_dap\_data
* get\_time\_dim
* initialize\_NetCDF

