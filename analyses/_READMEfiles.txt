Started 22 June 2023
By Lizzie 

Files info!

calcclimatemetrics.R -- Reads in historical climate data, retreads it for sims, and plots it

calcclimatesims.R -- using the detrended historical data it creates simulated climate (and necessary PHENOFIT input files for sims)

calcclimatemetricsfuture.R -- this is code to plot the climate projections data (from Victor), which worked okay in July 2023, but then we switched to new data and I don't quite have it running nicely anymore. It needs WORK (see _dothis and log notes as well as notes in R files). 

calcclimatemetricsfuture_ssp585.R -- a Nov 2023 copy of calcclimatemetricsfuture.R but references the ssp585 data instead of the lower SSP. 

phenofitplot.R -- plots PHENOFIT results from historical climate

phenofitplotsims.R -- plots PHENOFIT results from simulations

phenofitsims/ -- code here is constantly overwritten as you run the simulation code (calcclimatesims.R) so not saved. 