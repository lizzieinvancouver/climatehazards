Started 14 April 2023
By Lizzie

Notes on code shared for this project 


<><><><><><><><><><><><>
From Victor Van der Meersch <victor.vandermeersch@cefe.cnrs.fr>

computer_PET.R -- Penman Monteith equation code (ugh)
write_phenofit_data.R -- write out files into phenofit format (info below)
reshape_phenofit_file.R -- read in files, I ended up not using this. 


write_phenofit_data.R sent on 6 April 2023 with this text: 

I guess it depends on the input format.
I do have an R function which creates Phenofit file and "fills" it with data - where data should be a table already in the required format (lat, lon, daily data).
It was made to be used after processing of hourly raster data from ERA5-Land. I don’t know if this is really gonna help you!
I wrote this function at the very beginning of my PhD, might not be the best way to do it. See the file attached.


compute_PET_for_Phenofit.R -- updated function to compute evapotranspiration (Penman Monteith FAO standard method for PET estimates)
Required (in PHENOFIT format): temperatures, wind speed and dewpoint temperature (+altitude)