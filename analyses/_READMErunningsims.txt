Started 22 Juin 2023
By Lizzie

How to run the simulations ... (run calcclimatemetrics.R first!)

1) Update the relevant lines in calcclimatatesims.R. This file ...
	- Reads in detrended data created in calcclimatemetrics.R (update 10 June 2024: Lizzie included all files needed to run this, she thinks)
	- Created the tmn and tmx simulated data, from those creates tmp.
	- Creates duplicated PRE, ALT, WHC files
	- Creates updated PET files
2) Run phenofit (for three species).

	cd /Applications/Capsis/
	sh capsis.sh -l en

Set the years to 1950-2000 (and remember that you just select the folder in the phenofit GUI, you do not click into it). 

3) I then manually move output to input/phenofit/sims