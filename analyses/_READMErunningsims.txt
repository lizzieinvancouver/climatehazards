Started 22 Juin 2023
By Lizzie

How to run the simulations ...

1) Update the relevant lines in calcclimatatesims.R. This file ...
	- Reads in detrended data created in calcclimatemetrics.R 
	- Created the tmn and tmx simulated data, from those creates tmp.
	- Creates duplicated PRE, ALT, WHC files
	- Creates updated PET files
2) Run phenofit (for three species).

	cd /Applications/Capsis/
	sh capsis.sh -l en

3) I then manually move output to input/phenofit/sims