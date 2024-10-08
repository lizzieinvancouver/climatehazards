Taken from _dothis.txt in Dec 2023
By Lizzie

<><><><><><><><><><><><><><>
<> How to detrend: START <>
<><><><><><><><><><><><><><>

22 March 2023 
From Ben

Yeah, these are the right approaches I think. To estimate st. dev. And the mean for the scaling, I would pool all days from each month for all years. For example, for January, that would be 31 days X 20 years = 620 days, and then calculate st. dev. and mean from this pooled distribution. 

One suggestion to simplify things, since this is meant to be a constrained demonstration of the fitness ideas using PhenoFit: before doing anything, detrend the data to remove any warming trend. How I would do it:

(1) For each month, calculate monthly average TMEAN. You should then have a time series of monthly average tmean for 1970-2000 (this is basically what you have in the mean_toben.pdf, I think).

(2) Subtract these monthly tmean values from the daily values for the associated month. For example, subtract monthly tmean for January 1970 from the daily values for January 1970, monthly Jan 1971 from daily Jan 1971, etc. You will then be left with daily tmean ANOMALIES.

(3) For each monthly time series, run a linear regression of the values against year (time). Retain the residuals, which will be the monthly temperature anomalies with the trend removed.

(4) For each monthly residual time series, add on the long term mean monthly temperature. For example, for January time series of monthly detrended residuals, add on the mean monthly temperature calculated for 1970-2000.

(5) You should now now have a time series for each month of average TMEAN, with any trend removed by the variability retained and everything centered on the long term average.

(6) You can then take these monthly average time series and add it back on to the anomalies you calculated in step (2) above to now recover daily tmean data to use for the calculations, but just with the long term trend removed. You could then use all the data to estimate mean and st. dev.

Sorry if this is confusing (or mansplainy)! Happy to schedule some time to talk through if you like.


<><><><><><><><><>
SENT to BEN on 3 April 2023
"I want to make sure I am doing the detrending correctly, as all my checks make me sure that I am not. Can you take a look at my updated methods (my notes in []) below?"
He confirmed I had this correct 

(1) For each month, calculate monthly average TMEAN. You should then have a time series of monthly average tmean for 1970-2000 (this is basically what you have in the mean_toben.pdf, I think). 

[Okay.]

(2) Subtract these monthly tmean values from the daily values for the associated month. For example, subtract monthly tmean for January 1970 from the daily values for January 1970, monthly Jan 1971 from daily Jan 1971, etc. You will then be left with daily tmean ANOMALIES. 

[When done, I set these `daily tmean ANOMALIES' aside until step 6.]

(3) For each monthly time series [12 datapoints per year], run a linear regression of the values against year (time) [run 12 separate regressions, one for each month]. Retain the residuals, which will be the monthly temperature anomalies with the trend removed.

[I am NOT running this regression on the daily anomalies from (2) -- I tried that see plotforBenTake1.pdf. Instead, I run this on the monthly means by year, see ploforBenTake2.[df]

(4) For each monthly residual time series, add on the long term mean monthly temperature. For example, for January time series of monthly detrended residuals, add on the mean monthly temperature calculated for 1970-2000.

[I take residuals in (3) and add on long-term mean for that month (one value across the whole time series.]

(5) You should now now have a time series for each month of average TMEAN, with any trend removed by the variability retained and everything centered on the long term average.

[Okay.]

(6) You can then take these monthly average time series and add it back on to the anomalies you calculated in step (2) above to now recover daily tmean data to use for the calculations, but just with the long term trend removed. You could then use all the data to estimate mean and st. dev.

[I take output of (4) and add it to (2), right?. Right now that seems to recreate the original data... but I will check my code if you confirm I have this right.]

<><><><><><><><><><><><><><>
<> How to detrend: END <>
<><><><><><><><><><><><><><>