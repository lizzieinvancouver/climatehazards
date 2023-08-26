This data package was produced by and downloaded from the National Ecological Observatory Network (NEON). NEON is funded by the National Science Foundation (Awards 0653461, 0752017, 1029808, 1138160, 1246537, 1638695, 1638696, 1724433) and managed cooperatively by Battelle. These data are provided under the terms of the NEON data policy at https://www.neonscience.org/data-policy.



DATA PRODUCT INFORMATION

------------------------



ID: NEON.DOM.SITE.DP1.00003.001



Name: Triple aspirated air temperature



Description: Air temperature, available as one- and thirty-minute averages derived from triplicate 1 Hz temperature observations. Observations are made by sensors located at the top of the tower infrastructure. Temperature observations are made by three platinum resistance thermometers, which are housed together in a fan aspirated shield to reduce radiative biases.



NEON Science Team Supplier: Terrestrial Instrument System



Abstract: Triple aspirated air temperature measurements, available as one- and thirty-minute averages of 1 Hz observations. Temperature is one of the most fundamental physical measurements. It is a primary driving factor for countless physical, chemical, and biological processes. The triple aspirated sensor assembly comprises three individual Platinum Resistance Thermometers (PRTs) housed within an aspirated shield.

Latency:
Data collected in any given month are published during the second full week of the following month.



Brief Design Description: The Triple Aspirated Air Temperature assembly is deployed at core and relocatable tower sites. It is located on the top level of the tower infrastructure.



Brief Study Area Description: These data are collected at all NEON terrestrial sites.



Sensor(s): Thermometrics Climate RTD 100 Ω Probe, housed within a Met One 076B fan aspirated radiation shield



Keywords: tower top, met station, air temperature, meteorology, triple aspirated air temperature (TRAAT), tower, platinum resistance thermometers (PRT)





QUERY INFORMATION

-----------------



Date-Time for Data Publication: 2022-12-10 16:46 (UTC)

Start Date-Time for Queried Data: 2022-03-01 00:00 (UTC)

End Date-Time for Queried Data: 2022-04-01 00:00 (UTC)



Site: SERC

Geographic coordinates (lat/long datum): 
Domain: D02





DATA PACKAGE CONTENTS

---------------------



This folder contains the following documentation files:



- This readme file: NEON.D02.SERC.DP1.00003.001.readme.20230127T120753Z.txt

- Term descriptions, data types, and units: NEON.D02.SERC.DP1.00003.001.variables.20221210T164653Z.csv

- Machine-readable metadata file describing the data package: NEON.D02.SERC.DP1.00003.001.EML.20220301-20220401.20230127T120753Z.xml. This file uses the Ecological Metadata Language schema. Learn more about this specification and tools to parse it at https://www.neonscience.org/about/faq.

- Sensor position information: NEON.D02.SERC.DP1.00003.001.sensor_positions.20221210T164653Z.csv

- Other related documents, such as engineering specifications, field protocols and data processing documentation, are available. Please visit https://data.neonscience.org/data-products/DP1.00003.001 for more information.





This folder also contains 2 data files:

NEON.D02.SERC.DP1.00003.001.000.060.001.TAAT_1min.2022-03.basic.20221210T164653Z.csv - Triple aspirated air temperature averaged over 1 minute
NEON.D02.SERC.DP1.00003.001.000.060.030.TAAT_30min.2022-03.basic.20221210T164653Z.csv - Triple aspirated air temperature averaged over 30 minutes



Basic download package definition: Includes the data product, summary statistics, expanded uncertainty, and final quality flag.



Expanded download package definition: Includes the basic package information plus quality metrics for all of the quality assessment and quality control analyses.





FILE NAMING CONVENTIONS
-----------------------

NEON data files are named using a series of component abbreviations separated by periods. File naming conventions for NEON data files differ between NEON science teams. A file will have the same name whether it is accessed via NEON's data portal or API. Please visit https://www.neonscience.org/data-formats-conventions for a full description of the naming conventions.

ISSUE LOG

----------



This log provides a list of issues that were identified during data collection or processing, prior to publication of this data package. For a more recent log, please visit this data product's detail page at https://data.neonscience.org/data-products/DP1.00003.001.



Issue Date: 2022-09-13
Issue: Severe flooding destroyed several roads into Yellowstone National Park in June 2022, making the YELL and BLDE sites inaccessible to NEON staff. Preventive and corrective maintenance are not being performed, nor is the annual exchange of sensors for calibration and validation. While automated quality control routines are likely to detect and flag most issues, users are advised to review data carefully.
       Date Range: 2022-06-12 to 2022-12-31
       Location(s) Affected: YELL
Resolution Date: 
Resolution: 

Issue Date: 2022-01-10
Issue: Erratic data not caught by autoflagging
       Date Range: 2021-06-30 to 2021-06-30
       Location(s) Affected: OSBS (HOR.VER: 000.060)
Resolution Date: 2022-01-10
Resolution: Manual flagging applied

Issue Date: 2022-01-18
Issue: Data were reprocessed to incorporate minor and/or isolated corrections to quality control thresholds, sensor installation periods, geolocation data, and manual quality flags.
       Date Range: 2013-01-01 to 2021-10-01
       Location(s) Affected: All
Resolution Date: 2022-01-01
Resolution: Reprocessed provisional data are available now. Reprocessed data previously included in RELEASE-2021 will become available when RELEASE-2022 is issued.

Issue Date: 2021-01-11
Issue: Safety measures to protect personnel during the COVID-19 pandemic resulted in reduced or canceled maintenance activities for extended periods at NEON sites. Data availability and/or quality may be negatively impacted during this time.

In addition, the annual refresh of sensors and data acquisition systems (DAS) did not occur according to the typical 1-year schedule for many sites. The annual refresh is where freshly calibrated and verified sensors and DAS replace the units in the field.
       Date Range: 2020-03-23 to 2021-12-31
       Location(s) Affected: ALL
Resolution Date: 2021-12-31
Resolution: NEON reviewed data from all sites and time periods potentially impacted by COVID-19 safety precautions to identify and manually flag suspect data that escaped automated quality tests. Suspect data are indicated by the final quality flag in the data files, which should be used to inform data filtering prior to use. 

Currently, there are no quality flags indicating lack of conformance to an annual sensor refresh interval, but these are in development. Data during this time period should be treated as valid unless marked suspect by other quality flags.

Issue Date: 2020-01-08
Issue: The alpha and beta quality metric multipliers used for computing the final quality flag were swapped in processing. The final quality flag is raised when alphaQM*m + betaQM*n >= 20%. The multiplier m should be set to 2 and n should be set to 1, but these values were swapped, making it slightly harder to raise the final quality flag. These multipliers were corrected in data produced after the resolution date. Data prior to the resolution date will be reprocessed prior to the first NEON data release. An additional changelog comment will be entered at that time.
       Date Range: 2020-01-08 to 2020-01-08
       Location(s) Affected: All
Resolution Date: 2020-06-10
Resolution: 2020-01-08: Processing code updated in forward processing.
2020-06-10: All data reprocessed with corrected algorithm.

Issue Date: 2020-03-26
Issue: Coefficients used in IS data processing to compute the uncertainty contributed by resistance or voltage readings made by the field data acquisition system (FDAS) were updated. The updated coefficients are based on a larger FDAS sample and are larger than previous estimates. Although the difference between previous uncertainty estimates and updated estimates is small, there may be periods for which the FDAS uncertainty represents the largest source of uncertainty. Data produced after the resolution date have been produced with the updated coefficients. Data prior to the resolution date will be reprocessed prior to the first NEON data release. An additional changelog comment will be entered at that time.
       Date Range: 2019-12-11 to 2019-12-11
       Location(s) Affected: All
Resolution Date: 2020-06-10
Resolution: 2019-12-11: Updated coefficients used in processing.
2020-06-10: All data reprocessed with correct coefficients.

Issue Date: 2020-06-10
Issue: All data were reprocessed with the most recent algorithms, quality control thresholds, and/or other metadata to improve overall data coverage and quality. Notes have been added to the logs of previously identified issues that have been corrected.
       Date Range: 2013-01-01 to 2020-06-10
       Location(s) Affected: All
Resolution Date: 2020-06-10
Resolution: Informational log only.

Issue Date: 2020-04-23
Issue: Tower top boom was damaged during an earthquake.
       Date Range: 2020-01-06 to 2020-01-17
       Location(s) Affected: D04 GUAN measurement level 5 (ML5, HOR.VER.000.050)
Resolution Date: 2020-04-23
Resolution: Manual flagging applied

Issue Date: 2019-09-19
Issue: Sensor was streaming while field crews conducted preventative maintenance
       Date Range: 2019-07-30 to 2019-07-30
       Location(s) Affected: D11 OAES ML4 (HOR.VER.000.040)
Resolution Date: 2019-09-19
Resolution: Data manually flagged

Issue Date: 2019-02-19
Issue: Incorrect calibration information was applied to data for some sites and time periods.
       Date Range: 2018-01-01 to 2018-10-31
       Location(s) Affected: All locations.
Resolution Date: 2019-02-19
Resolution: Data reprocessed with correct calibration information.

Issue Date: 2018-05-08
Issue: Sensor not powered down for cleaning.
       Date Range: 2018-04-19 to 2018-04-19
       Location(s) Affected: NIWO (HOR.VER 000.040)
Resolution Date: 2018-05-08
Resolution: Applied manual flagging.





ADDITIONAL INFORMATION

----------------------




NEON DATA POLICY AND CITATION GUIDELINES

----------------------------------------



A citation statement is available in this data product's detail page at https://data.neonscience.org/data-products/DP1.00003.001. Please visit https://www.neonscience.org/data-policy for more information about NEON's data policy and citation guidelines.



DATA QUALITY AND VERSIONING
---------------------------

NEON data are initially published with a status of Provisional, in which updates to data and/or processing algorithms will occur on an as-needed basis, and query reproducibility cannot be guaranteed. Once data are published as part of a Data Release, they are no longer provisional, and are associated with a stable DOI.

To learn more about provisional versus released data, please visit https://www.neonscience.org/data-revisions-releases.