# Trend_per_region
Smoothed daily incidence and trend the past 21 days.

R-code for the re-occuring report with name "Trendanalyser per region" found here:
https://www.folkhalsomyndigheten.se/smittskydd-beredskap/utbrott/aktuella-utbrott/covid-19/statistik-och-analyser/analys-och-prognoser/
(28 may 2021)

The report is continuously updated but here we upload a fixed data set (data until 2021-05-26, but in the analysis we use data until 2021-05-23 due to delays).
Hence, this is an example on how the trends for each region is obtained.
Note also, the data shared here could deviate from the one presented at The public health agency of sweden's dashboard on reported cases 
(https://www.folkhalsomyndigheten.se/smittskydd-beredskap/utbrott/aktuella-utbrott/covid-19/statistik-och-analyser/bekraftade-fall-i-sverige/)

The R-code produces, for the regions in Sweden, the two figures 'Figur 1' and 'Figur 2'
 - Figur 1: The daily number of reported cases and smoothed curve of the daily number of cases
 - Figur 2: The daily number of cases and estimated trend the past 21  days.

The code also produces an excel-file 'doubling_time_(2021-05-25)' with the estimated doubling-time/half-life. 
If the number is negative it is a half-life, if positive it is a doubling-time. 
The estimated doubling-time/half-life is only saved if it is significant. 
