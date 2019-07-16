# TESSlc
##Description of files
###S7/ and S9/
These directories store some of the outputs from code/calPeriodogram.R
There are 2746 periodograms for sector 7 generated by code/calPeriodogram.R
There are 1500 periodograms for sector 9 generated by code/calPeriodogram.R
I select these targets from a larger sample of nearby stars

###code/
The R routines are available in this directory. I conduct the analysis following the following steps.
>1. I first download fits files from https://archive.stsci.edu/tess/bulk_downloads/bulk_downloads_ffi-tp-lc-dv.html
>2. I download section 7 and 9 using tesscurl_sector_7_lc.sh and tesscurl_sector_9_lc.sh
>3. Then I use code/find_radec.R to read PDCSAP_FLUX and PDCSAP_FLUX_ERR from fits and save as ascii files .dat (this is not put in github due to large size). (related codes: read_from_header.R and code/find_radec.R)
>4. I crossmatch these targets with Simbad to find stellar types, parallax and etc. (related codes: code/match_tess_sim.R)
>5. I select the targets with parallax larger than 10 (or distance less than 100 pc) from these targets and get a list of files to be read by code/calPeriodogram.R. The file names are s7file.txt and s9file.txt (related codes: code/select_files.R)
>6. Using code/calPeriodogram.R, I calculate the periodograms for the data listed in s7file.txt and s9file.txt 
The periodogram that I used is Generalized Lomb-Scargle periodogram with floating trend (GLST), it is developed as part of the Agatha software and application (https://academic.oup.com/mnras/article/470/4/4794/3828792), the application is available at https://github.com/phillippro/agatha and https://www.shinyapps.io/admin/#/application/150471
>7. In step 6, I have calculated GLST for the raw data and for the raw data subtracted by subsequent signals. For every target or data file, I identify 3 sinusoidal signals in total and then fit the combined solution to the data. The first column in the plot gives the RMS of the data, the second column shows the fit, the third column shows the phase curve and the fourth column shows the corresponding GLSTs. The last panel in the bottom shows the combined fit of three signals. The dotted and dashed lines in the fourth columns show the thresholds of ln(BF)=0 and 5. I only select signals pass ln(BF)=5 as statistically significant signals for further analysis. Note that the TESS light curves are binned with a bin size of 0.01 day or 0.24 hour. This binning is for the purpose of efficiency and would not influence signals with periods of a few days. (Related codes: code/get_period.R)
>8. Finally, I combine the information from Simbad and the signficant signals found by code/calPeriodogram.R to make file s7final.csv and s9final.csv (related codes: code/combine_allinfo.R)
>9. For the targets in s7final.csv and s9final.csv, I only select targets with signals passing the ln(BF)=5 threshold. For each of these targets, I only select the most significant signal as the characteristic signal. Then I plot the distribution of periods as a function of stellar type, distance and colors. (related codes: code/plot_period_Stype.R)
The other R toutines, code/mcmc_func.R, code/periodoframe.R, and code/periodograms.R, provide auxiliary functions. 





j