## Resubmission
This is a resubmission. 

In this version I have:  

* adapted the return_value of simtimer::sim_wday() to a character, giving the weekday number 
("1" = Monday, "2" = Tuesday, ..., "7" = Sunday). Before this function returned the 
abbreviated weekday (depending on base::Sys.getlocale())
* decoupled simtimer of lubridate by replacing lubridate-calls with base-calls. 


## Test environments
* local OS X 10.11.6 install, R 3.3.0
@ Adrian! Install rhub-package (might be a 5min pain-in-the-ass, but afterwards it might be very useful)


## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

devtools::revdep_check() throws no errors or warnings. 

$revdeps
NULL

$platform
 setting  value                       
 version  R version 3.4.3 (2017-11-30)
 system   x86_64, mingw32             
 ui       RStudio (1.1.383)           
 language (EN)                        
 collate  German_Switzerland.1252     
 tz       Europe/Berlin               
 date     2019-01-22                  

$dependencies
 package        * version date       source                     
 knitr            1.21    2018-12-10 CRAN (R 3.4.4)             
 microbenchmark   1.4-6   2018-10-18 CRAN (R 3.4.4)             
 rmarkdown        1.11    2018-12-08 CRAN (R 3.4.4)             
 simtimer       * 4.0.0   2019-01-22 local (ims-fhs/simtimer@NA)
 testthat         2.0.1   2018-10-13 CRAN (R 3.4.4)             

$results
named list()

---
