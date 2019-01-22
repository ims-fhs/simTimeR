## Resubmission
This is a resubmission. 

In this version I have:  

* adapted the return_value of `simtimer::sim_wday()` to a character, giving the weekday number 
("1" = Monday, "2" = Tuesday, ..., "7" = Sunday). Before this function returned the 
abbreviated weekday (depending on `base::Sys.getlocale()`). The new solution therefore improves platform independency.
* decoupled simtimer of lubridate by replacing calls to lubridate functions with calls to base R functions. 


## Test environments
* local Windows 10 install, R 3.4.3
* local OS X 10.12 install, R 3.3.0


## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

devtools::revdep_check() throws no errors or warnings.
