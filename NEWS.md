# simtimer 4.0.0

* Adapt return_value of `simtimer::sim_wday()` to a character, giving the weekday number 
("1" = Monday, "2" = Tuesday, ..., "7" = Sunday). Before this function returned the 
abbreviated weekday (depending on `base::Sys.getlocale()`). The new solution therefore improves platform independency.
* decoupling of lubridate package by replacement with base functions. 


# simtimer 3.0.0

* Added a `NEWS.md` file to track changes to the package.
* Basic functionality as described in readme.md and vignette.

