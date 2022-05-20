/***************
Preliminaries
***************/
clear all
capture log close
set more off

* Enter the location
global location = "/Users/jonathanlatner/OneDrive/SECCOPA/projects/wage_mobility"

* Enter the location of the do files
global do_files = `"$location/do_files/"'

* Enter the location of the data files
global data_files = `"$location/data_files/"'

* Enter the location of the data files
global support_files = `"$location/support_files/"'

* Change directory
cd "$location"

/***************
Data files
***************/

use "$data_files/germany.dta", clear

/***************
Model
***************/

xtset pid year

xtreg ln_hourly_wage unmp i.temp##c.age##i.age_cat i.year, fe
