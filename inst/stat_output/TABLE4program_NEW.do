//log using "\\file\UsersW$\wrr15\Home\My Documents\My Files\D'HAULTFOEUILLE Files\DATAVERSE FILES\TABLE4.smcl", replace

set matsize 10000

// MAT: 
ssc install twowayfeweights, replace
// use "\\file\UsersW$\wrr15\Home\My Documents\My Files\D'HAULTFOEUILLE Files\DATAVERSE FILES\GentzkowData.dta", clear
use "/home/stigler/stata/GentzkowData.dta", clear

// simple way, year effects
twowayfeweights prestout cnty90 year numdailies , type(feTR) breps(2) test_random_weights(year) brepscluster(cnty90) path("/home/stigler/stata/outputStata_2way_feTR_simple.dta")

Under the common trends assumption, beta estimates a weighted sum of 10378 ATTs.
>  
6180 ATTs receive a positive weight, and 4198 receive a negative weight.
The sum of the negative weights is equal to -.47401318.
beta is compatible with a DGP where the average of those ATTs is equal to 0,
while their standard deviation is equal to .00095803.
beta is compatible with a DGP where those ATTs all are of a different sign than 
> beta,
while their standard deviation is equal to .00194149.


// simple waystate-, year effects
twowayfeweights prestout cnty90 styr numdailies , type(feTR) breps(2) test_random_weights(year) brepscluster(cnty90) path("/home/stigler/stata/outputStata_2way_feTR_styr.dta")


Under the common trends assumption, beta estimates a weighted sum of 10375 ATTs. 
6219 ATTs receive a positive weight, and 4156 receive a negative weight.
The sum of the negative weights is equal to -.53306156.
beta is compatible with a DGP where the average of those ATTs is equal to 0,
while their standard deviation is equal to .00036553.
beta is compatible with a DGP where those ATTs all are of a different sign than beta,
while their standard deviation is equal to .00072946.

twowayfeweights prestout cnty90 year numdailies , type(feTR) controls(styr1-styr666) breps(2) test_random_weights(year) brepscluster(cnty90) path("/home/stigler/stata/outputStata_2way_feTR_controls.dta")

Under the common trends assumption, beta estimates a weighted sum of 10373 ATTs. 
6212 ATTs receive a positive weight, and 4161 receive a negative weight.
The sum of the negative weights is equal to -.53280479.
beta is compatible with a DGP where the average of those ATTs is equal to 0,
while their standard deviation is equal to .0003413.
beta is compatible with a DGP where those ATTs all are of a different sign than beta,
while their standard deviation is equal to .00068118.


// Computing weights attached to FD regression
set seed 1
twowayfeweights changeprestout cnty90 year changedailies numdailies, type(fdTR) controls(styr1-styr666) test_random_weights(year) breps(100) brepscluster(cnty90)

// Computing weights attached to FE regression
set seed 1
twowayfeweights prestout cnty90 year numdailies , type(feTR) controls(styr1-styr666) breps(100) test_random_weights(year) brepscluster(cnty90)

log close
