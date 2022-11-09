// ECONOMETRICS I - PROBLEM SET II
// VICENTE GARCIA AVERELL

cd "C:/Users/vgarciaaverell/Documents/LBS/Econometrics/PS2/"

use PS1.dta, clear

rename (w0 ed0 a0) (wage educ age)
gen lwage = log(wage)
gen exper = age - (educ + 6)

// (a) Create dummy variables

xi: regress lwage i.educ i.exper
predict fitted
rename fitted alphahat


// (b)



// To compute F-statistic we need restricted and unrestricted model

reg lwage educ exper
gen SSR_r=e(rss)
reg lwage i.educ i.exper
gen SSR_u=e(rss)



// F statistic

gen F=((SSR_r-SSR_u)/39)/((SSR_u)/(1500-39))
