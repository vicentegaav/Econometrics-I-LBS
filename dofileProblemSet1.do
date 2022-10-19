//// ECONOMETRICS I - PROBLEM SET 1 - QUESTION 5 
cd "C:/Users/vgarciaaverell/Documents/LBS/Econometrics/PS1/"
use "PS1.dta", replace

gen exp = a0 - (ed0 +6)
gen logwage = log(w0)
gen expsquare = exp^2

// (a)
reg logwage ed0 exp expsquare
outreg2 using results_a.tex

// (b)
reg logwage ed0 exp expsquare
predict fittedw0

reg logwage ed0 exp fittedw0
outreg2 using results_b.tex

// (c)
reg exp ed0 expsquare
predict residexp, residuals

reg logwage ed0 expsquare
predict residlogwage, residuals

reg residlogwage residexp
outreg2 using results_c.tex

// (d)

reg logwage residexp
outreg2 using results_d.tex
