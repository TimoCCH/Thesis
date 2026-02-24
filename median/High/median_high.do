**# Set up #1

*clean Stata session
clear all

*close any existing logs
capture log close

*delete current program
program drop _all

*index the libraries of Mata functions
mata mata mlib index

*set parallel cluster
parallel initialize 15, f

*set working directory
global path "C:\Users\Chung\Desktop\EVS\Median\High"

*create log file
log using "$path\median_high.log", replace

*load the main data
use "$path\median_high.dta", clear 

*set seed
set seed 123

*define the local variables for the estimation
local demographic_vars urban EF29 monthly age edu
local median_prices med_P1 med_P2 med_P3 med_P4 med_P5 med_P6 med_P7 med_P8 med_P9 med_P10 med_P11 med_P12 med_P13 med_P14 med_P15

*estimate the QUAIDSCE model
quaidsce W1-W15, anot(2) prices(`median_prices') expenditure(expfd) demographics(`demographic_vars') nolog reps(50)
parmest, saving(median_high_output, replace)

log close
exit, clear