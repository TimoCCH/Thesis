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
parallel initialize 7, f

*set working directory
global path "C:\Users\Chung\Desktop\EVS\Adj_med\Low"

*create log file
log using "$path\adj_med_low.log", replace

*load the main data
use "$path\adj_med_low.dta", clear 

*set seed
set seed 123

*define the local variables for the estimation
local demographic_vars urban EF29 monthly age edu
local adj_median_prices adj_P1 adj_P2 adj_P3 adj_P4 adj_P5 adj_P6 adj_P7 adj_P8 adj_P9 adj_P10 adj_P11 adj_P12 adj_P13 adj_P14 adj_P15

*estimate the QUAIDSCE model
quaidsce W1-W15, anot(2) prices(`adj_median_prices') expenditure(expfd) demographics(`demographic_vars') nolog reps(50)
parmest, saving(adj_med_low_output, replace)

log close
exit, clear