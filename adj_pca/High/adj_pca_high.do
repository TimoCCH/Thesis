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
parallel initialize 6, f

*set working directory
global path "/home/timo/Documents/Adj_pca/High"

*create log file
log using "$path/adj_pca_high.log", replace

*load the main data
use "$path/adj_pca_high.dta", clear 

*set seed
set seed 123

*define the local variables for the estimation
local demographic_vars urban EF29 monthly age edu
local adj_pca_prices adj_pca_P1 adj_pca_P2 adj_pca_P3 adj_pca_P4 adj_pca_P5 adj_pca_P6 adj_pca_P7 adj_pca_P8 adj_pca_P9 adj_pca_P10 adj_pca_P11 adj_pca_P12 adj_pca_P13 adj_pca_P14 adj_pca_P15

*estimate the QUAIDSCE model
quaidsce W1-W15, anot(2) prices(`adj_pca_prices') expenditure(expfd) demographics(`demographic_vars') nolog reps(50)
parmest, saving(adj_pca_high_output, replace)

log close
exit, clear
