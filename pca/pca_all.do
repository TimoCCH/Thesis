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
global path "C:\Users\Chung\Desktop\EVS\PCA"

*create log file
log using "$path\pca_all.log", replace

*load the main data
use "$path\pca_all.dta", clear 

*set seed
set seed 123

*define the local variables for the estimation
local demographic_vars urban EF29 monthly age edu
local pca_prices pca_P1 pca_P2 pca_P3 pca_P4 pca_P5 pca_P6 pca_P7 pca_P8 pca_P9 pca_P10 pca_P11 pca_P12 pca_P13 pca_P14 pca_P15

*estimate the QUAIDSCE model
quaidsce W1-W15, anot(2) prices(`pca_prices') expenditure(expfd) demographics(`demographic_vars') nolog reps(50)
parmest, saving(pca_all_output, replace)

log close
exit, clear