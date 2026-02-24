**# Set up #1

*clean Stata session
clear all

*close any existing logs
capture log close

*delete current program
program drop _all

*set working directory
global path "C:\Users\Chung\Desktop\EVS"

*create log file
log using "$path\EVS_median.log", replace

*load the main data
use "$path\EVS_median.dta", clear 

*set seed
set seed 123


**# Handle missing price information  #2

*1. create a unique identifier for each region type combination
egen region_type = group(EF2 EF5), label

*2. define the list for all unit prices
local unitprice_list 58 60 61 62 63 64 65 69 75 76 77 80 84 85 86 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 110 112 113 114 121 127 128 129 130 131 132 133 135 136 137 138 139 140 142 144 145 154 155 156 157 158 159 160 161 162 163 164 166 167 168 169 170 171 172 173 174 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 192 193 202 203 210

*3. combined loop to fill missing prices by hierarchical regional medians
quietly {
    foreach a of local unitprice_list {
        * Check if unitprice`a' exists
        capture confirm variable unitprice`a'
        
        * If unitprice`a' exists, proceed with calculations
        if _rc == 0 {
            * Step 1: Calculate the regional median price by region_type (EF2 and EF5)
            bysort region_type: egen med_p1_`a' = median(unitprice`a')
			
            * Step 2: Fill in missing prices with medians by region_type
            by region_type: replace unitprice`a' = med_p1_`a' if unitprice`a' == .
            
            * Step 3: Calculate the median price by EF2 for remaining missing values
            bysort EF2: egen med_p2_`a' = median(unitprice`a')
			
            * Step 4: Fill in missing prices with medians by EF2
            by EF2: replace unitprice`a' = med_p2_`a' if unitprice`a' == .
            
            * Step 5: Calculate the median price by EF1 for remaining missing values
            bysort EF1: egen med_p3_`a' = median(unitprice`a')
			
            * Step 6: Fill in missing prices with medians by EF1
            by EF1: replace unitprice`a' = med_p3_`a' if unitprice`a' == .
            
            * Step 7: Drop temporary median variables
            drop med_p1_`a' med_p2_`a' med_p3_`a'
        }
    }
}

*4. drop the missing price information after cleaning (check if all prices are filled)
foreach a of local unitprice_list {
    capture confirm variable unitprice`a'
    
    * If unitprice`a' exists, drop observations where unitprice`a' is missing
    if _rc == 0 {
        quietly drop if unitprice`a' == .
    }
}

*5. drop the auxiliary variable
drop region_type


**# Quality-adjusted price #3

*1. generate an auxiliary variable 

*1.1 urban
gen urban = 1
replace urban = 0 if EF5 == 3

*1.2 seasons
gen spring = 0
replace spring = 1 if inrange(EF6, 1, 3)

gen summer = 0
replace summer = 1 if inrange(EF6, 4, 6)

gen autumn = 0
replace autumn = 1 if inrange(EF6, 7, 9)

gen winter = 0
replace winter = 1 if inrange(EF6, 10, 12)

*1.3 children
gen children = EF22 + EF23 + EF24 + EF25 + EF26

*1.4 net monthly income
gen monthly = EF30 / 3

*1.5 age
gen age = 2018 - EF8U3

*1.6 education
gen edu = 1
replace edu = 0 if EF8U8 == 0

*1.7 employment status
gen employ = 0
replace employ = 1 if inrange(EF8U9, 1, 9)

*2. define the list for all unit prices
local unitprice_list 58 60 61 62 63 64 65 69 75 76 77 80 84 85 86 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 110 112 113 114 121 127 128 129 130 131 132 133 135 136 137 138 139 140 142 144 145 154 155 156 157 158 159 160 161 162 163 164 166 167 168 169 170 171 172 173 174 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191 192 193 202 203 210

*3. run the hedonic price function
quietly {
	foreach a of local unitprice_list {
		
		* Calculate log unit prices
		gen ln_unitprice`a' = ln(unitprice`a')
		
		* Run the regression for each unit price variable on the quality characteristics
		regress ln_unitprice`a' i.EF2 urban summer autumn winter EF7 children EF29 monthly i.EF8U2 age i.EF8U4 edu employ
		
		* Save residuals for each unit price
		predict e`a', resid
		
		* Store the constant (intercept) from the regression as the quality-adjusted baseline price
		local alpha_i = _b[_cons]
		
		* Generate a quality-adjusted price variable for each unit price
		gen quality_adj_up`a' = `alpha_i' + e`a'
		
		* Exponentiate to get the adjusted price
		replace quality_adj_up`a' = exp(quality_adj_up`a')
		
		* Drop the residual variable
		drop e`a' ln_unitprice`a'
	}
}


**# Calculate category price #4

*1. initialize P1 to P15 to zero
local price_list 1/15
quietly {
	foreach a of numlist `price_list' {
		gen med_P`a' = 0
		gen adj_P`a' = 0
	}
}

*2. define the lists of variables
local fresh_fruits 154 155 156 157 158 159 160 161 162 163 164 165 177
local processed_fruits 166 167 168 169 170 171 172 173 174 175 176 210 213
local fresh_veg 178 179 180 181 182 183 184 185 186 187 188 189 190 191 207 208
local processed_veg 192 193 194 195 196 197 198 199 200 201 228
local fresh_potato 202
local potato_product 203 204 205 206
local fresh_meat 88 89 90 91 92 93 94 95 96 97
local processed_meat 98 99 100 101 102 103 104 105 106 107 108 109 110 111
local fresh_seafood 112 114 115 117
local processed_seafood 113 116 118 119 120 121 122 123 124 125
local eggs 142
local bread 62 63 64 65
local dairy_product 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 144 145
local wheat_product 60 61 66 67 68 69 70 71 72 73 74 75 76 77 78 79 86
local grain_product 58 59 80 84 85 87

*3.  define lists of categories and their target price variables
local category_lists fresh_fruits processed_fruits fresh_veg processed_veg fresh_potato potato_product fresh_meat processed_meat fresh_seafood processed_seafood eggs bread dairy_product wheat_product grain_product

local expenditure_list A_fresh_fruits A_processed_fruits A_fresh_veg A_processed_veg A_fresh_potato A_potato_product A_fresh_meat A_processed_meat A_fresh_seafood A_processed_seafood A_eggs A_bread A_dairy_product A_wheat_product A_grain_product

local median_prices med_P1 med_P2 med_P3 med_P4 med_P5 med_P6 med_P7 med_P8 med_P9 med_P10 med_P11 med_P12 med_P13 med_P14 med_P15

local quality_adj_prices adj_P1 adj_P2 adj_P3 adj_P4 adj_P5 adj_P6 adj_P7 adj_P8 adj_P9 adj_P10 adj_P11 adj_P12 adj_P13 adj_P14 adj_P15

*4. loop through each category to calculate category expenditure
quietly {
	foreach category in `category_lists' {
		* Initialize the summation variable
		gen A_`category' = 0

		* Loop over the variables in the category and sum them
		foreach a of local `category' {
			replace A_`category' = A_`category' + EF`a'U2
		}
	}
}

*5. loop through each category list and calculate expenditure shares
quietly {
	local i = 1
    foreach list of local category_lists {
        local category_exp : word `i' of `expenditure_list'
        
        foreach a of local `list' {
            gen share`a' = .  // Initialize the share variable
            replace share`a' = EF`a'U2 / `category_exp'  // Calculate the share of good `i` by dividing by category expenditure
			
			* Calculate means of each share and store in locals
            summarize share`a', meanonly
            local mean_share`a' = r(mean) // Store mean share in a local macro
        }
        
        local i = `i' + 1 // Increment counter to align with the next category expenditure
    }
}

*6. calculate median of each unit price and store in locals
quietly {
    foreach a of local unitprice_list {
        summarize unitprice`a', detail
        local median`a' = r(p50) // Storing the median price in a local
    }
}

*7. loop through each category list and calculate the category price (normalized to one)
quietly {
    local i = 1
    foreach list of local category_lists {
        local cat_price : word `i' of `median_prices'
        
        foreach a of local `list' {
            * Check if unitprice`a' exists
            capture confirm variable unitprice`a'
            
            * If the variable exists, add its product with share`a' to the target price
            if _rc == 0 {
                * Calculate the aggregated price by the average budget share and log unit price (geometric mean)
                replace `cat_price' = `cat_price' + `mean_share`a'' * ln(unitprice`a' / `median`a'')
            }
        }
        
        local i = `i' + 1
    }
}

*8. exponentiate to get the final aggregated category price
local price_list 1/15
foreach a of numlist `price_list' {
	quietly replace med_P`a' = exp(med_P`a')
}

*9. calculate median of each quality-adjusted unit price and store in locals
quietly {
    foreach a of local unitprice_list {
        summarize quality_adj_up`a', detail
        local adj_median`a' = r(p50) // Storing the median price in a local
    }
}

*10. loop through each category list and calculate the category price (normalized to one)
quietly {
    local i = 1
    foreach list of local category_lists {
        local cat_price : word `i' of `quality_adj_prices'
        
        foreach a of local `list' {
            * Check if quality_adj_up`a' exists
            capture confirm variable quality_adj_up`a'
            
            * If the variable exists, add its product with share`a' to the target price
            if _rc == 0 {
                * Calculate the aggregated price by the average budget share and log unit price (geometric mean)
                replace `cat_price' = `cat_price' + `mean_share`a'' * ln(quality_adj_up`a' / `adj_median`a'')
            }
        }
        
        local i = `i' + 1
    }
} 

*11. exponentiate to get the final aggregated category price
local price_list 1/15
foreach a of numlist `price_list' {
	quietly replace adj_P`a' = exp(adj_P`a')
}

*12. remove outliers

*12.1 create a temporary variable to mark outliers for all variables
gen is_outlier_med = 0
gen is_outlier_adj = 0

*12.2 loop to mark outliers for median prices
local price_list 1/15
quietly { 
	foreach a of numlist `price_list' {
		* Calculate mean and standard deviation
		sum med_P`a'
		local mean = r(mean)
		local sd = r(sd)

		* Mark outliers for the current variable
		replace is_outlier_med = 1 if med_P`a' > (`mean' + 5 * `sd') | med_P`a' < (`mean' - 5 * `sd')
	}
}

*12.3 loop to mark outliers for adjusted prices
local price_list 1/15
quietly {
	foreach a of numlist `price_list' {
		* Calculate mean and standard deviation
		sum adj_P`a'
		local mean = r(mean)
		local sd = r(sd)

		* Mark outliers for the current variable
		replace is_outlier_adj = 1 if adj_P`a' > (`mean' + 5 * `sd') | adj_P`a' < (`mean' - 5 * `sd')
	}
}

*12.4 drop all observations marked as outliers
drop if is_outlier_med == 1 & is_outlier_adj == 1

*12.5 drop the temporary variable
drop is_outlier_med is_outlier_adj


**# Estimate the demand system #5

*1. initialize total food expenditure to zero
gen expfd = 0

*2. calculate the total food expenditure
local expenditure_list A_fresh_fruits A_processed_fruits A_fresh_veg A_processed_veg A_fresh_potato A_potato_product A_fresh_meat A_processed_meat A_fresh_seafood A_processed_seafood A_eggs A_bread A_dairy_product A_wheat_product A_grain_product
foreach a of local expenditure_list {
    quietly replace expfd = expfd + `a'
}

*3. drop any observations with zero expfd
quietly count if expfd == 0
if r(N) > 0 {
    drop if expfd == 0
}
else {
    display "No observations with expfd == 0."
}

*4. loop through each category to generate expenditure shares
local share_var W1 W2 W3 W4 W5 W6 W7 W8 W9 W10 W11 W12 W13 W14 W15
local i = 1
foreach share of local share_var {
    local numerator : word `i' of `expenditure_list'
    quietly gen `share' = `numerator' / expfd
    local i = `i' + 1
}

*5. income level comparison

*5.1 generate per-capita income
gen per_capita = monthly / EF7

*5.2 calculate the 25th and 75th percentiles of per_capita
summarize per_capita, detail
local p25 = r(p25)  // 25th percentile
local p75 = r(p75)  // 75th percentile

*5.3 generate the dummies based on the quartiles
gen low = per_capita < `p25'
gen mid = per_capita >= `p25' & per_capita <= `p75'
gen high = per_capita > `p75'

*6. sample statistics

*6.1 regression variables
sum urban spring summer autumn winter EF7 children EF29 monthly age edu employ
tab EF2
tab EF8U2
tab EF8U4

*6.2 median prices
local price_list 1/15
display "=== Median Prices Summary ==="
foreach a of numlist `price_list' {
    sum med_P`a'
}

*6.3 quality-adjusted prices
local price_list 1/15
display "=== Quality-adjusted Median Prices Summary ==="
foreach a of numlist `price_list' {
    sum adj_P`a'
}

*6.4 expenditure
local expenditure_list A_fresh_fruits A_processed_fruits A_fresh_veg A_processed_veg A_fresh_potato A_potato_product A_fresh_meat A_processed_meat A_fresh_seafood A_processed_seafood A_eggs A_bread A_dairy_product A_wheat_product A_grain_product

display "=== Expenditure Summary ==="
foreach var of local expenditure_list {
    quietly summarize `var', detail
    display "Mean of `var' (all): " r(mean)
    display "SD of `var' (all): " r(sd)
	
    quietly summarize `var' if low==1, detail
    display "Mean of `var' (low): " r(mean)
    display "SD of `var' (low):   " r(sd)

    quietly summarize `var' if mid==1, detail
    display "Mean of `var' (mid): " r(mean)
    display "SD of `var' (mid):   " r(sd)

    quietly summarize `var' if high==1, detail
    display "Mean of `var' (high): " r(mean)
    display "SD of `var' (high):   " r(sd)
}

*6.5 expenditure shares
local share_var W1 W2 W3 W4 W5 W6 W7 W8 W9 W10 W11 W12 W13 W14 W15

display "=== Expenditure Shares Summary ==="
foreach var of local share_var {
    quietly summarize `var', detail
    display "Mean of `var' (all): " r(mean)
    display "SD of `var' (all): " r(sd)

    quietly summarize `var' if low==1, detail
    display "Mean of `var' (low): " r(mean)
    display "SD of `var' (low):   " r(sd)
	
    quietly summarize `var' if mid==1, detail
    display "Mean of `var' (mid): " r(mean)
    display "SD of `var' (mid):   " r(sd)

    quietly summarize `var' if high==1, detail
    display "Mean of `var' (high): " r(mean)
    display "SD of `var' (high):   " r(sd)
}

*6.6 % of consumption
display "=== Consumption Percentage Summary ==="
foreach var of local share_var {
    * Overall consumption percentage
    quietly count if `var' > 0
    local num_consuming = r(N)
    quietly count
    local total_obs = r(N)
    display "Percentage of `var' > 0 (all): " 100 * `num_consuming' / `total_obs'

    * Low-income group
    quietly count if low == 1 & `var' > 0
    local num_consuming_low = r(N)
    quietly count if low == 1
    local total_low = r(N)
    if `total_low' > 0 {
        display "Percentage of `var' > 0 (low): " 100 * `num_consuming_low' / `total_low' " if low==1"
    }
    else {
        display "No observations in low-income group"
    }

    * Mid-income group
    quietly count if mid == 1 & `var' > 0
    local num_consuming_mid = r(N)
    quietly count if mid == 1
    local total_mid = r(N)
    if `total_mid' > 0 {
        display "Percentage of `var' > 0 (mid): " 100 * `num_consuming_mid' / `total_mid' " if mid==1"
    }
    else {
        display "No observations in mid-income group"
    }

    * High-income group
    quietly count if high == 1 & `var' > 0
    local num_consuming_high = r(N)
    quietly count if high == 1
    local total_high = r(N)
    if `total_high' > 0 {
        display "Percentage of `var' > 0 (high): " 100 * `num_consuming_high' / `total_high' " if high==1"
    }
    else {
        display "No observations in high-income group"
    }
}

*6.7 net monthly income
display "=== Net Monthly Income Summary ==="
sum monthly if low == 1 
sum monthly if mid == 1
sum monthly if high == 1


**# Write output #6

*1. median without quality-adjusted

*1.1 all household
preserve
keep EF2 urban spring summer autumn winter EF7 children EF29 monthly EF8U2 age EF8U4 edu employ med_P1 med_P2 med_P3 med_P4 med_P5 med_P6 med_P7 med_P8 med_P9 med_P10 med_P11 med_P12 med_P13 med_P14 med_P15 W1-W15 expfd
save "$path\Median\median_all.dta", replace
restore

*1.2 low-income
preserve
keep if low == 1  // Keep only observations where low == 1
keep EF2 urban spring summer autumn winter EF7 children EF29 monthly EF8U2 age EF8U4 edu employ med_P1 med_P2 med_P3 med_P4 med_P5 med_P6 med_P7 med_P8 med_P9 med_P10 med_P11 med_P12 med_P13 med_P14 med_P15 W1-W15 expfd
save "$path\Median\Low\median_low.dta", replace 
restore

*1.3 middle-income
preserve
keep if mid == 1  // Keep only observations where mid == 1
keep EF2 urban spring summer autumn winter EF7 children EF29 monthly EF8U2 age EF8U4 edu employ med_P1 med_P2 med_P3 med_P4 med_P5 med_P6 med_P7 med_P8 med_P9 med_P10 med_P11 med_P12 med_P13 med_P14 med_P15 W1-W15 expfd
save "$path\Median\Mid\median_mid.dta", replace 
restore

*1.4 high-income
preserve
keep if high == 1  // Keep only observations where high == 1
keep EF2 urban spring summer autumn winter EF7 children EF29 monthly EF8U2 age EF8U4 edu employ med_P1 med_P2 med_P3 med_P4 med_P5 med_P6 med_P7 med_P8 med_P9 med_P10 med_P11 med_P12 med_P13 med_P14 med_P15 W1-W15 expfd
save "$path\Median\High\median_high.dta", replace 
restore

*2. median with quality-adjusted

*2.1 all household
preserve
keep EF2 urban spring summer autumn winter EF7 children EF29 monthly EF8U2 age EF8U4 edu employ adj_P1 adj_P2 adj_P3 adj_P4 adj_P5 adj_P6 adj_P7 adj_P8 adj_P9 adj_P10 adj_P11 adj_P12 adj_P13 adj_P14 adj_P15 W1-W15 expfd
save "$path\Adj_med\adj_med_all.dta", replace
restore

*2.2 low-income
preserve
keep if low == 1  // Keep only observations where low == 1
keep EF2 urban spring summer autumn winter EF7 children EF29 monthly EF8U2 age EF8U4 edu employ adj_P1 adj_P2 adj_P3 adj_P4 adj_P5 adj_P6 adj_P7 adj_P8 adj_P9 adj_P10 adj_P11 adj_P12 adj_P13 adj_P14 adj_P15 W1-W15 expfd
save "$path\Adj_med\Low\adj_med_low.dta", replace 
restore

*2.3 middle-income
preserve
keep if mid == 1  // Keep only observations where mid == 1
keep EF2 urban spring summer autumn winter EF7 children EF29 monthly EF8U2 age EF8U4 edu employ adj_P1 adj_P2 adj_P3 adj_P4 adj_P5 adj_P6 adj_P7 adj_P8 adj_P9 adj_P10 adj_P11 adj_P12 adj_P13 adj_P14 adj_P15 W1-W15 expfd
save "$path\Adj_med\Mid\adj_med_mid.dta", replace 
restore

*2.4 high-income
preserve
keep if high == 1  // Keep only observations where high == 1
keep EF2 urban spring summer autumn winter EF7 children EF29 monthly EF8U2 age EF8U4 edu employ adj_P1 adj_P2 adj_P3 adj_P4 adj_P5 adj_P6 adj_P7 adj_P8 adj_P9 adj_P10 adj_P11 adj_P12 adj_P13 adj_P14 adj_P15 W1-W15 expfd
save "$path\Adj_med\High\adj_med_high.dta", replace 
restore

log close
exit, clear