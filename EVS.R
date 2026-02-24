rm(list = ls())
cat("\014")

setwd("D:/University/Thesis/Main_thesis/EVS")

#1. load necessary library
library(dplyr)
library(data.table)
library(haven)
library(softImpute)


#2. Data imputation*************************************************************

#2.1 load the data and set seed
EVS <- read_dta("D:/University/Thesis/Main_thesis/EVS/GF4daten.dta")
set.seed(123)

#2.2 conversion

#2.2.1 define the list for conversion
convert_to_kg <- c(58, 60:65, 75:77, 80, 84:86, 88:104, 110, 112:114,  
                   121, 127:133, 135:138, 145:164, 166:174, 177:193,
                   202:203, 209:212, 214, 216:217, 227, 229, 232:235, 
                   250:251, 253:255)
liter_to_kg <- c(139:140, 144, 224, 256:260, 262:288)

#2.2.2 grams to kilograms
for (i in convert_to_kg) {
  # Construct the column name
  column_name <- paste0("EF", i, "U1")
  
  # Check if the column exists in EVS
  if (column_name %in% names(EVS)) {
    # Divide the column by 1000
    EVS[[column_name]] <- EVS[[column_name]] / 1000
  }
}

#2.2.3 liter to kg 
for (i in liter_to_kg) {
  # Construct the column name
  column_name <- paste0("EF", i, "U1")
  
  # Check if the column exists in EVS
  if (column_name %in% names(EVS)) {
    # multiply by 1.035
    EVS[[column_name]] <- EVS[[column_name]] * 1.035
  }
}

#2.3 define the list of indices
consumption_list <- c(58:300)

#2.4 create an empty data frame to store the unit prices
UnitValue <- data.frame(matrix(ncol = length(consumption_list), nrow = nrow(EVS)))
colnames(UnitValue) <- paste0("UnitValue", consumption_list)

#2.5 iterate over each identifier in consumption_list
for (f in consumption_list) {
  # Construct variable names
  var_U1 <- paste0("EF", f, "U1")
  var_U2 <- paste0("EF", f, "U2")
  var_U3 <- paste0("UnitValue", f)
  
  # Check if both EF`a`U1 and EF`a`U2 exist in the data frame
  if (var_U1 %in% names(EVS)) {
    # Calculate the unit price and store it in the UnitValue data frame
    UnitValue[[var_U3]] <- EVS[[var_U2]] / EVS[[var_U1]]
  }
}

#2.6 remove columns that are entirely NA
UnitValue <- UnitValue[, colSums(!is.na(UnitValue)) > 0]

#2.7 scaled PCA imputation
UnitValue_scaled <- cbind(EVS[, c(1:2, 4:6, 8:19, 111:125)], UnitValue)

#2.8 change the data frame to matrix
UnitValue_scaled <- as.matrix(UnitValue_scaled)

#2.9 log-transform the data 
UnitValue_scaled[, 33:185] <- log(UnitValue_scaled[, 33:185])

#2.10 calculate mean and standard deviation for each price column
UP_means <- colMeans(UnitValue_scaled, na.rm = TRUE)
UP_sds <- apply(UnitValue_scaled, 2, sd, na.rm = TRUE)

#2.11 standardize the price matrix (subtract the mean and divide by sd)
UnitValue_scaled <- scale(UnitValue_scaled, center = UP_means, scale = UP_sds)

#2.12 price imputation

#2.12.1 check lambda
lam_scaled <- lambda0(UnitValue_scaled)
lam_scaled

#2.12.2 create a lambda sequence
lamseq_scaled <- exp(seq(from=log(lam_scaled), to=log(1), length=100))
lamseq_scaled

#2.12.3 apply softImpute to the data with warm start
fits_scaled <- as.list(lamseq_scaled)
ranks_scaled <- as.integer(lamseq_scaled)
rank_max_scaled <- 10
warm_scaled <- NULL
for (i in seq(along = lamseq_scaled)) {
  fiti_scaled <- softImpute(UnitValue_scaled, lambda = lamseq_scaled[i], type = c("svd"), rank = rank_max_scaled, warm = warm_scaled, maxit = 500)
  ranks_scaled[i] <- sum(round(fiti_scaled$d, 4) > 0) # number of positive sing.values
  rank_max_scaled <- min(ranks_scaled[i]+10, min(dim(UnitValue_scaled))-1)
  warm_scaled <- fiti_scaled # warm start for next
  fits_scaled[[i]] <- fiti_scaled
  cat(sprintf("%2d lambda=%9.5g, rank_max_scaled = %d  ==> rank = %d\n",
              i, lamseq_scaled[i], rank_max_scaled, ranks_scaled[i]))
}

#2.13 complete the matrix
UnitValue_scaled_completed <- complete(UnitValue_scaled, fiti_scaled)

#2.14 rescale the matrix back to the original price scale
UnitValue_scaled_completed <- sweep(UnitValue_scaled_completed, 2, UP_sds, FUN = "*")
UnitValue_scaled_completed <- sweep(UnitValue_scaled_completed, 2, UP_means, FUN = "+")

#2.15 exponentiate the data matrix
UnitValue_scaled_completed[, 33:185] <- exp(UnitValue_scaled_completed[, 33:185])

#2.16 change the matrix to data frame
UnitValue_PCA <- as.data.frame(UnitValue_scaled_completed)

#2.17 rename columns starting with "UnitValue" to start with "pca_UnitValue" instead
names(UnitValue_PCA) <- sub("^UnitValue", "pca_UnitValue", names(UnitValue_PCA))

#2.18 join the completed matrix with original data frame
EVS_median <- cbind(EVS, UnitValue)
EVS_PCA <- cbind(EVS, UnitValue_PCA[, 33:185])


#3. Data aggregation************************************************************

#3.1 define the lists
demo_list1 <- c(1:2, 5:7, 16:30)
demo_list2 <- c(2:13)
food_list <- c(58:300)

#3.2 initialize an empty vector to store column names
columns_to_keep <- NULL

#3.3 loop through demo_list1 to keep the necessary demographic columns
for (d in demo_list1) {
  column_demo1 <- paste0("EF", d)

  if (column_demo1 %in% colnames(EVS)) {
    columns_to_keep <- c(columns_to_keep, column_demo1)
  }
}

#3.4 loop through demo_list2 to keep the necessary demographic columns
for (d in demo_list2) {
  column_demo2 <- paste0("EF8U", d)

  if (column_demo2 %in% colnames(EVS)) {
    columns_to_keep <- c(columns_to_keep, column_demo2)
  }
}

#3.5 loop through food_list to keep the necessary consumption and price columns
for (f in food_list) {
  column_U1 <- paste0("EF", f, "U1")
  column_U2 <- paste0("EF", f, "U2")

  if (column_U1 %in% colnames(EVS)) {
    columns_to_keep <- c(columns_to_keep, column_U1)
  }

  if (column_U2 %in% colnames(EVS)) {
    columns_to_keep <- c(columns_to_keep, column_U2)
  }
}

#3.6 create an extra list to keep imputed prices
columns_to_keep_PCA <- columns_to_keep

#3.7 loop through food_list to keep the necessary price columns for subset_agg_median
for (f in food_list) {
  column_price <- paste0("UnitValue", f)

  if (column_price %in% colnames(EVS_median)) {
    columns_to_keep<- c(columns_to_keep, column_price)
  }
}

#3.8 loop through food_list to keep the necessary price columns for subset_agg_PCA
for (f in food_list) {
  column_price_PCA <- paste0("pca_UnitValue", f)

  if (column_price_PCA %in% colnames(EVS_PCA)) {
    columns_to_keep_PCA <- c(columns_to_keep_PCA, column_price_PCA)
  }
}

#3.9 create a subset data
EVS_median <- EVS_median[, columns_to_keep, drop = FALSE]

#3.10 save the subset to a Stata file
write_dta(EVS_median, "EVS_median.dta")

#3.11 create a subset data (PCA imputation)
EVS_PCA <- EVS_PCA[, columns_to_keep_PCA, drop = FALSE]

#3.12 save the subset to a Stata file (PCA imputation)
write_dta(EVS_PCA, "EVS_PCA.dta")
