# Data Processing and Repository Structure

1. The raw EVS data are processed in EVS.R. One branch performs unit harmonisation, log-transformation, standardisation, and price imputation using the softImpute (PCA-based) method, followed by rescaling and exponentiation back to price levels. The other branch performs only unit harmonisation. This results in two datasets: EVS_median (no PCA imputation) and EVS_PCA (PCA-imputed prices).

2. The Stata scripts EVS_median.do and EVS_PCA.do aggregate detailed food items into 15 food groups, compute aggregate prices, generate summary statistics, and create subsamples by income group and by quality-adjustment status.

3. The median and pca folders contain results based on regional median prices and PCA-imputed prices, respectively. The adj_med and adj_pca folders contain the corresponding quality-adjusted versions.

4. Each folder is further divided into all households, low-income, middle-income, and high-income subsamples.

5. Due to confidentiality agreements with the German statistical authorities, the raw data cannot be shared in this repository.
