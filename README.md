# COST-FAST-MMPTs
Code relating to COST publication.

## Libraries
```
packages <- c("tidyverse", "haven", "lubridate",         # Data Management
				"patchwork", "GGally", "gplots",                 # plotting
				"Rcpp", "RccpArmadillo", "RccpDist",             # Rcpp tools
				"MASS", "mvtnorm", "truncnorm",                  # Gaussian functions
				"MCMCpack",                                      # Wishart Distribution
				"Bolstad2",                                      # Numerical Calculus
				"FNN", "gbm","mgcv","nprobust","bbemkr"          # Competitor Models
				"np", "quantreg", "e1071", "nnet",               # Competitor Models
				"bartMachine", "caret", "rpart",                 # Competitor Models
				"randomForest", "mclust", "HDclassif")           # Competitor Models
  
install.packages(setdiff(packages, rownames(installed.packages())))  
```

## Code Organization
Each folder pertains to a particular figure/table/section from the manuscript.

Within each folder are subfolders specifying which table/figure/table/section from the manuscript. In those subfolders, you will find a run.R file. This run.R file creates the output for the manuscript using other files (data/helper functions/etc.) in the folder.

Below, I outline the contents of this zip file. Each run file is designed to work in its directory. So the best way to use these files is to extract the full zip file to retain the directory structure and ensure you run the files in their working directory.

## 00-Bins
* This is the code for Figure 1 in the manuscript showing the notation for Polya trees. 
* Runtime: Less than 1 minute.

## 01-DensityEstimation 
* This is the code for Figures 2-3 in the manuscript (dogbowl/Gaussian estimates). 
* Runtime: Figures 2/3 take roughly 1 day of computation time.
* Supplement: 3 Run files (2+ days computation time).

## 02-RegressionViz
* This is the code for Figures 4 and 6 in the manuscript (regression/density estimates).
* Runtime: Figure 4 takes 1-2 days of computation time, Figure 6 takes <1 day.

## 03-Regressioneval 
* This is the code for the first three columns in Table 1 (bivariate simulation results)
* Note: There are ten run files per folder. Each runs a fold of the tenfold cross validation.
* Writing Data: writedata.R simulates the data and creates/saves the folds.
* Compile Results: compileresults.R will combine the cross validation results.
* Runtime: Running simultaneously these took 1-2 days to complete.

## 04-Hemo
* Note: There are ten run files. Each runs a fold of the tenfold cross validation.
* Writing Data: writedata.R simulates the data and creates/saves the folds
* Compile Results: compileresults.R will combine the cross validation results
* Runtime: Running simultaneously these took 1-2 days to complete

## 05-MFAP4
* Note: There are ten run files. Each runs a fold of the tenfold cross validation.
* Writing Data: writedata.R simulates the data and creates/saves the folds
* Compile Results: compileresults.R will combine the cross validation results
* Runtime: Running simultaneously these took <1 day to complete

## 06-Benchmarks
* Note: Each subdirectory contains a benchmark dataset and the code for conducting a 1/3 holdout analysis. 
* Runtime: Simulations range from a few hours to a few weeks to complete

## 07-exploring-priors (For supplement)
* alpha1 provides the estimates when alpha is drawn from gamma(hat(alpha, 1)) versus alpha is drawn from gamma(1, 1/hat(alpha))
* fixedalpha provides the estimates when alpha is fixed and when the shape parameter is varied.
* fastVfast provides multiple estimates of the same data for evaluating reliability of the FAST sampler
* mcmcVmcmc provides multiple estimates of the same data for evaluating reliability of the full sampler

## 08-exploring-props (For supplement)
* Code for conducting the benchmark analyses with different rates of random acceptance. 
