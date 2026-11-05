# MASTER SCRIPT --------------------------

# 1. Load in data from S3 Bucket 

#Change source depending on year and whether it is end- or mid-year
source("DownloadPCC1.R")

# 2. Run functions and processing script 
source("PCC2526 Processing.R")

# 3. Run QA script which checks for missing data etc.
source("PCC QA.R")

# 4. Run outputs scripts
source("PCC Outputs.R")
