# Remote Sensing Based Estimation of Riverine Phosphorus Using Machine Learing.

This repository contains the source code for the paper titled 'Toward Large-Scale Riverine Phosphorus Estimation Using Remote Sensing and Machine Learning.' The source code was used to develop machine learning (ML) models for estimating riverine phosphorus in U.S. rivers using river reflectance data retrieved from the Landsat archive.

**Overview:** We compare the estimation accuracy of four ML models—XGBlinear, Randomized Random Forest (RRF), XGBtree, and K-Nearest Neighbours—while considering three ML approaches: Base, Cluster, and Sampling. We also assess the estimation accuracy of the ML models on both the validation and training datasets to evaluate the potential for overfitting.

- The `src` folder contains R scripts for the various ML models.
- The `function` folder contains R scripts with user-defined functions utilized in the ML models.
- The `cache` folder includes data files.
- The `data` folder includes the matchup file used for training and validation data.

**To reproduce the results:**
1. Download this repository.
2. Update the file paths at the specified locations:
    - `source("C:\\Research\\Function_Code\\R\\Library.R")`  (available in "function" folder)
    - `matchup_data_clean <- read.csv("C:\\Research\\TP_Part1\\Data_V1\\matchup_data_clean_good.csv")`  (available in "data" folder)
    - `source("C:\\Research\\TP_Part1\\Data_V1\\Features.R")`   (available in "cache" folder)

3. Modify the value of `validation =` in "Define Model Name" for estimating model accuracy on the training and validation datasets:
    - Use `1` for the training dataset and `2` for the validation dataset.
     
The ML results are reproduced in the same folder where the source code exists. Please refer to Model Overview.txt file for a better understanding on the ML model's structure.
  
  If you have any questions or requests for additional data related to this paper, please contact the corresponding author: Pradeep Ramtel (ramtelpp@mail.uc.edu).
