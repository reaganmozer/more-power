## About 
This repository contains the materials needed to replicate the analyses presented in **"More power to you: Using machine learning to augment human coding for more efficient inference in text-based randomized trials"** by Mozer and Miratrix (2024), forthcoming in the *Annals of Applied Statistics*. 

## Replication Data
The two data sets used in this study are publicly available and can be downloaded by following the instructions below.

- **Simulation study (Section 4)**: The data used for the simulation study presented in Section 4 of the paper are available on Github. Go to (https://github.com/scrosseye/persuade_corpus_2.0) and follow the links to download.

- **Application (Section 5):** The data used for the application described in Section 5 of the paper are available through the Harvard Dataverse. Go to  (https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/J9KSHU), then click the "Access Dataset" button and complete the data use agreement to download the relevant files.


## System Requirements
Replication scripts require the following packages to be installed from Github:

```{r}
## quanteda add-ons
devtools::install_github("quanteda/quanteda.sentiment")
devtools::install_github("kbenoit/quanteda.dictionaries")

## rcttext development package
devtools::install_github("reaganmozer/rcttext")

```	

## Instructions
To replicate the analyses and results presented in the paper, run the R scripts in the `scripts` folder in the order indicated below. 

### Initial setup \& configurations
- `00_setup.R`: Initial setup for the project and loading required libraries

### Simulation study using PERSUADE corpus
- `sim_functions.R`: Utility functions for model training and impact analysis using the model-assisted approach
- `01_get_text_features.R`: Processes the PERSUADE data and generates text features used to train the ML models
- `02_run_sims.R`: Runs the simulations described in Section 4 of the paper (we ran this script on a high performance computing cluster)
- `03_sim_results.R`: Aggregate simulation results and generate plots for performance of point and variance estimators

### Application to the MORE study
- `04_prep_pilot_data.R`: Processes the pilot data and generates text features
- `05_eval_pilot.R`: Fits several ML models to the pilot data and performs power calculations for the main study
- `06_prep_MORE_data.R`: Processes data from the main study and generates text features used for model training
- `07_estimate_impacts.R`: Fits several ML models to the main study and calculates impact estimates and uncertainty intervals
- `08_impacts_cov_adj.R`: Calculates impact estimates for the main study data via regression, adjusting for baseline differences in pre-test scores


### Supplement
All scripts to reproduce the results shown in the supplement are provided in the "scripts-supplement folder". Generated data and simulation results for the supplemental analyses are stored in the "results-supplement" folder.

## Notes
- Raw data for the simulation study and application (pilot study and main RCT data) are stored in the `raw data` folder.
- The `external data` folder is used to hold files generated for processing in external programs (e.g., LIWC, TAACO, and using the OpenAI API).
- Scripts 01-06 generate intermediate data files that are stored in the `generated data` folder (created during initial configuration)

