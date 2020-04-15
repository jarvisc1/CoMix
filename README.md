# CoMix

This repository contains the functions and analysis used to reproduce the CoMix survey results. The data is not currently available but dummy data will be added so that the code can be seen and checked. 

```
# install.packages("remotes")
remotes::install_github("jarvisc1/CoMix")
```


# Running code

The `inst/scripts` folder contains code to run the social mixing matrices analysis for CoMix using POLYMOD as a baseline for imputation and comparison and imputation. 

## Set panel and wave variables

To run the analysis:

1. Create panel and wave specific folders in data and outputs using the exisitng structure (refer to `inst/scripts/000_run_all.R`, will add a map below)
2. Add clean data for participants and contacts from the CoMix survey to the data folder
3. Update variable names to match folder paths and age limits in the file `inst/scripts/000_run_all.R` and run the file. Outputs will appear as mapped in the path settings of the file.
