# CoMix

This repository contains the functions and analysis used to reproduce the CoMix survey results. The data is not currently available but dummy data will be added so that the code can be seen and checked. 

```
# install.packages("remotes")
remotes::install_github("jarvisc1/CoMix")
```


# Run Analysis

The `inst/scripts` folder contains code to run the social mixing matrices analysis for CoMix using POLYMOD as a baseline for imputation and comparison and imputation. 

1. Choose from the following: 
    * Option 1: Fork the CoMix package repository
    * Option 2: Copy the `inst/` folder to another repository
        * For either option, set your `.gitignore` preferences before pushing any code
    or date to github (see the 
    [gitignore documentation](https://git-scm.com/docs/gitignore) for details)

2. Create panel and wave specific folders in data and outputs using the following
structure (refer to `inst/scripts/000_run_all.R`)
    * each panel and wave will need the following folders:
        * `inst/data/panel_[LETTER]/wave_[NUMBER]/data` 
        * `inst/outputs/panel_[LETTER]/wave_[NUMBER]/contact_matrices`
        * `inst/outputs/panel_[LETTER]/wave_[NUMBER]/contact_matrices_online`
        * `inst/outputs/combined`
3. Add the following data:
    * clean data for participants and contacts from the CoMix survey to the panel 
    and wave specific data folder created in the previous step 
    (`clean_participants.rds` and `clean_contacts.rds` from the CoMix survey)
    * polymod participant and contacts data (named `polymod_contacts.rds`, 
    `polymod_participants.rds`, and `polymod_contacts_part.rds`)
4. Run the `inst/scripts/000_run_all.R` file. By default it will bootstrap 5000 
samples, which can be changed in `aaa_create_contact_matrices`


# Outputs




# Contributing
