# The Emergence of a New Pilgrimage as an Assurance Game

This repository contains all data and code necessary to reproduce all analyses for the paper "The Emergence of a New Pilgrimage as an Assurance Game". 

The `scripts` folder contains a file called `bayesian_stag_hunt.R`, which is the agent-based model we use for the main analysis. The file named `parameter_exploration.R` runs the model for the different parameter combinations and saves the result as `full_bayesian_parameter_exploration.rds` in the `data` folder. This is the file used to produce most of the figures on the `manuscript.Rmd` file, which compiles the paper. 

The `supplementary_materials.Rmd` file creates that document, using a csv file called `clean_pucara_regional.csv`, that can be found in the `data` folder. 


