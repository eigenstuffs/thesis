Replication file for: How climate policy commitments influence energy systems and the economy of US States
Authors: Parrish Bergquist, Chris Warshaw

The files in this replication file run the analyses and reproduce the figures presented in the main text and the supplementary information. 


In order for the paths to work properly within each script, users should download all files as a single zip file, choosing the option for "original format." This will preserve the folder structure of the archive. The file set includes the folders: 
* codebooks contains descriptions and sources of the variables in our analysis
* outcome_data contains the primary data for our dependent variable measures
* policy_data contains the inputs to our climate policy index
* policy_index_outputs contains the outputs from the model that estimates our climate policy index
* population contains population data from the US census
* robustness_checks contains the outputs of a series of climate policy indices, each of which is estimated without one of the policies in the index presented in the text
* validation_data contains the data that we compare our estimates with, in our validations presented in the SI 

To save figures and tables, users should create a figures/ and tables/ folder within the replication folder. 

The replication folder contains four scripts, which should be run in order: 
* 0climate_policies_model.r estimates the climate policy index. 
* 1climatepolicy_dataprep.R compiles and cleans data for analysis
* 2climatepolicy_descriptivefigures.R produces all descriptive results and validations presented in the paper and SI 
* 3climatepolicy_analysis.R runs all regressions presented in the paper and SI 
