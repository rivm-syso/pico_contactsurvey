################################################################################
#
# Main script for data cleaning of pico and pienter3 contacts
# Cleaned data available from https://zenodo.org/record/10370353
# 
################################################################################

source("./scripts/data/01_Load_packages.R")
source("./scripts/data/02_Load_raw_data.R")
source("./scripts/data/03_Include_pico_data.R")
source("./scripts/data/04_Include_pienter3_data.R")
source("./scripts/data/05_Combine_data.R")
source("./scripts/data/06_Clean_participants.R")
source("./scripts/data/07_Construct_contacts.R")
source("./scripts/data/08_Get_population_data.R")
source("./scripts/data/09_Get_education_data.R")
source("./scripts/data/10_Save_data.R")

