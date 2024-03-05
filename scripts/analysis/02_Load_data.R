################################################################################
#
# Load data
# 
# Load csv files in socialcontactdata.org format from Zenodo
# (produced in data pipeline ./scripts/data)
# Combine participants and contacts
#
################################################################################


prefix <- "https://zenodo.org/record/10370353/files/PienterCorona_NL_"

participants <- read_csv(file = paste0(prefix, "participant_common.csv")) %>% 
  full_join(read_csv(file = paste0(prefix, "hh_common.csv"))) %>% 
  full_join(read_csv(file = paste0(prefix, "sday.csv"))) %>% 
  full_join(read_csv(file = paste0(prefix, "participant_extra.csv")))

contacts <- read_csv(file = paste0(prefix, "contact_common.csv"))

# Population data
# see: https://opendata.cbs.nl/#/CBS/nl/dataset/83482NED/table?dl=98643
# download from CBS (Statistics Netherlands) website
source("./scripts/data/08_Get_population_data.R")

# Reference education level data
# see: https://opendata.cbs.nl/statline/#/CBS/nl/dataset/82816ned/table?dl=9846E
# download from CBS (Statistics Netherlands) website
source("./scripts/data/09_Get_education_data.R")

# Reference medical risk data
# see: https://www.nivel.nl/sites/default/files/bestanden/1004308.pdf
medical_risk_data <- tibble(age = c("0-59", "60-64", "65+"),
                            frac_high = c(0.094, 2.7/7.2, 12.0/21.2),
                            frac_pop = c(1-0.284, 0.072, 0.212)) 


rm(prefix)
