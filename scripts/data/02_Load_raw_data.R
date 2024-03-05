################################################################################
#
# Load raw data and code book
# Generate random participant id to avoid using pico id
# Save key (participant id <-> dn_randomisatienr) here and deposit at pico team
# 
################################################################################

# load sas files 
# pico_data <- read_sas("./data/raw_data/pico_varselection_20230901.sas7bdat") 
p3_data <- read_sas("./data/raw_data/p3_jb_20230714v2.sas7bdat") 


# generate random participant id 
# pico_data <- pico_data %>%
#   as_tibble() %>%
#   mutate(participant_id = sample(20000:99999, size = nrow(.), replace = FALSE))

# save as rds file, which is smaller and faster loading than sas
# saveRDS(pico_data,"./data/raw_data/pico_data_raw.rds")

pico_data <- readRDS("./data/raw_data/pico_data_raw.rds")
