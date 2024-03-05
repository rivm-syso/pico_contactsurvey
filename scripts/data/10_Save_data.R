################################################################################
#
# Save data in socialcontactdata.org format
# - Cleaned data available from https://zenodo.org/record/10370353
# 
################################################################################


##################### Original data ############################################

# note that random participant id's are stored for pico participants in pico_data_raw
# but for p3 participants (that did not do pico) a unused participant id is assigned
# in 05_Combine_date.R
write_csv(data, "./data/pico_p3_contactdata_withkey.csv")

##################### Open data ################################################


prefix <- "./data/PienterCorona_NL_"


# participant_common

data %>% 
  select(part_id, hh_id, part_age, part_gender) %>% 
  write_csv(paste0(prefix, "participant_common.csv"))


# participant_extra

data %>%
  select(part_id, 
         participant_id,
         participant_withhh,
         medical_risk,
         education_level,
         workschoolyesterday,
         workathome,
         workathome_beforecovid) %>% 
  write_csv(paste0(prefix, "participant_extra.csv"))


# hh_common

data %>% 
  mutate(country = "NL") %>% 
  select(hh_id, country, hh_size, hh_size_reported) %>% 
  write_csv(paste0(prefix, "hh_common.csv"))


# sday

data %>% 
  rename(round = pico) %>% 
  rename(dayofweek = survey_day) %>% 
  mutate(survey_day = levels(wday(today(), label=TRUE, abbr=FALSE, week_start = 1))[dayofweek],
         year = format(survey_date, "%Y")) %>% 
  select(part_id, round, survey_date, survey_day, dayofweek, year) %>% 
  write_csv(paste0(prefix, "sday.csv"))


# contact_common

contacts %>% 
  mutate(cnt_age_exact = cnt_age) %>% 
  select(part_id,
         cnt_id,
         cnt_age_exact,
         cnt_age_est_min,
         cnt_age_est_max,
         cnt_gender,
         hh_member,
         cnt_type) %>% 
  write_csv(paste0(prefix, "contact_common.csv"))


# population

population %>% 
  write_csv(paste0(prefix, "population.csv"))


# education level

education_data %>% 
  write_csv(paste0(prefix, "education.csv"))


rm(prefix)
rm(data)
rm(contacts)
rm(population)
rm(education_data)



