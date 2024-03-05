################################################################################
#
# Clean participant data:
# - Generate part_id from participant_id and pico round
# - Add survey day based on fill-out date and compare to self-reported survey day
# - Clean vaccination status
# - Determine medical risk (use influenza vaccination indicators except age as proxy)
# - Compare medical risk to influenza vaccination status
# 
################################################################################


data <- data %>% 
  # Generate part_id and hh_id
  mutate(part_id = 100*participant_id + pico,
         hh_id = paste0("HH", part_id)) %>% 
  # rename and recode participant age and gender
  mutate(part_gender = if_else(part_gender == "Man", "M", "F", NA_character_),
         part_age_group = cut(part_age, breaks = c(seq(0, 15, 5), seq(20, 80, 10), Inf), include.lowest = TRUE, right = FALSE,
                              labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")))


# Survey date is the day before the fill out date (note SAS origin date is 1 Jan 1960)
# Survey day is the weekday (1 Monday to 7 Sunday), either self-reported (welkedaggist) or based on survey date,
# where the self-reported survey day has priority over the weekday of the survey date
data <- data %>% 
  mutate(survey_date = as.Date(fillindate, origin = as.Date("1960-01-01")) - 1,
         survey_date = if_else(survey_date < as.Date("2016-01-01") | survey_date > as.Date("2023-06-01"), ymd(NA), survey_date),
         survey_day_fromdate = wday(survey_date, label = FALSE, week_start = 1),
         survey_day = coalesce(contactday, survey_day_fromdate)) 



###### COVID-19 vaccination status ######

# coronavaccinatie in pico 4: 1 (Yes), 2:5 (No), 6 (Unknown)
# coronavaccinatie in pico 5: 1 (Yes), 2:6 (No), 7 (Unknown)
# coronavaccinatie_volw in pico 6: 1 (Yes), 2:6 (No), 7 (Unknown)
# coronavaccinatie_kind in pico 6: 1 (Yes), 2:6 (No), 7 (Unknown)
# coronavaccinatie_volw in pico 7: 1 (Yes), 2 (No), 7 (Unknown)
# coronavaccinatie_kind in pico 7: 1 (Yes), 2:6 (No), 7 (Unknown)
# coronavaccinatie_volw in pico 8,9,10: 1 (Yes), 2 (No), 7:8 (Unknown)
# coronavaccinatie_kind in pico 8,9,10: 1 (Yes), 2 (No), 3:4 (Unknown)
# question not asked to children < 15 years in pico 4 and 5 -> set to FALSE
data <- data %>% 
  mutate(vaccinated = case_when(pico == 4 & coronavaccinatie == 1 ~ TRUE,
                                pico == 4 & coronavaccinatie %in% 2:5 ~ FALSE,
                                pico == 5 & coronavaccinatie == 1 ~ TRUE,
                                pico == 5 & coronavaccinatie %in% 2:6 ~ FALSE,
                                pico %in% 4:5 & part_age < 15 ~ FALSE,
                                pico %in% 6:7 & (coronavaccinatie_volw == 1 | coronavaccinatie_kind == 1) ~ TRUE,
                                pico %in% 6:7 & (coronavaccinatie_volw %in% 2:6 | coronavaccinatie_kind %in% 2:6) ~ FALSE,
                                pico >= 8 & (coronavaccinatie_volw == 1 | coronavaccinatie_kind == 1) ~ TRUE,
                                pico >= 8 & (coronavaccinatie_volw == 2 | coronavaccinatie_kind == 2) ~ FALSE,
                                TRUE ~ NA))


###### Medical risk ######

# Medical risk determined by influenza vaccination indication
# https://lci.rivm.nl/richtlijnen/influenzavaccinatie
# https://lci.rivm.nl/richtlijnen/asplenie
# exclude rheumatic arthritis, not in flu vaccination indication
# also morbid obesity (BMI >= 40)

data <- data %>% 
  select(-diseaserheumati) %>% 
  # add BMI (= weight in kg / (height in m)^2)
  # only valid for 18+ participants and in realistic range [15, 60)
  # morbid obesity when BMI >= 40
  mutate(BMI = part_weight/(part_height/100)^2,
         BMI = if_else(part_age >= 18 & BMI >= 15 & BMI < 60, BMI, NA_real_),
         diseaseobesity = if_else(BMI >= 40, 1, 2, NA_real_)) %>% 
  # complete disease obesity for intermediate rounds
  group_by(participant_id) %>% 
  arrange(pico) %>% 
  fill(diseaseobesity) %>% 
  ungroup() %>% 
  # determine when at least 2 out of 12 diseases are reported 
  # (this is to make sure that a BMI < 40 does not lead to low medical risk when all other diseases are NA)
  # determine if any disease is present
  # medical_risk = TRUE if any disease is present
  # medical_risk = FALSE when at least 2 out 12 diseases are not NA and low risk
  # medical_risk = NA otherwise
  mutate(allna_disease = (rowSums(is.na(across(starts_with("disease"))), na.rm = FALSE) >= 11),
         any_disease = (rowSums(across(starts_with("disease")) == 1, na.rm = TRUE) > 0),
         medical_risk = if_else(any_disease, "high", if_else(allna_disease, NA, "low")))


# for participants with an unambiguous medical risk, missing medical risk is imputed
# (ambiguous = when both TRUE and FALSE are reported)
# because medical risk definitions differ, this is done separately for pico 1 and 2, and for pico 3 and higher

participants_unambiguous_medical_risk <- data %>% 
  filter(!is.na(medical_risk),
         pico < 3) %>%
  group_by(participant_id) %>%
  filter(length(unique(medical_risk)) == 1) %>%
  summarise(unique_medical_risk = unique(medical_risk)) %>%
  select(participant_id, unique_medical_risk)


data <- data %>% 
  left_join(participants_unambiguous_medical_risk) %>% 
  mutate(unique_medical_risk = if_else(pico < 3, unique_medical_risk, NA),
         medical_risk = coalesce(medical_risk, unique_medical_risk)) %>% 
  select(-unique_medical_risk)

participants_unambiguous_medical_risk <- data %>% 
  filter(!is.na(medical_risk),
         pico >= 3) %>%
  group_by(participant_id) %>%
  filter(length(unique(medical_risk)) == 1) %>%
  summarise(unique_medical_risk = unique(medical_risk)) %>%
  select(participant_id, unique_medical_risk)


data <- data %>% 
  left_join(participants_unambiguous_medical_risk) %>% 
  mutate(unique_medical_risk = if_else(pico >= 3, unique_medical_risk, NA),
         medical_risk = coalesce(medical_risk, unique_medical_risk)) %>% 
  select(-unique_medical_risk)

rm(participants_unambiguous_medical_risk)


###### Education level ######

# 1 = No completed education (primary school, not completed)
# 2 = Primary education (primary school, special primary school)
# 3 = Junior or pre-vocational secondary education (such as junior secondary technical school (LTS), junior commercial education (LEAO), junior domestic science and vocational education (LHNO), junior secondary vocational education (LBO), and the basic vocational track (BB), advanced vocational track (KB), combined track (GL) of pre-vocational secondary education (VMBO))
# 4 = Junior general secondary education (such as junior general secondary education (MAVO), higher elementary education (MULO), advanced elementary education (ULO), basic vocational education (MBO-kort), the theoretical track of pre-vocational secondary education (VMBO-TL))
# 5 = Senior secondary vocational education and work-based education (such as senior secondary vocational education (MBO-lang), senior secondary technical school (MTS), upper secondary vocational education in business and administration (MEAO), school-based pathway (BOL), work-based pathway (BBL), and medical assistant training (INAS))
# 6 = Senior general secondary education and pre-university education (such as senior general secondary education (HAVO), pre-university education (VWO) without Latin or Greek (Atheneum) or with Latin and Greek (Gymnasium), secondary modern school (HBS), girls' secondary school (MMS))
# 7 = Higher professional education (such as school for higher professional education (HBO), institute of technology (HTS), school for higher education in economics and management (HEAO), Bachelor degree))
# 8 = University level education

# Impute NA education with last reported value
# Categorize education in three levels

data <- data %>% 
  arrange(participant_id, pico) %>% 
  group_by(participant_id) %>% 
  fill(education) %>%
  ungroup() %>% 
  mutate(education_level = case_when(education %in% 1:4 ~ "low",
                                     education %in% 5:6 ~ "medium",
                                     education %in% 7:8 ~ "high",
                                     TRUE ~ NA_character_))


########## Yesterday at school/work #########

# Were you yesterday at work (paid or unpaid) or at school? (from pico 2 onwards)
# 1: Yes
# 2: No, I don't work nor go to school
# 3: No, yesterday wasn't a work or school day
# 4: No, I worked from home
# 5: No, I was ill
# 6: No, for another reason

data <- data %>% 
  mutate(workschoolyesterday = if_else(gisterwerkschool == 0, NA, gisterwerkschool))


