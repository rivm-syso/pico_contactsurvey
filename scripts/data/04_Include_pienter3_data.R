################################################################################
#
# Exclude participants
# - when contact day is not given and number of contacts = 0 
# - when household size is not given, set participant_withhh to FALSE
#
# Align p3 data with pico data:
# - align participant id's
# - align variable names and labels
#
################################################################################


# check combinations of whether contactday and household are reported and number of contacts
p3_data %>% 
  mutate(contacttotal = if_else(is.na(contacttotal), 0, contacttotal),
         contactday_reported = !is.na(contactday),
         hh_size = rowSums(!is.na(across(starts_with("householdage"))), na.rm = TRUE)) %>% 
  group_by(contactday_reported, (hh_size > 0), contacttotal > 0) %>% 
  count()

p3_data <- p3_data %>% 
  mutate(contacttotal = if_else(is.na(contacttotal), 0, contacttotal),
         hh_size = rowSums(!is.na(across(starts_with("householdage"))), na.rm = TRUE)) %>% 
  # exclude 374+784 participants that did not report contactday nor any contacts
  filter(!(is.na(contactday) & contacttotal == 0)) %>% 
  # set participant_withhh = TRUE for participants with reported hh distribution
  # (to be able to exclude participants !participant_withhh when constructing home contacts)
  mutate(participant_withhh = if_else(hh_size > 0, TRUE, FALSE))
  

# harmonize variable names and labels
p3_data <- p3_data %>% 
  mutate(part_gender = if_else(gender_true == 1, "Man", "Vrouw")) %>% 
  # allgroep ok
  mutate(zipcode4 = as.character(zipcode4_true)) %>% 
  rename(part_age = age_year) %>% 
  # include all participants (also oversampled groups) except 1276 LVC participants (oversampling_group 1)
  filter(is.na(oversampling_group) | oversampling_group != 1) %>% 
  # for participants < 15 years, education is highest education of parents
  # 1 = No completed education (primary school, not completed)
  # 2 = Primary education (primary school, special primary school)
  # 3 = Junior or pre-vocational secondary education (such as junior secondary technical school (LTS), junior commercial education (LEAO), junior domestic science and vocational education (LHNO), junior secondary vocational education (LBO), and the basic vocational track (BB), advanced vocational track (KB), combined track (GL) of pre-vocational secondary education (VMBO))
  # 4 = Junior general secondary education (such as junior general secondary education (MAVO), higher elementary education (MULO), advanced elementary education (ULO), basic vocational education (MBO-kort), the theoretical track of pre-vocational secondary education (VMBO-TL))
  # 5 = Senior secondary vocational education and work-based education (such as senior secondary vocational education (MBO-lang), senior secondary technical school (MTS), upper secondary vocational education in business and administration (MEAO), school-based pathway (BOL), work-based pathway (BBL), and medical assistant training (INAS))
  # 6 = Senior general secondary education and pre-university education (such as senior general secondary education (HAVO), pre-university education (VWO) without Latin or Greek (Atheneum) or with Latin and Greek (Gymnasium), secondary modern school (HBS), girls' secondary school (MMS))
  # 7 = Higher professional education (such as school for higher professional education (HBO), institute of technology (HTS), school for higher education in economics and management (HEAO), Bachelor degree))
  # 8 = University level education
  mutate(educationparent = pmax(educationparent1, educationparent2, na.rm = TRUE),
         education = coalesce(education, educationparent)) %>%
  mutate(hh_size_reported = household) %>% 
  mutate(part_weight = coalesce(childweight, weight),
         part_height = coalesce(childlength, howtall)) %>% 
  rename(cont00tot04jaarman = contact04ymen1,
         cont00tot04jaarvrouw = contact04ywom1,
         cont05tot09jaarman = contact59ymen1,
         cont05tot09jaarvrouw = contact59ywom1,
         cont10tot19jaarman = contact1019ymen1,
         cont10tot19jaarvrouw = contact1019ywom1,
         cont20tot29jaarman = contact2029men1,
         cont20tot29jaarvrouw = contact2029ywom1,
         cont30tot39jaarman = contact3039men1,
         cont30tot39jaarvrouw = contact3039ywom1,
         cont40tot49jaarman = contact4049men1,
         cont40tot49jaarvrouw = contact4049ywom1,
         cont50tot59jaarman = contact5059men1,
         cont50tot59jaarvrouw = contact5059ywom1,
         cont60tot69jaarman = contact6069men1,
         cont60tot69jaarvrouw = contact6069ywom1,
         cont70tot79jaarman = contact7079men1,
         cont70tot79jaarvrouw = contact7079ywom1,
         cont80tot89jaarman = contact8089men1,
         cont80tot89jaarvrouw = contact8089ywom1,
         cont90plusjaarman = contact90men1,
         cont90plusjaarvrouw = contact90wom1) %>% 
  mutate(flushot = if_else(flushot == 1, FALSE, TRUE, NA)) %>% 
  # flip diseasenone (have you never had a disease) to disease (have you ever had a disease)
  mutate(disease = 3-diseasenone) 
  
  