################################################################################
#
# Prepare data for analysis of pico contacts
# - exclude participants < 1 years of age (only from baseline survey)
# - combine age groups to harmonize analysis
# - add weekend/weekday distinction
# - restrict maximum number of contacts per participant per cnt_age_group to 50
# - determine dates of pico rounds
# 
################################################################################

# Combine part_age groups to harmonize analysis
participants <- participants %>% 
  # exclude 387 (oversampled) 0-year olds from baseline survey (round 0)
  filter(part_age > 0) %>% 
  mutate(part_gender = factor(part_gender, 
                              levels = c("F", "M")),
         # lump all 85+ as 85-year olds, because there are few 85+ participants
         part_age = if_else(part_age > 85, 85, part_age),
         part_age_group = cut(part_age, breaks = c(seq(0, 10, 5), seq(20, 80, 10), Inf), right = FALSE, include.lowest = TRUE,
                              labels = c("0-4", "5-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")),
         medical_risk = factor(medical_risk, 
                               levels = c("low", "high"),
                               labels = c("Low", "High")),
         education_level = factor(education_level, 
                                  levels = c("low", "medium", "high"),
                                  labels = c("Low", "Medium", "High")))


# Add weekend or weekday (needed for sample weights)
participants <- participants %>% 
  mutate(weekend = factor(if_else(dayofweek >= 6, TRUE, FALSE, FALSE)))

# Construct cnt_age group and combine cnt_age groups to harmonize analysis
contacts <- contacts %>% 
  mutate(cnt_gender = factor(cnt_gender),
         cnt_age_group = paste0(cnt_age_est_min, "-", cnt_age_est_max),
         cnt_age_group = if_else(cnt_age_group %in% c("80-89", "90-120"), "80+", cnt_age_group),
         cnt_age_group = if_else(cnt_age_group %in% c("10-14", "15-19"), "10-19", cnt_age_group),
         cnt_age_group = factor(cnt_age_group, levels = c("0-4", "5-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))) 

# Define population age groups to harmonize analysis
population <- population %>%
  mutate(sex = factor(sex),
         # lump all 85+ as 85-year olds, consistent with participants
         age = if_else(age > 85, 85, age),
         age_group = cut(age, breaks = c(seq(0, 10, 5), seq(20, 80, 10), Inf), right = FALSE, include.lowest = TRUE,
                         labels = c("0-4", "5-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))) %>% 
  group_by(round) %>% 
  mutate(frac_pop = n/sum(n))


# 95th percentile of community contacts is 59 (max is 6477)
contacts %>% 
  filter(!hh_member) %>% 
  group_by(part_id) %>% 
  summarise(n_cont = sum(!is.na(cnt_id))) %>% 
  reframe(probs = c(0.5, 0.95, 0.975),
          n = quantile(n_cont, probs = probs),
          max = max(n_cont))

# Restrict maximum number of community contacts per participant per cnt_age to 50
set.seed(123456789)
contacts <- bind_rows(contacts %>% 
                        filter(hh_member),
                      contacts %>% 
                        filter(!hh_member) %>%
                        group_by(part_id, cnt_age_group) %>% 
                        mutate(n = sample(n(), replace = FALSE),
                               max_n = max(n)) %>% 
                        filter(n <= 50) %>% 
                        select(-n))

# after restriction, 95th percentile of community contacts is 54 (max is 500)
contacts %>% 
  filter(!hh_member) %>% 
  group_by(part_id) %>% 
  summarise(n_cont = sum(!is.na(cnt_id))) %>% 
  reframe(probs = c(0.5, 0.95, 0.975),
          n = quantile(n_cont, probs = probs),
          max = max(n_cont))

# in 1262 questionnaires, number of community contacts was restricted
contacts %>% 
  #left_join(participants %>% select(part_id, participant_id)) %>% 
  filter(max_n > 50) %>% 
  filter(!duplicated(part_id)) %>% nrow


# Determine dates for rounds (for time axis of figures)
round_dates <- participants %>% 
  group_by(round) %>% 
  summarise(date_median = median(survey_date, na.rm = TRUE),
            date_min = min(survey_date, na.rm = TRUE),
            date_max = max(survey_date, na.rm = TRUE)) %>% 
  filter(round > 0) %>% 
  add_row(round = 0) %>% 
  mutate(date_median = if_else(round == 0, ymd("2020-01-29"), date_median),
         date_min = if_else(round == 0, min(date_min, na.rm = TRUE) - 100, date_min),
         date_max = if_else(round == 0, max(date_max, na.rm = TRUE) + 100, date_max),
         date_mean = date_min + (date_max - date_min)/2)

