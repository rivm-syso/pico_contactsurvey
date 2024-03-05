################################################################################
#
# Get population data for each pico survey month from CBS
# 
################################################################################

# Determine which 1st of month is closest to median survey date by pico
# from data when used in data script, but allow for standalone use in analysis script

if("pico" %in% names(data)) {
  
  population_months <- data %>% 
    rename(round = pico) %>% 
    group_by(round) %>% 
    summarise(date_median = median(survey_date, na.rm = TRUE),
              survey_month = round_date(date_median, "month")) %>% 
    mutate(Perioden = format(survey_month, "%YMM%m"))
  
} else {
  
  population_months <- tibble(Perioden = c("2017MM02", "2020MM04", "2020MM06", "2020MM10", "2021MM03", "2021MM07", "2021MM11", "2022MM04", "2022MM07", "2022MM11", "2023MM05")) %>% 
    mutate(round = 0:(n()-1),
           survey_month = as.Date(paste0(Perioden, "-01"), format = "%YMM%m-%d"))
  
}



# Get CBS data from table
# https://opendata.cbs.nl/#/CBS/nl/dataset/83482NED/table?dl=98643


metadata <- cbs_get_meta("83482NED")

population <- cbs_get_data("83482NED", 
                     Perioden = population_months$Perioden,
                     Migratieachtergrond = "T001040",
                     Generatie = "T001040",
                     Geslacht = metadata$Geslacht %>% filter(Title %in% c("Mannen", "Vrouwen")) %>% pull(Key),
                     Leeftijd = metadata$Leeftijd %>% filter(CategoryGroupID == 2) %>% pull(Key)) %>% 
  cbs_add_label_columns()



population <- population %>% 
  # recode age to integers
  mutate(age = gsub(Leeftijd_label, pattern = " of ouder", replacement = ""),
         age = as.integer(gsub(age, pattern = " jaar", replacement = "")),
         sex = if_else(Geslacht_label == "Mannen", "M", "F")) %>% 
  # summarise by pico round, age and sex
  group_by(Perioden, age, sex) %>% 
  summarise(n = sum(BevolkingOpDeEersteVanDeMaand_1)) %>% 
  ungroup() %>% 
  full_join(population_months) %>% 
  select(round, survey_month, age, sex, n)


rm(metadata)
rm(population_months)
