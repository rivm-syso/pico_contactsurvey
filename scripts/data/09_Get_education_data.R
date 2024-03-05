################################################################################
#
# Retrieve data for education level in Dutch population from CBS
# https://opendata.cbs.nl/statline/#/CBS/nl/dataset/85184NED/table?dl=984E1
# Education level for 15 to 90 year olds, in Q1 2021
# Used as reference data in overview table participants and 
# Used as reference data in figure of education level of participants by age group
#
################################################################################

metadata <- cbs_get_meta("85184NED")

education_data_cbs <- cbs_get_data("85184NED", 
                               Perioden = "2021KW01",
                               Geslacht = metadata$Geslacht %>% filter(Title %in% c("Mannen", "Vrouwen")) %>% pull(Key),
                               Onderwijsniveau = metadata$Onderwijsniveau %>% filter(CategoryGroupID == 5) %>% pull(Key),
                               Onderwijsrichting = "T001072",
                               Leeftijd = c("53993", metadata$Leeftijd %>% filter(CategoryGroupID == 1) %>% pull(Key))) %>% 
  cbs_add_label_columns()



education_data <- education_data_cbs %>% 
  transmute(education_level = case_when(Onderwijsniveau_label == "1 Laag onderwijsniveau" ~ "Low",
                                        Onderwijsniveau_label == "2 Middelbaar onderwijsniveau" ~ "Medium",
                                        Onderwijsniveau_label == "3 Hoog onderwijsniveau" ~ "High"),
            total = if_else(Leeftijd_label == "15 tot 90 jaar", TRUE, FALSE),
            start_age = as.integer(sapply(str_split(Leeftijd_label, " "), "[[", 1)),
            end_age = as.integer(sapply(str_split(Leeftijd_label, " "), "[[", 3)),
            sex = if_else(Geslacht_label == "Mannen", "M", "F"),
            n = Bevolking_1)


education_data <- bind_rows(education_data %>% 
                              filter(!total) %>%
                              select(-total),
                            education_data %>% 
                              filter(!total) %>% 
                              group_by(sex, education_level) %>% 
                              summarise(n_tmp = sum(n),
                                        start_age = max(end_age)) %>% 
                              full_join(education_data %>% 
                                          select(-start_age) %>% 
                                          filter(total)) %>% 
                              mutate(n = n - n_tmp) %>% 
                              select(-total, -n_tmp)
) %>% 
  mutate(age_group = paste0(start_age, "-", end_age - 1))


rm(metadata)
rm(education_data_cbs)
