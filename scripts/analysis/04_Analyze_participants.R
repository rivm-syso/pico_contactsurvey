################################################################################
#
# Analysis of participants
# - overview table of participants
# - figure of participants by medical risk group and education level
# - overview table of weighted participants (Suppl S2)
# 
################################################################################

# Overview table

tmp_part <- participants %>% 
  mutate(survey = if_else(round == 0, "baseline", "PiCo") %>% factor(),
         round = factor(round),
         hh_size = if_else(hh_size >= 5, "5+", as.character(hh_size)) %>% factor())


part_table <- bind_rows(
  "Participant age group" = tmp_part %>% 
    rename(var = part_age_group) %>% 
    group_by(round, var) %>% 
    count(),
  "Participant sex" = tmp_part %>% 
    mutate(part_gender = fct_recode(part_gender, Female = "F", Male = "M")) %>% 
    rename(var = part_gender) %>% 
    group_by(round, var) %>% 
    count(),
  "Household size" = tmp_part %>% 
    mutate(hh_size = if_else(participant_withhh, hh_size, NA)) %>% 
    rename(var = hh_size) %>% 
    group_by(round, var) %>% 
    count(),
  "Medical risk group" = tmp_part %>% 
    rename(var = medical_risk) %>% 
    group_by(round, var) %>% 
    count(),
  "Education level" = tmp_part %>% 
    rename(var = education_level) %>% 
    group_by(round, var) %>% 
    count(),
  .id = "name") %>% 
  group_by(round, name, .drop = FALSE) %>% 
  mutate(perc = 100*n/sum(n),
         var = if_else(is.na(var), "(Missing)", var),
         rank = 1:n(),
         perc = sprintf("%0.1f", perc)) %>% 
  select(round, name, rank, var, perc) %>% 
  pivot_wider(names_from = round, values_from = perc) %>% 
  replace_na(replace = as.list(setNames(rep("0.0", 11), 0:10)))

reference_table <- bind_rows(
  population %>% 
    filter(round == 1) %>% 
    group_by(age_group) %>% 
    summarise(ref = 100*sum(frac_pop)) %>% 
    mutate(name = "Participant age group") %>% 
    rename(var = age_group),
  population %>% 
    filter(round == 1) %>% 
    mutate(sex = if_else(sex == "F", "Female", "Male")) %>% 
    group_by(sex) %>% 
    summarise(ref = 100*sum(frac_pop)) %>% 
    mutate(name = "Participant sex") %>% 
    rename(var = sex),
  # see (2020): https://opendata.cbs.nl/statline/#/CBS/nl/dataset/37975/table?dl=9E579
  # corrected for size bias
  tibble(name = "Household size",
         var = 1:5,
         hh_NL = c(3079778, 2610601, 938515, 961314, 407592)) %>% 
  mutate(ref = 100*(1:5)*hh_NL/sum((1:5)*hh_NL),
           var = if_else(var >= 5, "5+", as.character(var))) %>% 
    select(-hh_NL),
  # see: https://www.nivel.nl/sites/default/files/bestanden/1004308.pdf
  medical_risk_data %>% 
    summarise(
      name = "Medical risk group",
      "frac_High" = 100*sum(frac_high*frac_pop)/sum(frac_pop),
      "frac_Low" = 100*sum((1-frac_high)*frac_pop)/sum(frac_pop)) %>% 
    pivot_longer(cols = starts_with("frac"), names_to = "var", values_to = "ref", names_prefix = "frac_"),
  # see: https://opendata.cbs.nl/statline/#/CBS/nl/dataset/82816ned/table?dl=9846E
  education_data %>% 
    group_by(education_level) %>% 
    summarise(n = sum(n)) %>% 
    mutate(name = "Education level",
           ref = 100*n/sum(n)) %>% 
    rename(var = education_level) %>% 
    select(-n)
) %>% 
  mutate(ref = sprintf("%0.1f", ref))


  

participants_table <- bind_rows(
  # first line with survey months per pico round
  round_dates %>%
    mutate(survey_month = format(round_date(date_median, unit = "month"), "%b %Y"),
           survey_month = if_else(round == 0, "", survey_month)) %>%
    arrange(round) %>%
    transmute(round = round,
              name = "Survey month",
              var = "",
              n = survey_month) %>% 
    pivot_wider(names_from = round, values_from = n),
  # second line with number of participants per survey round
  tmp_part %>% 
    group_by(round) %>% 
    count() %>% 
    mutate(name = "Number of participants",
           var = "",
           n = as.character(n)) %>% 
    pivot_wider(names_from = round, values_from = n),
  # rest of table are percentages per variable per survey round
  part_table %>% 
    full_join(reference_table) %>% 
    mutate(name = if_else(rank == 1, name, "")) %>% 
    select(-rank)
  ) %>% 
  replace_na(replace = list(ref = ""))

saveRDS(participants_table, "./results/participants_table.rds")


# Figure medical risk

fig_medical <- participants %>%
  filter(round >= 4,
         !duplicated(participant_id)) %>% 
  mutate(part_age_group = cut(part_age, breaks = c(seq(0, 15, 5), seq(20, 80, 10), Inf), right = FALSE, include.lowest = TRUE,
                              labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))) %>% 
  mutate(medical_risk = if_else(is.na(medical_risk), "(Missing)", medical_risk) %>%
           factor(levels = c("(Missing)", "Low", "High"))) %>%
  ggplot(aes(x = part_age_group, fill = medical_risk)) +
  geom_bar(position = "fill") +
  geom_segment(data = medical_risk_data %>% 
                 mutate(x = c(0.55, 8.55, 9),
                        xend = c(8.45, 9, 11.45)),
               aes(x = x, xend = xend, y = frac_high, yend = frac_high),
               inherit.aes = FALSE) +
  scale_fill_manual(values = c("darkgrey", '#bae4bc','#2b8cbe')) +
  labs(x = NULL,
       y = "Fraction",
       fill = "Medical risk group") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# Figure education level

tmp <- education_data %>% 
  group_by(start_age) %>% 
  summarise(frac1 = sum(n[education_level == "High"])/sum(n),
            frac2 = sum(n[education_level != "Low"])/sum(n)) %>% 
  mutate(age = as.integer(factor(start_age)) + 3,
         age_end = if_else(age == 10, 11.45, age + 1),
         age = if_else(age == 4, 3.55, age)) 

fig_education <- participants %>%
  filter(round > 0,
         !duplicated(participant_id)) %>% 
  mutate(part_age_group = cut(part_age, breaks = c(seq(0, 15, 5), seq(20, 80, 10), Inf), right = FALSE, include.lowest = TRUE,
                              labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))) %>% 
  mutate(education_level = if_else(is.na(education_level), "(Missing)", education_level) %>% 
           factor(levels = c("(Missing)", "Low", "Medium", "High"))) %>%
  ggplot(aes(x = part_age_group, fill = education_level)) +
  geom_bar(position = "fill") +
  geom_segment(data = tmp,
               aes(x = age, xend = age_end, y = frac1, yend = frac1),
               inherit.aes = FALSE) +
  geom_segment(data = tmp,
               aes(x = age, xend = age_end, y = frac2, yend = frac2),
               inherit.aes = FALSE) +
  scale_fill_manual(values = c("darkgrey", '#bae4bc','#7bccc4','#2b8cbe')) +
  labs(x = "Participant age group",
       y = "Fraction",
       fill = "Education level") +
  theme_minimal()

plot_grid(fig_medical,
          NULL,
          fig_education,
          labels = c("A", "", "B"),
          align = "hv",
          rel_heights = c(1, -0.1, 1),
          ncol = 1)

ggsave(filename = paste0("./figures/Participants.png"), height = 7, width = 7, dpi = 300, bg = "white")




# Weighted participants table (Suppl S2)

# Split 10-19 participant age group in 10-14 and 15-19 because reference education level starts from 15 years
tmp_part <- participants %>% 
  mutate(part_age_group = cut(part_age, breaks = c(seq(0, 15, 5), seq(20, 80, 10), Inf), right = FALSE, include.lowest = TRUE,
                              labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))) %>% 
  mutate(survey = if_else(round == 0, "baseline", "PiCo") %>% factor(),
         round = factor(round),
         hh_size = if_else(hh_size >= 5, "5+", as.character(hh_size)) %>% factor(),
         education_level = if_else(part_age < 15, NA_character_, education_level),
         education_level = factor(education_level, levels = levels(participants$education_level)))

tmp_pop <- population %>% 
  mutate(age_group = cut(age, breaks = c(seq(0, 15, 5), seq(20, 80, 10), Inf), right = FALSE, include.lowest = TRUE,
                         labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")),
         round = factor(round)) %>% 
  group_by(round, age_group, sex) %>% 
  summarise(frac_pop = sum(frac_pop))

part_table_weighted <- tibble()

for(name in c("hh_size", "medical_risk", "education_level")) {
  
  weights <- tmp_part %>% 
    rename(var = name) %>%
    filter(!is.na(var)) %>%
    group_by(round, part_age_group, part_gender) %>% 
    count(name = "n_part") %>% 
    group_by(round) %>% 
    mutate(frac_part = n_part/sum(n_part)) %>% 
    full_join(tmp_pop,
              by = c("round" = "round", "part_age_group" = "age_group", "part_gender" = "sex")) %>% 
    mutate(weight = if_else(n_part == 0, 0, frac_pop/frac_part))
 
  tmp <- tmp_part %>%  
    rename(var = name) %>% 
    filter(!is.na(var)) %>%
    left_join(weights) %>% 
    group_by(round, var) %>% 
    summarise(n_part = n(),
              n_part_weighted = sum(weight)) %>% 
    group_by(round) %>%
    mutate(frac = n_part/sum(n_part),
           frac_weighted = n_part_weighted/sum(n_part_weighted),
           name = name) 
 
  part_table_weighted <- bind_rows(part_table_weighted, tmp)
}


part_table_weighted <- part_table_weighted %>% 
  mutate(name = factor(name,
                       levels = c("hh_size", "medical_risk", "education_level"),
                       labels = c("Household size", "Medical risk group", "Education level"))) %>% 
  group_by(round, name, .drop = FALSE) %>% 
  mutate(perc = 100*frac_weighted,
         rank = 1:n(),
         perc = sprintf("%0.1f", perc)) %>% 
  select(round, name, rank, var, perc) %>% 
  pivot_wider(names_from = round, values_from = perc) %>% 
  replace_na(replace = as.list(setNames(rep("0.0", 11), 0:10)))


participants_table_weighted <- bind_rows(
  # first line with survey months per pico round
  round_dates %>%
    mutate(survey_month = format(round_date(date_median, unit = "month"), "%b %Y"),
           survey_month = if_else(round == 0, "", survey_month)) %>%
    arrange(round) %>%
    transmute(round = round,
              name = "Survey month",
              var = "",
              n = survey_month) %>% 
    pivot_wider(names_from = round, values_from = n),
  # second line with number of participants per survey round
  tmp_part %>% 
    group_by(round) %>% 
    count() %>% 
    mutate(name = "Number of participants",
           var = "",
           n = as.character(n)) %>% 
    pivot_wider(names_from = round, values_from = n),
  # rest of table are percentages per variable per survey round
  part_table_weighted %>% 
    full_join(reference_table) %>% 
    mutate(name = if_else(rank == 1, name, "")) %>% 
    select(-rank)
) %>% 
  filter(!is.na(name)) %>% 
  replace_na(replace = list(ref = ""))

saveRDS(participants_table_weighted, "./results/participants_table_weighted.rds")

