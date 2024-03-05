################################################################################
#
# Analyze work from home
# - by age group over rounds
# - by education level over rounds
# 
################################################################################

# last 2 rounds, 58% of participants work completely at work
participants %>% 
  filter(round > 8) %>% 
  filter(part_age >= 20 & part_age < 70) %>% 
  filter(workathome %in% 1:3) %>% 
  filter(!duplicated(participant_id)) %>% 
  summarise(frac_work = sum(workathome == 3)/n())

# before covid, 77% of participants work completely at work
participants %>% 
  filter(part_age >= 20 & part_age < 70) %>% 
  filter(workathome_beforecovid %in% 1:3) %>% 
  filter(!duplicated(participant_id)) %>% 
  summarise(frac_work_beforecovid = sum(workathome_beforecovid == 3)/n())


# in lockdown rounds, 72%, 59%, 27% of participants with low, medium, high education level work completely at work
participants %>% 
  filter(round == 1 | round == 4) %>% 
  filter(part_age >= 20 & part_age < 70) %>% 
  filter(workathome %in% 1:3) %>% 
  filter(!is.na(education_level)) %>% 
  filter(!duplicated(participant_id)) %>% 
  group_by(education_level) %>% 
  summarise(frac_work = sum(workathome == 3)/n())


# last 2 rounds, 77%, 72%, 47% of participants with low, medium, high education level work completely at work
participants %>% 
  filter(round > 8) %>% 
  filter(part_age >= 20 & part_age < 70) %>% 
  filter(workathome %in% 1:3) %>% 
  filter(!is.na(education_level)) %>% 
  filter(!duplicated(participant_id)) %>% 
  group_by(education_level) %>% 
  summarise(frac_work = sum(workathome == 3)/n())

# before covid, 88%, 85%, 70% of participants with low, medium, high education level work completely at work
participants %>% 
  filter(part_age >= 20 & part_age < 70) %>% 
  filter(workathome_beforecovid %in% 1:3) %>% 
  filter(!is.na(education_level)) %>% 
  filter(!duplicated(participant_id)) %>% 
  group_by(education_level) %>% 
  summarise(frac_work_beforecovid = sum(workathome_beforecovid == 3)/n())



fig_age <- participants %>% 
  filter(part_age >= 20 & part_age < 70) %>% 
  filter(workathome %in% 1:3) %>% 
  group_by(part_age_group, workathome, round) %>% 
  count %>% 
  bind_rows(participants %>% 
              filter(part_age >= 20 & part_age < 70) %>% 
              filter(workathome_beforecovid %in% 1:3) %>% 
              group_by(part_age_group, workathome_beforecovid) %>% 
              count %>% 
              mutate(round = 11,
                     workathome = workathome_beforecovid)
  ) %>% 
  ggplot(aes(x = factor(round), y = n, fill = factor(workathome) %>% fct_rev)) +
  geom_bar(stat = "identity",
           position = "fill") +
  scale_x_discrete(labels = c(1:10, 0)) +
  scale_fill_manual(values = c("#56B1F7", "#336A98", "#132B43"),
                    labels = c("at work", "at work and home", "at home")) +
  labs(x = NULL,
       y = "Fraction",
       fill = "Last week I worked") +
  theme_minimal() +
  theme(legend.position = c(5/6, 1/4)) +
  facet_wrap(facets = vars(part_age_group))



fig_edu <- participants %>% 
  filter(!is.na(education_level)) %>% 
  filter(part_age >= 20 & part_age < 70) %>% 
  filter(workathome %in% 1:3) %>% 
  group_by(education_level, workathome, round) %>% 
  count %>% 
  bind_rows(participants %>% 
              filter(!is.na(education_level)) %>% 
              filter(part_age >= 20 & part_age < 70) %>% 
              filter(workathome_beforecovid %in% 1:3) %>% 
              group_by(education_level, workathome_beforecovid) %>% 
              count %>% 
              mutate(round = 11,
                     workathome = workathome_beforecovid)
  ) %>% 
  ggplot(aes(x = factor(round), y = n, fill = factor(workathome) %>% fct_rev)) +
  geom_bar(stat = "identity",
           position = "fill") +
  scale_x_discrete(labels = c(1:10, 0)) +
  scale_fill_manual(values = c("#56B1F7", "#336A98", "#132B43"),
                    labels = c("at work", "at work and home", "at home")) +
  labs(x = "Survey round",
       y = "Fraction",
       fill = "Last week I worked") +
  theme_minimal() +
  guides(fill = "none") +
  #theme(legend.position = c(5/6, 1/4)) +
  facet_wrap(facets = vars(education_level))


plot_grid(fig_age,
          NULL,
          fig_edu,
          labels = c("A", "", "B"),
          align = "v",
          rel_heights = c(2, 0, 1.2),
          ncol = 1)


ggsave(filename = paste0("./figures/Workfromhome_age_edu.png"), height = 7, width = 7, dpi = 300, bg = "white")
