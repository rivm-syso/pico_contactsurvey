################################################################################
#
# Examine results of contacts analysis
# - figure with number of contacts per age group per round, observed and modelled
# - figure with number of contacts by medical risk group and education level 
# 
################################################################################

results <- readRDS("./results/results_contactanalysis.rds")

# Summary results by round and age group (with bias-corrected bootstrap intervals)
results_byage <- results %>% 
  select(sample, type, round, part_age_group, cnt_age_group, starts_with(c("m_smt.", "m_obs."))) %>% 
  pivot_longer(cols = starts_with(c("m_smt.", "m_obs.")), 
               names_to = c( ".value", "i"), 
               names_pattern = "(m_smt|m_obs)(.*)") %>%
  group_by(sample, type, round, part_age_group, i) %>% 
  summarise(m_smt_bs = sum(m_smt),
            m_obs_bs = sum(m_obs)) %>% 
  left_join(results %>% 
              group_by(sample, type, round, part_age_group) %>% 
              summarise(m_smt = sum(m_smt),
                        m_obs = sum(m_obs),
                        n_part = mean(n_part))) %>% 
  filter(!is.na(m_smt_bs)) %>% # only for combined data sets with different number of bs samples
  group_by(sample, type, round, part_age_group, n_part) %>% 
  reframe(m_smt = mean(m_smt),
          m_smt_0.5 = median(m_smt_bs),
          m_obs = mean(m_obs),
          limit = c("lower", "upper"),
          m_smt_ = biascorrected_boot_ci(mean(m_smt), m_smt_bs),
          m_obs_ = biascorrected_boot_ci(mean(m_obs), m_obs_bs)) %>% 
  pivot_wider(names_from = limit, values_from = ends_with("_"), names_sep = "") %>% 
  left_join(round_dates)

ggplot(data = results_byage %>% filter(type == "community", sample == "general", round > 0)) +
  geom_rect(data = results_byage %>% filter(type == "community", sample == "general", round == 0),
            aes(xmin = date_min, xmax = date_max, ymin = m_smt_lower, ymax = m_smt_upper),
            alpha = 0.5,
            fill = "darkgrey",
            col = NA) +
  geom_segment(data = results_byage %>% filter(type == "community", sample == "general", round == 0),
               aes(x = date_min, xend = date_max, y = m_smt, yend = m_smt),
               col = "darkgrey",
               alpha = 1) +
  geom_rect(aes(xmin = date_min, xmax = date_max, ymin = m_smt_lower, ymax = m_smt_upper),
            alpha = 0.5,
            fill = "#0868ac",
            col = NA) +
  geom_segment(aes(x = date_min, xend = date_max, y = m_smt, yend = m_smt),
               alpha = 1,
               col = "#0868ac") +
  geom_segment(aes(x = date_median, xend = date_median, y = m_smt_lower, yend = m_smt_upper),
               alpha = 1,
               col = "#0868ac") +
  geom_pointrange(data = results_byage %>% filter(type == "community", sample == "general") %>% filter(round > 0),
                  aes(x = date_mean, y = m_obs, ymin = m_obs_lower, ymax = m_obs_upper, size = n_part),
                  pch = 1,
                  alpha = 0.6,
                  col = "#DF65B0") +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(c(0, 0.05))) +
  scale_size_area(max_size = 1) +
  #scale_alpha_manual(values = c(0.2, rep(0.5, 10))) +
  coord_cartesian(xlim = c(as.Date("2020-03-16"), as.Date("2023-07-01"))) +
  labs(x = NULL,
       y = "Average number of community contacts pppd",
       size = "Number of participants") +
  theme_light() +
  theme(strip.background = element_blank(),
        strip.text = element_text(colour = 1, size = 11),
        legend.position = c(0.95, 0.05),
        legend.justification = c(1, 0)) +
  facet_wrap(facets = vars(part_age_group)) +
  inset_element(inset, left = 0.53, right = 0.75, bottom = 0.05, top = 0.23)

ggsave(filename = "./figures/Contacts_byagegroup.png", height = 6, width = 10, dpi = 300)


# Summary results by round (with bias-corrected bootstrap intervals)
results_fullpop <- results %>% 
  filter(type == "community") %>%
  select(sample, type, round, part_age_group, cnt_age_group, starts_with(c("m_smt.", "m_obs."))) %>% 
  pivot_longer(cols = starts_with(c("m_smt.", "m_obs.")), 
               names_to = c( ".value", "i"), 
               names_pattern = "(m_smt|m_obs)(.*)") %>%
  group_by(sample, type, round, part_age_group, i) %>% 
  summarise(m_smt_bs = sum(m_smt),
            m_obs_bs = sum(m_obs)) %>% 
  left_join(results %>% 
              group_by(sample, type, round, part_age_group) %>% 
              summarise(m_smt = sum(m_smt),
                        m_obs = sum(m_obs))) %>% 
  left_join(population %>% 
              group_by(round, age_group) %>% 
              summarise(frac_pop = sum(frac_pop)),
            by = c("part_age_group" = "age_group", "round" = "round")) %>% 
  group_by(sample, type, round, i) %>% 
  summarise(m_smt = sum(m_smt*frac_pop),
            m_smt_bs = sum(m_smt_bs*frac_pop),
            m_obs = sum(m_obs*frac_pop),
            m_obs_bs = sum(m_obs_bs*frac_pop)) %>%
  group_by(sample, type, round) %>% 
  reframe(m_smt = mean(m_smt),
          m_smt_0.5 = median(m_smt_bs),
          m_smt_var = var(m_smt_bs),
          m_obs = mean(m_obs),
          m_obs_0.5 = median(m_obs_bs),
          limit = c("lower", "upper"),
          m_smt_ = biascorrected_boot_ci(mean(m_smt), m_smt_bs),
          m_obs_ = biascorrected_boot_ci(mean(m_obs), m_obs_bs)) %>%
  pivot_wider(names_from = limit, values_from = ends_with("_"), names_glue = "{.value}{limit}",) %>%
  left_join(round_dates)

saveRDS(results_fullpop, "./results/results_fullpop.rds")


# SES & MED Summary results by round (with bias-corrected bootstrap intervals)

results_strat <- results_fullpop %>% 
  filter(sample != "general") %>% 
  mutate(sample2 = case_when(grepl(sample, pattern = "education") ~ "Education level",
                             TRUE ~ "Medical risk group"),
         sample2 = factor(sample2, levels = c("Medical risk group", "Education level"))) %>% 
  mutate(sample = case_when(grepl(sample, pattern = "low") ~ "Low",
                            grepl(sample, pattern = "high") ~ "High",
                            TRUE ~ "Medium")) %>% 
  mutate(sample = factor(sample, levels = c("Low", "Medium", "High"))) 



ggplot(data = results_strat %>% filter(round > 0),
       # plot m_obs variables (because subgroups do not necessarily mix reciprocally within group)
       # here m_smt = m_obs (because of symmetrisation method)
       aes(x = round, y = m_obs, ymin = m_obs_lower, ymax = m_obs_upper, col = sample)) +
  geom_segment(data = results_strat %>% filter(round == 0) %>% mutate(Baseline = TRUE),
               aes(x = 0, xend = 11, yend = m_obs, lty = Baseline)) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  scale_x_continuous(
    breaks = 1:10,
    minor_breaks = 1:10) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(c(0, 0.02))) +
  scale_color_manual(values = c('#bae4bc','#7bccc4','#2b8cbe')) +
  scale_fill_manual(values = c('#bae4bc','#7bccc4','#2b8cbe')) +
  scale_linetype_manual(values = 2,
                        labels = c("")) +
  coord_cartesian(xlim = c(0.5, 10.5)) +
  labs(x = "Survey round",
       y = "Average number of community contacts pppd\n(if full population consisted of a single stratum)",
       color = "Stratum") +
  theme_light() +
  theme(strip.background = element_blank(),
        strip.text = element_text(colour = 1, size = 12)) +
  facet_wrap(facets = vars(sample2),
             ncol = 1)

ggsave(filename = "./figures/Contacts_edu_med.png", height = 7, width = 7, dpi = 300)


