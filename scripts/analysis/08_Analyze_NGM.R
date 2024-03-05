################################################################################
#
# Analyze next generation matrix
# - maximum eigenvalue of next generation matrix (NGM) is proportional to R0
# - total number of contacts (community and household) per participant age group
#   and contact age group multiplied by relative susceptibility and infectiousness
#   is a proxy for the NGM 
# - compute maximum eigenvalue for each round and compare to baseline
# - make figure
# 
################################################################################

results <- readRDS("./results/results_contactanalysis.rds")


# Compute max eigenvalue of NGM for sum of community and household contacts of general sample
# (with bias-corrected bootstrap intervals)

relsusinf <- bind_rows("equal" = tibble(age_group = 1:10,
                                        relsus = 1,
                                        relinf = 1),
                       "Franco et al. (2022) A" = tibble(age_group = 1:10,
                                          relsus = c(0.182, 0.550, 0.603, 1, 1.172, 1.009, 0.880, 0.869, 0.846, 0.805),
                                          relinf = c(0.54, 0.55, 0.56, 0.59, 0.7, 0.76, 0.9, 0.99, 0.99, 0.99)),
                       "Franco et al. (2022) B" = tibble(age_group = 1:10,
                                          relsus = c(0.4, 0.39, 0.38, 0.79, 0.86, 0.8, 0.82, 0.88, 0.74, 0.74),
                                          relinf = c(0.346, 0.892, 1.310, 1, 0.645, 3.783, 1.320, 0.266, 1.277, 0.099)),
                       "Klinkenberg et al. (2023)" = tibble(age_group = 1:10,
                                              relsus = c(1, 1, 3.05, 5.75, 3.54, 3.71, 4.36, 5.69, 5.32, 7.21),
                                              relinf = c(1, 1, 3.05, 5.75, 3.54, 3.71, 4.36, 5.69, 5.32, 7.21)),
                       "Zhang et al. (2020)" = tibble(age_group = 1:10,
                                        relsus = c(0.34, 0.34, (1+0.34)/2, 1, 1, 1, 1, (1+1.47)/2, 1.47, 1.47),
                                        relinf = 1),
                       
                       .id = "reference")


results_NGM <- tibble()

for(i in unique(relsusinf$reference)) {
  
  relsus <- relsusinf %>% filter(reference == i) %>% pull(relsus)
  relinf <- relsusinf %>% filter(reference == i) %>% pull(relinf)
  
  tmp <- results %>% 
    filter(sample == "general") %>%
    select(type, round, part_age_group, cnt_age_group, starts_with("m_smt")) %>% 
    pivot_longer(cols = starts_with("m_smt"), 
                 names_to = c( ".value", "i"), 
                 names_pattern = "(m_smt)(.*)") %>%
    mutate(estimate = if_else(i == "", "default", "bs")) %>% 
    # no grouping by type to sum community and household contacts
    group_by(round, part_age_group, cnt_age_group, i, estimate) %>% 
    summarise(m_smt_bs = sum(m_smt)) %>% 
    #filter(estimate == "default") %>% # for testing
    group_by(round, i, estimate) %>% 
    arrange(round, i, estimate, part_age_group, cnt_age_group) %>% 
    summarise(mev = compute_maxeigenvalue(as_vector(m_smt_bs), relsus = relsus, relinf = relinf)) %>% 
    group_by(round, estimate) %>% 
    reframe(maxeigenvalue = mean(mev),
            limit = c("lower", "upper"),
            maxeigenvalue_ = biascorrected_boot_ci(maxeigenvalue, mev)) %>%  
    pivot_wider(names_from = limit, values_from = ends_with("_"), names_sep = "", names_prefix = "maxeigenvalue_") %>% 
    filter(estimate == "bs") %>% 
    select(-estimate)
  
  results_NGM <- bind_rows(results_NGM, tmp %>% mutate(reference = i))
  
}

saveRDS(results_NGM, "./results/results_NGM.rds")

results_NGM %>% 
  # comment next line out to obtain SI figure (Suppl S3)
  filter(reference == "equal" | reference == "Zhang et al. (2020)") %>% 
  left_join(round_dates) %>% 
  group_by(reference) %>% 
  mutate(mev_0 = maxeigenvalue[round == 0],
         ratio = maxeigenvalue/mev_0,
         ratio_lower = maxeigenvalue_lower/mev_0,
         ratio_upper = maxeigenvalue_upper/mev_0) %>%  
  ggplot(aes(col = reference, fill = reference)) +
  geom_rect(data = . %>% filter(round == 0, reference == "equal"),
            aes(xmin = date_min, xmax = date_max, ymin = ratio_lower, ymax = ratio_upper),
            alpha = 0.5,
            col = NA,
            fill = "darkgrey") +
  geom_segment(data = . %>% filter(round == 0, reference == "equal"),
               aes(x = date_min, xend = date_max, y = ratio, yend = ratio),
               alpha = 1,
               col = "darkgrey") +
  geom_rect(data = . %>% filter(round > 0),
            aes(xmin = date_min, xmax = date_max, ymin = ratio_lower, ymax = ratio_upper),
            alpha = 0.5,
            col = NA) +
  geom_segment(data = . %>% filter(round > 0),
               aes(x = date_min, xend = date_max, y = ratio, yend = ratio),
               alpha = 1) +
  geom_segment(data = . %>% filter(round > 0),
               aes(x = date_median, xend = date_median, y = ratio_lower, yend = ratio_upper),
               alpha = 1) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(c(0, 0.05))) +
  scale_color_manual(values = c("equal" = "#0868ac", 
                                "Franco et al. (2022) A" =  "#D4B9DA",
                                "Franco et al. (2022) B" = "#C994C7", 
                                "Klinkenberg et al. (2023)" = "#DF65B0", 
                                "Zhang et al. (2020)" = "#7bccc4")) +
  scale_fill_manual(values = c("equal" = "#0868ac", 
                               "Franco et al. (2022) A" =  "#D4B9DA",
                               "Franco et al. (2022) B" = "#C994C7", 
                               "Klinkenberg et al. (2023)" = "#DF65B0", 
                               "Zhang et al. (2020)" = "#7bccc4")) +
  coord_cartesian(xlim = c(as.Date("2020-03-16"), as.Date("2023-07-01"))) +
  labs(x = NULL,
       y =  "Transmission potential compared to baseline value",
       color = "Relative susceptibility\nand infectiousness\nover age groups",
       fill = "Relative susceptibility\nand infectiousness\nover age groups") +
  theme_light() +
  theme(legend.position = c(0.95, 0.05),
        legend.justification = c(1, 0))


ggsave(filename = "./figures/Ratio_R0.png", height = 5, width = 8, dpi = 300)

