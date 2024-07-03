################################################################################
#
# Analyze contact type
# - only for survey rounds 2-7, 9, 10
# - per survey round weighted by age group distribution 
#   (to correct for confounding by age)
# - overall figure
# - figure by medical risk group (Suppl S5)
# 
################################################################################

contacts %>% 
  left_join(participants) %>% 
  filter(!hh_member) %>% 
  filter(!is.na(cnt_id)) %>% 
  filter(round > 0) %>% 
  mutate(cnt_type = factor(cnt_type, 
                           levels = c(4, 3, 1, 2),
                           labels = c("distant (> 1.5m)", "close protected", "close", "close unprotected"))) %>% 
  group_by(round, part_age_group, part_id, cnt_type, .drop = FALSE) %>% 
  count() %>% 
  group_by(round, part_id, part_age_group, .drop = FALSE) %>% 
  mutate(n_cont = sum(n),
         frac_type = n/n_cont) %>% 
  group_by(round, part_age_group, cnt_type, .drop = FALSE) %>% 
  summarise(frac_type = mean(frac_type)) %>% 
  filter(!is.na(frac_type)) %>%
  left_join(population %>% 
              group_by(round, age_group) %>% 
              summarise(frac_pop = sum(frac_pop)),
            by = c("part_age_group" = "age_group", "round" = "round")) %>% 
  group_by(round, cnt_type, .drop = FALSE) %>% 
  summarise(frac_type = sum(frac_type*frac_pop)/sum(frac_pop)) %>% 

  ggplot(aes(x = factor(round), y = frac_type, fill = factor(cnt_type))) +
  geom_bar(stat = "identity",
           position = "stack") +
  scale_y_continuous(expand = expansion(c(0,0))) +
  scale_fill_manual(values = rev(c("#42145f", "#750A60", "#a90061", "#f092cd")),
                    na.value = "white") +
  labs(x = "Survey round",
       y = "Fraction of number of contacts per participant",
       fill = "Contact type") +
  theme_minimal()

# ggsave(filename = "./figures/Contacttypes.png", height = 4, width = 7, dpi = 300, bg = "white")


contacts %>% 
  left_join(participants) %>% 
  filter(!hh_member) %>% 
  filter(!is.na(cnt_id)) %>% 
  filter(round > 0) %>% 
  mutate(cnt_type = factor(cnt_type, 
                           levels = c(4, 3, 1, 2),
                           labels = c("distant (> 1.5m)", "close protected", "close", "close unprotected"))) %>% 
  group_by(round, medical_risk, part_age_group, part_id, cnt_type, .drop = FALSE) %>% 
  count() %>% 
  #filter(!is.na(cnt_type)) %>% 
  group_by(round, medical_risk, part_age_group, part_id, .drop = FALSE) %>% 
  mutate(n_cont = sum(n),
         frac_type = n/n_cont) %>% 
  group_by(round, medical_risk, part_age_group, cnt_type, .drop = FALSE) %>% 
  summarise(frac_type = mean(frac_type)) %>% 
  filter(!is.na(medical_risk)) %>% 
  filter(!is.na(frac_type)) %>%
  left_join(population %>% 
              group_by(round, age_group) %>% 
              summarise(frac_pop = sum(frac_pop)),
            by = c("part_age_group" = "age_group", "round" = "round")) %>% 
  group_by(round, medical_risk, cnt_type, .drop = FALSE) %>% 
  summarise(frac_type = sum(frac_type*frac_pop)/sum(frac_pop)) %>% 
  
  ggplot(aes(x = factor(round), y = frac_type, fill = factor(cnt_type))) +
  geom_bar(stat = "identity",
           position = "stack") +
  scale_y_continuous(expand = expansion(c(0,0))) +
  scale_fill_manual(values = rev(c("#42145f", "#750A60", "#a90061", "#f092cd")),
                    na.value = "white") +
  labs(x = "Survey round",
       y = "Fraction of number of contacts per participant",
       fill = "Contact type") +
  theme_minimal() +
  facet_wrap(facets = vars(medical_risk))
  
ggsave(filename = "./figures/Contacttypes_medrisk.png", height = 4, width = 7, dpi = 300, bg = "white")

