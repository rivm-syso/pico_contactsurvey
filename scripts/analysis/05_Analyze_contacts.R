################################################################################
#
# Analyze contacts
# - sample weights based on population distribution of age group and gender
# - sample weights based on weekend/weekday only for community contacts
# - bootstrapping within participant age group
# - analyze general, medical risk groups and education levels separately
# 
################################################################################

input <- expand_grid(type = c("community", "home"),
                     sample = "general") %>% 
  add_row(type = "community", sample = c("risk_high", "risk_low", "education_high", "education_medium", "education_low")) %>% 
  mutate(i = as.character(1:n()),
         weighted_age = TRUE,
         weighted_gender = TRUE,
         weighted_weekend = (type == "community")) 


results <- pmap(.l = input %>% map(as_vector),
     .f = function(type, sample, weighted_age, weighted_weekend, weighted_gender, input) 
       suppressMessages(
       analyze_contacts_n(part = participants,
                          cont = contacts,
                          pop = population,
                          r = NULL,
                          type = type,
                          sample = sample,
                          weighted_age = weighted_age,
                          weighted_gender = weighted_gender,
                          weighted_weekend = weighted_weekend,
                          n = 1000))) %>% 
  bind_rows(.id = "i") %>% 
  full_join(input) %>% 
  select(-i)

saveRDS(results, "./results/results_contactanalysis.rds")



