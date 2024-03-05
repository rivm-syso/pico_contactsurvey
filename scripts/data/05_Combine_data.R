################################################################################
#
# Combine p3 and pico data
# - determine p3 participant id's via original S-numbers
# - give p3 participants that did not participate in pico an unused participant id
# - p3 as pico 0 
# 
################################################################################


# add participant_id by coupling with S-number
p3_data <- p3_data %>% 
  left_join(pico_data %>% 
              transmute(s_nummer2 = as.character(dn_extra_usn),
                        participant_id = participant_id,
                        dn_randomisatienr = dn_randomisatienr) %>% 
              distinct()) %>% 
  # add participant id for p3 participants that never participated in pico
  # (no sampling to make it reproducible)
  # mutate(participant_id = if_else(is.na(participant_id), sample(setdiff(10000:99999, original_participant_ids), size = nrow(.), replace = FALSE), participant_id)) %>% 
  arrange(s_nummer2) %>% 
  mutate(participant_id = if_else(is.na(participant_id), 10000 + 1:nrow(.), participant_id)) %>% 
  # p3 is baseline (pico = 0)
  mutate(pico = 0)


# variables both in p3 and pico data
common_names <- intersect(names(pico_data), names(p3_data))
# add variables that are only in p3 or pico data
additional_names <- c("monthlyincome", "householdage11", "householdage12", "householdgnd11", "householdgnd12", 
                      "gisterwerkschool", "seropos",
                      pico_data %>% select(starts_with("employ"), starts_with("coronavaccinatie"), starts_with("cont"), starts_with("workathome")) %>% names)


data <- bind_rows(
  p3_data %>% select(any_of(c(common_names, additional_names))),
  pico_data %>% select(any_of(c(common_names, additional_names)))
)


rm(common_names)
rm(additional_names)
rm(p3_data)
rm(pico_data)
