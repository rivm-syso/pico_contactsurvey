################################################################################
#
# Construct contacts:
# - Household contacts from household composition (i.e. not reported contacts!)
# - Check whether number of household members agrees with reported household size
# - Community contacts from reported contacts
# - Combine household and community contacts
# 
################################################################################

# Household contacts

contacts_home <- data %>% 
  select(participant_id, part_id, pico, part_age, part_gender, matches("householdage|householdgnd")) %>% 
  pivot_longer(cols = grep(names(.), pattern = "householdage|householdgnd"), 
             names_to = c( ".value", "hh_member"), 
             names_pattern = "(householdage|householdgnd)([0-9]+)") %>%
  ungroup() %>% 
  filter(!(is.na(householdage))) %>% 
  # in p3 (= pico 0) and pico 1 the full household composition was reported: participant needs to be omitted
  # (would also omit true household members of same age and gender as participant)
  mutate(householdgnd = if_else(householdgnd == 1, "M", "F", NA_character_)) %>% 
  filter(!(pico <= 1 & part_age == householdage & part_gender == householdgnd))


# add counted hh_size to data
data <- data %>% 
  full_join(
    contacts_home %>% 
      group_by(part_id) %>% 
      summarise(hh_size = n() + 1)) %>% 
  replace_na(replace = list(hh_size = 1))


# 2286 participant-pico instances where reported household size does not agree with the number of household members
# mainly in p3 (= pico 0) and pico10
# leave as is
data %>% 
  filter(hh_size_reported != hh_size) %>%
  group_by(pico) %>% 
  count 


# Community contacts

contacts_comm <- data %>%
  select(part_id, pico, starts_with("cont")) %>% 
  pivot_longer(cols = starts_with("cont"), 
               names_to = c("agegroup", ".value"), 
               #names_prefix = "cont", 
               #values_to = "bla",
               names_pattern = "cont(.*)(jaarman|jaarvrouw|dichtbij|dichtbescherm|dichtonbescherm|ver)") %>% 
  pivot_longer(cols = c("jaarman", "jaarvrouw", "dichtbij", "dichtbescherm", "dichtonbescherm", "ver"),
               names_to = "type",
               values_to = "n") %>% 
  filter(!is.na(n)) %>%
  uncount(weights = n) %>% 
  mutate(cnt_gender = case_when(pico %in% c(0, 1, 8) & type == "jaarman" ~ "M",
                                pico %in% c(0, 1, 8) & type == "jaarvrouw" ~ "F",
                                TRUE ~ NA_character_),
         cnt_type = case_when(type == "dichtbij" ~ 1,
                              type == "dichtonbescherm" ~ 2,
                              type == "dichtbescherm" ~ 3,
                              type == "ver" ~ 4,
                              TRUE ~ NA))


# Combine home and community contacts

contacts <- bind_rows(contacts_home %>% 
            rename(cnt_gender = householdgnd,
                   cnt_age = householdage) %>% 
            mutate(hh_member = TRUE,
                   agegroup = cut(cnt_age, breaks = c(seq(0, 15, 5), seq(20, 90, 10), Inf), right = FALSE, include.lowest = TRUE, 
                                  labels = c("00tot04", "05tot09", "10tot14", "15tot19", "20tot29", "30tot39", "40tot49", "50tot59", "60tot69", "70tot79", "80tot89", "90tot120"))) %>% 
            select(part_id, hh_member, agegroup, cnt_gender, cnt_age),
          contacts_comm %>% 
            mutate(hh_member = FALSE) %>% 
            select(part_id, hh_member, agegroup, cnt_gender, cnt_type)
) %>% 
  # split agegroup in range (min - max)
  mutate(agegroup = if_else(agegroup == "90plus", "90tot120", agegroup)) %>% 
  mutate(cnt_age_est_min = as.integer(str_split_i(agegroup, pattern = "tot", i = 1)),
         cnt_age_est_max = as.integer(str_split_i(agegroup, pattern = "tot", i = 2))) %>% 
  group_by(part_id) %>% 
  # add cnt_id per part_id (note that repeated contacts with household members are not taken into account)
  mutate(cnt_id = 10000*part_id + 1:n())


rm(contacts_home)
rm(contacts_comm)


