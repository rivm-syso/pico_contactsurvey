# Analyze contacts of contact survey
# (usually called by analyze_contacts_n, but can be used independently)
#   prt: participant data
#   cnt: contact data
#   pop: population data
#   w_gender: boolean to distribute population over genders
#   w_weekend: boolean to distribute population over weekend/weekday

analyze_contacts <- function(prt, cnt, pop, w_age, w_gender, w_weekend) {
  
  # determine population fractions, depending on desired level of resolution for weighting
  tmp_pop <- pop  %>% 
    rename(part_gender = sex) %>% 
    {if(w_age) rename(., part_age = age) else rename(., part_age = age_group)} %>% 
    group_by(round, part_age, .drop = FALSE) %>% 
    {if(w_gender) group_by(., part_gender, .add = TRUE, .drop = FALSE) else .} %>% 
    summarise(frac_pop = sum(frac_pop)) %>% 
    {if(w_weekend) 
      expand_grid(., weekend = factor(c(TRUE, FALSE))) %>% 
        mutate(., frac_pop = if_else(weekend == TRUE, 2*frac_pop/7, 5*frac_pop/7)) else .} %>% 
    # add part_age_group
    {if(w_age) left_join(., pop %>% transmute(part_age = age, part_age_group = age_group) %>% distinct()) else mutate(., part_age_group = part_age)}
  

  # weights of participant population compared to general population
  weights <- prt %>%
    {if(w_age) . else mutate(., part_age = part_age_group)} %>% 
    group_by(round, part_age, .drop = FALSE) %>% 
    {if(w_gender) group_by(., part_gender, .add = TRUE, .drop = FALSE) else .} %>% 
    {if(w_weekend) group_by(., weekend, .add = TRUE, .drop = FALSE) else .} %>% 
    count(name = "n_part") %>% 
    group_by(round) %>% 
    mutate(frac_part = n_part/sum(n_part)) %>% 
    left_join(tmp_pop) %>% 
    mutate(weight = if_else(n_part == 0, 0, frac_pop/frac_part))
  

  tmp <- prt %>% 
    # join with contacts (many-to-many needed for bootstrapped participant populations 
    # that can contain duplicate part_ids)
    # and count contacts per group
    left_join(cnt,
              relationship = "many-to-many") %>% 
    {if(w_age) . else mutate(., part_age = part_age_group)} %>% 
    group_by(round, part_age, cnt_age_group, .drop = FALSE) %>% 
    {if(w_gender) group_by(., part_gender, .add = TRUE, .drop = FALSE) else .} %>% 
    {if(w_weekend) group_by(., weekend, .add = TRUE, .drop = FALSE) else .} %>% 
    count(name = "n_cont") %>%     
    # filter out NA's in cnt_age group (caused by participants without any contacts)
    # not any sooner because it can cause NaN's when entire age group does not have any contacts (does not occur in practice)
    filter(!is.na(cnt_age_group)) %>%
    # join with weights for participant age group and count contacts per age group combination
    # (summing over part_gender, either weighted or not)
    left_join(weights) %>% 
    group_by(round, part_age_group, cnt_age_group) %>% 
    summarise(n_cont_raw = sum(n_cont),
              n_cont = sum(n_part)*sum(weight*n_cont)/sum(weight*n_part),
              n_part = sum(n_part)) %>% 
    # join with population fraction for contact age group
    left_join(tmp_pop %>% 
                group_by(round, part_age_group) %>% 
                summarise(frac_pop = sum(frac_pop)) %>% 
                # divide by mean(frac_pop) to help interpretation of c-matrix
                # (not necessary, it scales c_obs with the number of age groups)
                mutate(frac_pop = frac_pop/mean(frac_pop)),
              by = c("cnt_age_group" = "part_age_group", "round" = "round")) %>% 
    # calculate m_obs: mean number of contacts per participant
    # and c_obs: mean number of contacts per participant if population were equally distributed over age groups
    mutate(m_obs = if_else(n_part > 0, n_cont/n_part, 0),
           c_obs = m_obs/frac_pop) 
  

  # symmetrize c-matrix by taking mean of cross-diagonal elements
  tmp <- tmp %>% 
    full_join(tmp %>% 
                rename(part_age_group = cnt_age_group,
                       cnt_age_group = part_age_group,
                       c_smt = c_obs) %>% 
                select(round, part_age_group, cnt_age_group, c_smt)) %>% 
    mutate(c_smt = rowMeans(cbind(c_obs, c_smt), na.rm = TRUE),
           m_smt = c_smt*frac_pop)
  
  return(tmp)
}
