# Analyze contacts of contact survey
#   part: participant data
#   cont: contact data
#   pop: population data
#   r: survey round (if r = 0, all rounds are analyzed simultaneously)
#   type: "home" (or "community" by default)
#   sample: "LVC", "high-risk" or "low-risk" (or "general" by default)
#   weighted: boolean to use population weighting or not
#   n: number of bootstrap samples (if n = 0, no bootstrapping)

analyze_contacts_n <- function(part, 
                               cont, 
                               pop, 
                               r = NULL, 
                               type = "community", 
                               sample = "general", 
                               weighted_age, 
                               weighted_gender, 
                               weighted_weekend, 
                               n = 0) {
  
  part <- part %>% 
    # select rounds listed in r (if NULL analyze all rounds)
    {if(length(r) > 0) filter(., round %in% r) else .} %>% 
    # select participants with high or low medical risk (or all when sample is not defined)
    {if(sample == "risk_high") filter(., medical_risk == "High") else if(sample == "risk_low") filter(., medical_risk == "Low") else .} %>% 
    # select participants with low, middle or high education (or all when sample is not defined)
    {if(sample == "education_high") filter(., education_level == "High") else if(sample == "education_medium") filter(., education_level == "Medium") else if(sample == "education_low") filter(., education_level == "Low") else .} %>% 
    # select only participants with household composition when home contacts are analyzed
    {if(type == "home") filter(., participant_withhh) else .}
  
  cont <- cont %>% 
    {if(type == "home") filter(., hh_member) else filter(., !hh_member)} 
  
  pop <- pop %>% 
    {if(length(r) > 0) filter(., round %in% r) else .}
    
  res <- analyze_contacts(prt = part, cnt = cont, pop = pop, w_age = weighted_age, w_gender = weighted_gender, w_weekend = weighted_weekend)
  
  # take n bootstrapping samples and combine with results
  if(n > 0) {
    for(i in 1:n) {
      
      ran_part <- part %>% 
        group_by(round, part_age_group) %>% 
        transmute(part_id = part_id[sample(length(part_id), replace = TRUE)]) %>% 
        left_join(part)

      tmp <- analyze_contacts(prt = ran_part, cnt = cont, pop = pop, w_age = weighted_age, w_gender = weighted_gender, w_weekend = weighted_weekend)
      
      res <- res %>% 
        full_join(tmp %>% 
                    transmute(round = round,
                              part_age_group = part_age_group,
                              cnt_age_group = cnt_age_group,
                              !!paste0("c_obs.",i) := c_obs,
                              !!paste0("m_obs.",i) := m_obs,
                              !!paste0("c_smt.",i) := c_smt,
                              !!paste0("m_smt.",i) := m_smt))
      
    }
  }
  
  return(res)
}


