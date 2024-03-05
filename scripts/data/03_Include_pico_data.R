################################################################################
#
# Put data in wide format 
# Omit the rounds without filled-out questionnaire
# Exclude participants:
# - Select only pico rounds in which participants participated
# - Determine total number of contacts per pico-participant combination
# - Exclude pp combination when
#   * did you have contacts yesterday = NA  and number of contacts = 0
#   * did you have contacts yesterday = yes but number of contacts = 0 
#   (did you have contacts yesterday = no and number of contacts != 0 does not occur)
# - Resulting in:
#   * when contacts are given always included (regardless of did you have contacts yesterday)
#   * when no contacts are given only included when did you have contacts yesterday = no
# 
################################################################################

pico_data <- pico_data %>% 
  # omit variables testpos_r[0-9]_cat (denoting symptomatic or asymptomatic test)
  select(!ends_with("_cat")) %>% 
  # rename cases seropositive for infection
  rename(seropos_r1 = pico1_s1_pos_combi,
         seropos_r2 = pico2_S1_pos,
         seropos_r3 = pico3_s1_pos,
         seropos_r4 = pico4_pos_inf,
         seropos_r5 = pico5_pos_inf,
         seropos_r6 = pico6_pos_inf,
         seropos_r7 = pico7_pos_inf,
         seropos_r8 = pico8_pos_inf,
         seropos_r9 = pico9_pos_inf) %>% 
  pivot_longer(cols = matches("_r([0-9])"), 
               names_to = c( ".value", "pico"),
               names_sep = "_r") %>% 
  mutate(pico = as.integer(pico))


# 1130 participants (792 unique participants) have bloodsample result but no questionnaire
pico_data %>% filter(crf_label == "", !is.na(seropos)) %>% nrow
pico_data %>% filter(crf_label == "", !is.na(seropos)) %>% filter(!duplicated(participant_id)) %>% nrow

# 14282 participants (6844 unique participants) have questionnaire but no bloodsample result
pico_data %>% filter(crf_label != "", is.na(seropos)) %>% nrow
pico_data %>% filter(crf_label != "", is.na(seropos)) %>% filter(!duplicated(participant_id)) %>% nrow


# only valid pico-participant combinations (61309)
pico_data <- pico_data %>% 
  filter(crf_label != "")

# check combinations of `did you have contacts yesterday` and number of contacts reported
pico_data %>%
  # indicator whether participant had contacts in specific age group ignored
  select(-matches("cont([0-9]+)tot([0-9]+)$"), -"cont90plus") %>% 
  # determine tot number of contacts
  mutate(n_cont = rowSums(across(starts_with("cont")), na.rm = TRUE)) %>% 
  mutate(nocontacts = (n_cont == 0)) %>% 
  group_by(dlnrgistrcont, nocontacts) %>% 
  count


# exclude participants without contacts that did not indicate not having had any contacts (499) and
# exclude participants that indicated having had contacts, but did not report any (163)
pico_data <- pico_data %>%
  # indicator whether participant had contacts in specific age group ignored
  select(-matches("cont([0-9]+)tot([0-9]+)$"), -"cont90plus") %>% 
  # determine tot number of contacts
  mutate(n_cont = rowSums(across(starts_with("cont")), na.rm = TRUE)) %>% 
  # exclude did you have contacts yesterday = NA  and number of contacts = 0
  filter(!(is.na(dlnrgistrcont) & n_cont == 0)) %>% 
  # exlude did you have contacts yesterday = yes but number of contacts = 0 
  filter(!(dlnrgistrcont == 1 & n_cont == 0))


# harmonize variable names and labels
pico_data <- pico_data %>% 
  rename(part_gender = Geslacht,
         part_age = lftyear,
         degree_urbanisation = Stedelijkheid,
         part_weight = gewicht,
         part_height = lengte,
         fillindate = algdatuminvul,
         workathome = thuiswafgweekdlnr,
         workathome_beforecovid = thuiswvoorcordlnr) %>% 
  mutate(hh_size_reported = hoevhuisgedlnr + 1,
         flushot = case_when(vacgriep == 1 ~ TRUE,
                             vacgriep == 2 ~ FALSE,
                             TRUE ~ NA)) %>%
  # correct contactday = 0 to NA (4 instances)
  mutate(contactday = if_else(welkedaggist == 0, NA_real_, welkedaggist)) %>% 
  filter(region != 6) %>% # exclude LVC participants
  mutate(
    # correct aand = 0 to NA (1 instance)
    # aand = if_else(aand == 0, NA, aand),
    # disease = coalesce(aand_combi, aand),
    diseaseastma = aandastma,
    diseasediabetes = pmin(aanddiabetype, aanddiabetype_combi, aanddiabetype1, aanddiabetype2, aanddiabetypeonb, na.rm = TRUE),
    diseaselung = pmin(aandlongtot_combi, aandlong, na.rm = TRUE),
    diseaseliver = aandlever,
    diseaseimmune = coalesce(aandimmuunstoor_combi, aandimmuunstoor),
    diseasecancer = pmin(aandkanker, aandkanker_combi, aandkankerbehandelnu, aandkankeronbehandel, na.rm = TRUE),
    diseaserheumati = NA,
    diseasespleen = aandgeenmilt,
    diseaserenal = coalesce(aandnier_combi, aandnier),
    diseasevascular = coalesce(aandhartvaat_combi, aandhartvaat),
    diseaseneurolog = aandneurologisch,
    diseaseorgan = aandtransplantatie) %>% 
  # for participants < 15 years, education is highest education of parents (except when still in school, i.e. level 9)
  # 1 = No completed education (primary school, not completed)
  # 2 = Primary education (primary school, special primary school)
  # 3 = Junior or pre-vocational secondary education (such as junior secondary technical school (LTS), junior commercial education (LEAO), junior domestic science and vocational education (LHNO), junior secondary vocational education (LBO), and the basic vocational track (BB), advanced vocational track (KB), combined track (GL) of pre-vocational secondary education (VMBO))
  # 4 = Junior general secondary education (such as junior general secondary education (MAVO), higher elementary education (MULO), advanced elementary education (ULO), basic vocational education (MBO-kort), the theoretical track of pre-vocational secondary education (VMBO-TL))
  # 5 = Senior secondary vocational education and work-based education (such as senior secondary vocational education (MBO-lang), senior secondary technical school (MTS), upper secondary vocational education in business and administration (MEAO), school-based pathway (BOL), work-based pathway (BBL), and medical assistant training (INAS))
  # 6 = Senior general secondary education and pre-university education (such as senior general secondary education (HAVO), pre-university education (VWO) without Latin or Greek (Atheneum) or with Latin and Greek (Gymnasium), secondary modern school (HBS), girls' secondary school (MMS))
  # 7 = Higher professional education (such as school for higher professional education (HBO), institute of technology (HTS), school for higher education in economics and management (HEAO), Bachelor degree))
  # 8 = University level education
  # 9 = Have not finished education yet
  mutate(opleidingdlnr = coalesce(opleiding_combi, opleidingdlnr),
         opleidingvrzg1 = if_else(opleidingvrzg1 > 8, NA_integer_, opleidingvrzg1),
         opleidingvrzg2 = if_else(opleidingvrzg2 > 8, NA_integer_, opleidingvrzg2),
         opleidingvrzg = pmax(opleidingvrzg1, opleidingvrzg2, na.rm = TRUE),
         education = coalesce(opleidingdlnr, opleidingvrzg)) %>% 
  # in pico 1 and 2 employment definitions are different from pico 3 onwards: set to NA and later infer
  # (this will only affect 12 participants)
  # from pico 3 onwards: In welke werkveld bent u werkzaam?
  # 1=(Gezondheids)zorg
  # 2=Ander werk dan gezondheidszorg met lichamelijk contact met klanten (o.a. kapper, schoonheidsspecialiste, pedi-/manicure, masseur, tatoeëerder/piercer, rijinstructeur)
  # 3=Landbouw (o.a. akkerbouw, kassenteelt, (champignon)kwekerijen)
  # 4=Groenvoorziening (o.a. hoveniers, landschapsverzorging, plantsoenendienst, bosbouwers)
  # 5=Werk met dieren of dierlijke producten (o.a. melk-, pluimvee-, varkenshouderijen, viskwekerijen, jagers, boswachters, ruimers, vleesverwerkende industrie)
  # 6=Afvalverwerking
  # 7=Schoonmaakbranche
  # 8=Seksindustrie
  # 9=Buitenland (zakenreizigers of expats)
  # 10=Transport (o.a. luchtvaart/haven, wegtransport en OV, scheepsvaart)
  # 11=Productie/fabriek
  # 12=Kinderopvang (0-4 jaar)
  # 13=Basisschoolonderwijs of BSO
  # 14=Middelbaar en hoger/universitair onderwijs
  # 15=Horeca (o.a. bediening, keuken, management)
  # 16=Winkel (o.a. retail en supermarkten)
  # 17=Kantoor
  # 18=Noodzakelijke overheidsprocessen (Rijk, provincie en gemeente)
  # 19=Andere sector, vul vraag 6a in
  # 30=Niet van toepassing, ik werk niet (meer) (ga verder bij vraag 8)
  # 31=Niet van toepassing, ik ben scholier of student (ga verder bij vraag 8)
mutate(employment = if_else(pico >= 3, werksectdlnr, NA),
       employment_caretaker1 = if_else(pico >= 3, werksectvrzg1, NA),
       employment_caretaker2 = if_else(pico >= 3, werksectvrzg2, NA)) %>% 
  # set !participant_withhh when hh composition (excl participant) is not reported and hh size is not 1
  mutate(huishoud_grootte = rowSums(!is.na(across(starts_with("leeftijdhg"))), na.rm = TRUE),
         participant_withhh = !(huishoud_grootte == 0 & (hh_size_reported != 1 | is.na(hh_size_reported))))

# rename multiple columns
names(pico_data) <- gsub(names(pico_data), pattern = "leeftijdhg", replacement = "householdage")
names(pico_data) <- gsub(names(pico_data), pattern = "geslachthg", replacement = "householdgnd")



