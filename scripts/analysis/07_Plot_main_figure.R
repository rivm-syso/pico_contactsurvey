################################################################################
#
# Plot main results figure for full population
# - with number of weekly hospital admissions
# - with Oxford stringency index
# 
################################################################################

results_fullpop <- readRDS("./results/results_fullpop.rds")


# Additional data

#https://github.com/OxCGRT
OxfordSI <- read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-dataset/main/data/OxCGRT_compact_national_v1.csv")

# https://data.rivm.nl/covid-19/
# citeren naar Geubels?
hosp_data <- bind_rows(
  read_csv2("https://data.rivm.nl/covid-19/COVID-19_ziekenhuisopnames_tm_03102021.csv"),
  read_csv2("https://data.rivm.nl/covid-19/COVID-19_ziekenhuisopnames.csv"))


h_data <- hosp_data %>% 
  mutate(week = ISOweek2date(paste0(ISOweek(Date_of_statistics), "-4"))) %>% 
  group_by(week) %>% 
  summarise(n_hosp = sum(Hospital_admission))

p_hosp <- h_data %>% 
  ggplot(aes(x = week, y = n_hosp)) +
  geom_bar(stat = "identity",
           width = 7) +
  geom_hline(yintercept = 0,
             lwd = 0.2) +
  scale_x_date(limits = c(as.Date("2020-01-01"), as.Date("2023-06-30")),
               expand = expansion(c(0, 0))) +
  labs(title = "Weekly hospital admissions") +
  theme_strip +
  theme(plot.title = element_text( hjust=1, vjust=0.5, size = 10, margin=margin(t=0,b=-20)))


# group SI by period, because plotting with tiles by day gives artefacts in saved plots
SI_data <- OxfordSI |> 
  filter(CountryName == "Netherlands") |> 
  mutate(date = as.Date(as.character(Date), format = "%Y%m%d")) |> 
  right_join(tibble(date = seq.Date(as.Date("2020-01-01"), as.Date("2023-06-30"), by = "day"))) %>% 
  select(date, StringencyIndex_Average) |> 
  mutate(lagSI = lag(StringencyIndex_Average, default = 0),
         period = cumsum(lagSI != StringencyIndex_Average)) |> 
  group_by(period) |> 
  reframe(date_min = min(date),
          date_max = max(date),
          StringencyIndex_Average = mean(StringencyIndex_Average)) 

# plot stringency index from start to end time series of effectiveness data, with dates on x-axis and legend
p_SI <- ggplot(data = SI_data,
                  aes(xmin = date_min, xmax = date_max, ymin = 0, ymax = 1, fill = StringencyIndex_Average, col = StringencyIndex_Average))+
  geom_rect() +
  scale_x_date(#date_breaks = "6 years",
    date_labels = "%Y",
    expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu")),
                       na.value = "white")+
  scale_color_gradientn(colors = rev(brewer.pal(11, "RdBu")),
                       na.value = "white")+
  guides(fill = "none",
         color = "none") +
  labs(title = "Stringency Index") +
  theme_strip +
  theme(plot.title = element_text( hjust=1, vjust=0.5, size = 10, margin=margin(t=0,b=-20)))


p_main <- ggplot(data = results_fullpop %>% filter(type == "community", sample == "general", round > 0)) +
  geom_rect(data = results_fullpop %>% filter(type == "community", sample == "general", round == 0),
            aes(xmin = as.Date("2020-01-01"), xmax = as.Date("2023-06-30"), ymin = m_smt_lower, ymax = m_smt_upper),
            alpha = 0.5,
            fill = "darkgrey",
            col = NA) +
  geom_segment(data = results_fullpop %>% filter(type == "community", sample == "general", round == 0),
               aes(x = as.Date("2020-01-01"), xend = as.Date("2023-06-30"), y = m_smt, yend = m_smt),
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
  geom_text(aes(x = date_mean, y = m_smt, label = round),
            alpha = 1,
            col = "white") +
  scale_x_date(limits = c(as.Date("2020-01-01"), as.Date("2023-06-30")),
               expand = expansion(c(0, 0))) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(c(0, 0.05))) +
  labs(x = NULL,
       y = "Average number of community contacts pppd") +
  theme_light()


p_hosp + p_SI + p_main + 
  plot_layout(ncol = 1, heights = c(0.15, 0.05, 0.85)) + 
  inset_element(inset, left = 0.7, right = 0.9, bottom = 0.1, top = 0.3)

ggsave(filename = "./figures/Contacts_main.png", height = 6, width = 9, dpi = 300)

