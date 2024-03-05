

theme_strip <- theme_minimal()+
  theme(axis.text = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()
  )

inset <- tibble(type = c("PiCo survey", "Baseline survey"),
                x = 0,
                y = 1:2) %>% 
  ggplot(aes(col = type, fill = type)) +
  geom_rect(aes(xmin = x, xmax = x + 3, ymin = y - 0.3, ymax = y + 0.3),
            alpha = 0.5,
            col = NA) +
  geom_segment(aes(x = x, xend = x + 3, y = y, yend = y),
               alpha = 1) +
  geom_segment(aes(x = 1, xend = 1, y = 0.7, yend = 1.3),
               alpha = 1,
               col = "#0868ac") +
  geom_text(aes(x = x + 4, y = y, label = type),
            col = 1,
            hjust = 0) +
  scale_x_continuous(limits = c(0, 30)) +
  scale_fill_manual(values = c("darkgrey", "#0868ac")) +
  scale_colour_manual(values = c("darkgrey", "#0868ac")) +
  guides(col = "none",
         fill = "none") +
  theme_strip
