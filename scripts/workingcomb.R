library(tidyverse)

setwd("/projects/b1059/projects/JB/2024_poultrynecropsy")

data <- read.csv("data/202305_necropsy.csv")

untrt <- data %>%
  dplyr::filter(condition=="control")


meep <- ggplot(untrt, aes(x = Parasites_Recovered, y = Intestine_weight_g)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(x = "Parasites recovered", y = "Intestine weight (g)") +
  theme_cowplot(12) +
  theme(
    axis.text.x = element_text(size = 12, hjust = 1, vjust = 1, margin = unit(c(0, 0, 0, 0), units = "in")),
    plot.background = element_rect(fill = "white"),
    strip.background = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.text = element_text(face = "bold", margin = margin(t = 10, b = 10))
  ) +
  facet_wrap(~species, scales = "fixed") +
  theme(strip.background = element_rect(fill = "grey80", color = "grey80")) + 
  stat_cor(aes(label = after_stat(rr.label)), color = "black", geom = "label", 
           method = "pearson", position = position_nudge(x = 15, y = 1)) +
  ggtitle("Untreated") +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 5)))  # Adjust 'b' margin as needed 




meep3 <- ggplot(untrt, aes(x = Parasites_Recovered, y = Liver_Weight_g)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(x = "Parasites recovered", y = "Liver weight (g)") +
  theme_cowplot(12) +
  theme(
    axis.text.x = element_text(size = 12, hjust = 1, vjust = 1, margin = unit(c(0, 0, 0, 0), units = "in")),
    plot.background = element_rect(fill = "white"),
    strip.background = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.text = element_blank()
  ) +
  facet_wrap(~species, scales = "fixed") +
  stat_cor(aes(label = after_stat(rr.label)), color = "black", geom = "label", 
           method = "pearson", position = position_nudge(x = 15, y = 1)) 


meep5 <- ggplot(untrt, aes(x = Parasites_Recovered, y = Total_Weight_kg)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(x = "Parasites recovered", y = "Total weight (kg)") +
  theme_cowplot(12) +
  theme(
    axis.text.x = element_text(size = 12, hjust = 1, vjust = 1, margin = unit(c(0, 0, 0, 0), units = "in")),
    plot.background = element_rect(fill = "white"),
    strip.background = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.text = element_blank()
  ) +
  facet_wrap(~species, scales = "fixed") +
  stat_cor(aes(label = after_stat(rr.label)), color = "black", geom = "label", 
           method = "pearson", position = position_nudge(x = 15, y = 1)) 



combined<- cowplot::plot_grid(meep,meep3,meep5, ncol = 1, nrow = 3 ,align = "hv", axis = "lrbt", labels = c("A", "B", "C"),rel_widths = c(.5,.5,.5), label_size = 12 )
combined
ggsave("combined.png", plot = combined, device = "png",  units = "in",dpi=96)


