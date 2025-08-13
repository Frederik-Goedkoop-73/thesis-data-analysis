library(ggplot2)
library(ggthemes)
library(scales)
library(gridExtra)

# 1. Current curve --------------------------------------------------------


# Define data
lvl <- 1:50
xp_quadratic <- ceiling(0.50025 * lvl^2 - 1.94125 * lvl + 7.7007)

data <- data.frame(
  lvl = lvl,
  XP_Quadratic = xp_quadratic
)

# Create plot
plot1 <- ggplot(data, aes(x = lvl)) +
  geom_line(aes(y = XP_Quadratic, color = "Quadratic progression"), linewidth = 1.2) +
  
  labs(
    title = "XP progression curve as a function of Level (lvl)",
    x = "lvl",
    y = "XP",
    color = "Legend",
    fill = "Legend"
  ) +
  
  coord_cartesian(xlim = c(1,50)) +
  scale_x_continuous(limits = c(1, 50), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) + 
  
  theme(legend.position = "bottom" ) +
  scale_color_viridis_d(option = "cividis") +
  
  theme_few() +
  theme(legend.position = "bottom", legend.title = element_blank() )




# 2. Improved suggestion --------------------------------------------------

# Suggestion 1: hybrid function

# Define data
lvl <- 1:50
xp_quadratic <- ceiling(0.50025 * lvl^2 - 1.5 * lvl + 7.7007) + 150
xp_linear <- lvl * 10 + 90
xp_hybrid <- ifelse(lvl < 10, xp_linear, xp_quadratic)

data <- data.frame(
  lvl = lvl,
  XP_Hybrid = xp_hybrid,
  XP_Quadratic = xp_quadratic,
  XP_Linear = xp_linear
)

# Create plot
plot2 <- ggplot(data, aes(x = lvl)) +
  geom_line(aes(y = XP_Hybrid, color = "Hybrid progression"), linewidth = 1.2) +
  geom_line(aes(y = XP_Quadratic, color = "Quadratic progression"), linewidth = 0.8, linetype = 2) +
  
  geom_vline(xintercept = 10, color = viridis::viridis(3, option = "cividis")[2], linetype = 4, alpha = 0.3) +
  
  annotate("text", x = 5, y = 1000, label = "Linear", hjust = 0.5, size = 3.5, color = viridis::viridis(3, option = "cividis")[2] ) +
  annotate("text", x = 25, y = 1000, label = "Quadratic", hjust = 0.5, size = 3.5, color = viridis::viridis(3, option = "cividis")[2] )+
  
  labs(
    title = "Improved XP progression curve as a function of Level (lvl)",
    x = "lvl",
    y = "XP",
    color = "Legend",
    fill = "Legend"
  ) +
  
  coord_cartesian(xlim = c(1,50), ylim = c(0, xp_hybrid[50]) ) +
  scale_x_continuous(limits = c(1, 50), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) + 
  scale_color_viridis_d(option = "cividis") +
  
  theme_few() +
  theme(legend.position = "bottom", legend.title = element_blank() )



# 3. Plot both ------------------------------------------------------------

xp_plots <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("XP_curves.png", plot = xp_plots, width = 12, height = 5, dpi = 300, units = "in")

