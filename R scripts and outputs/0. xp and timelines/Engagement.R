# Create table degree of engagement
# exp ~ performance


library(dplyr)
library(ggplot2)

# XP data for gamified group (no exclusions)
xp_data <- tibble::tibble(
  participant = c("fe1", "fe2", "fe3", "me1", "me2", "me3", "me4", "me5", "me6", "me7"),
  xp = c(22880, 12640, 170, 20460, 38090, 14590, 18620, 6700, 26620, 31750)
)


# Calculate avg AZM per participant
# You need to first run the AZM.R script before doing this
azm_avg <- all_azm_data_exp %>%
  group_by(participant) %>%
  summarise(avg_azm = mean(total_minutes, na.rm = TRUE))

# Merge with XP
engagement_data <- left_join(azm_avg, xp_data, by = "participant")


ggplot(engagement_data, aes(x = xp, y = avg_azm, label = participant)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  geom_text(nudge_y = 2, size = 3) +
  labs(title = "Relationship between Website Engagement and Physical Activity",
       x = "XP (Website Engagement)",
       y = "Average Daily Active Zone Minutes") +
  theme_few()


# Apply threshold of 3500 xp
xp_data_sorted <- xp_data %>% arrange(xp)
write.table(xp_data_sorted, pipe("pbcopy"), sep = "\t", row.names = FALSE)
