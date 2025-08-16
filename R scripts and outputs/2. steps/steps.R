library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(rstatix)
library(tidyr)


# 1. Import and clean data ------------------------------------------------

clean <- function(name, may_data, june_data) {
  # Import data
  may_csv <- read_csv(may_data, show_col_types = FALSE)
  june_csv <- read_csv(june_data, show_col_types = FALSE)
  
  # Summarise both datasets: take totals for each day
  may <- may_csv[, c(1, 2)] %>%
    mutate(date = as.Date(timestamp)) %>%
    group_by(date) %>%
    summarise(steps = sum(steps, na.rm = TRUE))
  
  june <- june_csv[, c(1, 2)] %>%
    mutate(date = as.Date(timestamp)) %>%
    group_by(date) %>%
    summarise(steps = sum(steps, na.rm = TRUE))
  
  # Combine into one table
  steps_all <- bind_rows(may, june) %>%
    mutate(participant = name)
  
  # Filter: remove first date and limit to 2025-05_05 to 2025-06-17
  steps_all <- steps_all %>%
    arrange(date) %>%
    filter(date != min(date), date <= as.Date("2025-06-17"), date >= as.Date("2025-05-05"))
  
  return(steps_all) # Return this new table so we can save it
}

# Create table for each participant using the new clean() function
# Experimental group
me1_steps_data <- clean("me1", 
                        "../../Participant Fitbit data/me1/Physical Activity_GoogleData/steps_2025-05-04.csv",
                        "../../Participant Fitbit data/me1/Physical Activity_GoogleData/steps_2025-06-01.csv"
)
me2_steps_data <- clean("me2", 
                        "../../Participant Fitbit data/me2/Physical Activity_GoogleData/steps_2025-05-04.csv", 
                        "../../Participant Fitbit data/me2/Physical Activity_GoogleData/steps_2025-06-01.csv"
)
me3_steps_data <- clean("me3", 
                        "../../Participant Fitbit data/me3/Physical Activity_GoogleData/steps_2025-05-03.csv", 
                        "../../Participant Fitbit data/me3/Physical Activity_GoogleData/steps_2025-06-01.csv"
)
me4_steps_data <- clean("me4", 
                        "../../Participant Fitbit data/me4/Physical Activity_GoogleData/steps_2025-05-06.csv", 
                        "../../Participant Fitbit data/me4/Physical Activity_GoogleData/steps_2025-06-01.csv"
)
me5_steps_data <- clean("me5", 
                        "../../Participant Fitbit data/me5/Physical Activity_GoogleData/steps_2025-05-01.csv", 
                        "../../Participant Fitbit data/me5/Physical Activity_GoogleData/steps_2025-06-01.csv"
)
me6_steps_data <- clean("me6", 
                        "../../Participant Fitbit data/me6/Physical Activity_GoogleData/steps_2025-05-03.csv", 
                        "../../Participant Fitbit data/me6/Physical Activity_GoogleData/steps_2025-06-01.csv"
)
me7_steps_data <- clean("me7", 
                        "../../Participant Fitbit data/me7/Physical Activity_GoogleData/steps_2025-05-04.csv",
                        "../../Participant Fitbit data/me7/Physical Activity_GoogleData/steps_2025-06-01.csv"
)
fe1_steps_data <- clean("fe1", 
                        "../../Participant Fitbit data/fe1/Physical Activity_GoogleData/steps_2025-05-04.csv", 
                        "../../Participant Fitbit data/fe1/Physical Activity_GoogleData/steps_2025-06-01.csv"
)
fe2_steps_data <- clean("fe2", 
                        "../../Participant Fitbit data/fe2/Physical Activity_GoogleData/steps_2025-05-06.csv", 
                        "../../Participant Fitbit data/fe2/Physical Activity_GoogleData/steps_2025-06-01.csv"
)
# Omit due to low engagement
# fe3_steps_data <- clean("fe3", 
#                         "../../Participant Fitbit data/fe3/Physical Activity_GoogleData/steps_2025-05-07.csv", 
#                         "../../Participant Fitbit data/fe3/Physical Activity_GoogleData/steps_2025-06-01.csv"
# )

# Control group
mc1_steps_data <- clean("mc1",
                        "../../Participant Fitbit data/mc1/steps_2025-05-05.csv", 
                        "../../Participant Fitbit data/mc1/steps_2025-06-01.csv"
)
mc2_steps_data <- clean("mc2", 
                        "../../Participant Fitbit data/mc2/Physical Activity_GoogleData/steps_2025-05-01.csv", 
                        "../../Participant Fitbit data/mc2/Physical Activity_GoogleData/steps_2025-06-01.csv"
)
mc3_steps_data <- clean("mc3", 
                        "../../Participant Fitbit data/mc3/Physical Activity_GoogleData/steps_2025-05-06.csv", 
                        "../../Participant Fitbit data/mc3/Physical Activity_GoogleData/steps_2025-06-01.csv"
)
mc4_steps_data <- clean("mc4", 
                        "../../Participant Fitbit data/mc4/Physical Activity_GoogleData/steps_2025-05-06.csv", 
                        "../../Participant Fitbit data/mc4/Physical Activity_GoogleData/steps_2025-06-01.csv"
)
mc5_steps_data <- clean("mc5", 
                        "../../Participant Fitbit data/mc5/Physical Activity_GoogleData/steps_2025-05-08.csv", 
                        "../../Participant Fitbit data/mc5/Physical Activity_GoogleData/steps_2025-06-01.csv"
)
mc6_steps_data <- clean("mc6", 
                        "../../Participant Fitbit data/mc6/Physical Activity_GoogleData/steps_2025-05-06.csv", 
                        "../../Participant Fitbit data/mc6/Physical Activity_GoogleData/steps_2025-06-01.csv"
)
mc7_steps_data <- clean("mc7", 
                        "../../Participant Fitbit data/mc7/Physical Activity_GoogleData/steps_2025-05-01.csv", 
                        "../../Participant Fitbit data/mc7/Physical Activity_GoogleData/steps_2025-06-01.csv"
)
fc1_steps_data <- clean("fc1", 
                        "../../Participant Fitbit data/fc1/Physical Activity_GoogleData/steps_2025-05-06.csv", 
                        "../../Participant Fitbit data/fc1/Physical Activity_GoogleData/steps_2025-06-01.csv"
)
fc2_steps_data <- clean("fc2", 
                        "../../Participant Fitbit data/fc2/Physical Activity_GoogleData/steps_2025-05-08.csv", 
                        "../../Participant Fitbit data/fc2/Physical Activity_GoogleData/steps_2025-06-01.csv"
)
fc3_steps_data <- clean("fc3", 
                        "../../Participant Fitbit data/fc3/Physical Activity_GoogleData/steps_2025-05-14.csv", 
                        "../../Participant Fitbit data/fc3/Physical Activity_GoogleData/steps_2025-06-01.csv"
)

# 2. Combine participant data ---------------------------------------------

# Combine all the data into 1 table

# Experimental group
all_steps_data_exp <- bind_rows(
  me1_steps_data, 
  me2_steps_data, 
  me3_steps_data, 
  me4_steps_data, 
  me5_steps_data, 
  me6_steps_data, 
  me7_steps_data, 
  fe1_steps_data, 
  fe2_steps_data, 
  # fe3_steps_data
)

# Control group
all_steps_data_ctrl <- bind_rows(
  mc1_steps_data, 
  mc2_steps_data, 
  mc3_steps_data, 
  mc4_steps_data, 
  mc5_steps_data, 
  mc6_steps_data, 
  mc7_steps_data, 
  fc1_steps_data, 
  fc2_steps_data, 
  fc3_steps_data
)


# 3. Visualise raw data ---------------------------------------------------

plot_raw_ind <- function(data, participant_name, plot_name) {
  daily_avg <- mean(data$steps, na.rm = TRUE)
  
  p <- ggplot(data, aes(x = date, y = steps)) +
    
    geom_col(fill = viridis::viridis(2, option = "cividis")[1]) +
    
    geom_hline(
      yintercept = daily_avg, 
      linetype = "dashed", 
      color = viridis::viridis(3, option = "cividis")[2],
      linewidth = 1
    ) +
    
    annotate(
      "text", 
      x = min(data$date), 
      y =  max(data$steps, na.rm = TRUE) + 0.05 * max(data$steps), 
      label = paste("Average =", round(daily_avg, 1)),
      hjust = 0, 
      color = viridis::viridis(3, option = "cividis")[2], 
      size = 5
    ) +
    
    labs(
      title = paste("Daily Steps:", participant_name),
      x = "Date", y = "Total Steps") +
    
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    scale_y_continuous(limits = c(0, max(data$steps) + 0.1 * max(data$steps)), expand = c(0, 0)) + 
    theme_few()
  
  print (p)
  # ggsave(plot_name, plot = p, width = 1000, height = 500, units = "px", dpi = 300)
  
}

# Call plot function
# plot_raw_ind(me1_steps_data, "me1", "me1 eda")
# plot_raw_ind(me2_steps_data, "me2", "me2 eda")
# plot_raw_ind(me3_steps_data, "me3", "me3 eda")
# plot_raw_ind(me4_steps_data, "me4", "me4 eda")
# plot_raw_ind(me5_steps_data, "me5", "me5 eda")
# plot_raw_ind(me6_steps_data, "me6", "me6 eda")
# plot_raw_ind(me7_steps_data, "me7", "me7 eda")
# plot_raw_ind(fe1_steps_data, "fe1", "fe1 eda")
# plot_raw_ind(fe2_steps_data, "fe2", "fe2 eda")
#plot_raw_ind(fe3_steps_data, "fe3", "fe3 eda")

#plot_raw_ind(mc1_steps_data, "mc1", "mc1 eda")
#plot_raw_ind(mc2_steps_data, "mc2", "mc2 eda")
#plot_raw_ind(mc3_steps_data, "mc3", "mc3 eda")
#plot_raw_ind(mc4_steps_data, "mc4", "mc4 eda")
#plot_raw_ind(mc5_steps_data, "mc5", "mc5 eda")
#plot_raw_ind(mc6_steps_data, "mc6", "mc6 eda")
#plot_raw_ind(mc7_steps_data, "mc7", "mc7 eda")
#plot_raw_ind(fc1_steps_data, "fc1", "fc1 eda")
#plot_raw_ind(fc2_steps_data, "fc2", "fc2 eda")
#plot_raw_ind(fc3_steps_data, "fc3", "fc3 eda")



# 4. Calculate group-level averages ---------------------------------------

# Calculate average for the group

# Experimental group
participant_avg_exp <- all_steps_data_exp %>%
  group_by(participant) %>%
  summarise(avg_steps = mean(steps, na.rm = TRUE))

group_avg_exp <- mean(all_steps_data_exp$steps, na.rm = TRUE)

# Control group
participant_avg_ctrl <- all_steps_data_ctrl %>%
  group_by(participant) %>%
  summarise(avg_steps = mean(steps, na.rm = TRUE))

group_avg_ctrl <- mean(all_steps_data_ctrl$steps, na.rm = TRUE)

# Plot this data
plot_raw_group <- function(all_steps_data, group_avg_data, group) {
  
  ggplot(all_steps_data, aes(x = date, y = steps, color = participant)) +
    geom_line(size = 1) +
    
    labs(
      title = paste("Daily Steps by Participant:", group),
      x = "Date", 
      y = "Total Steps"
    ) +
    
    geom_hline(
      yintercept = group_avg_data, 
      linetype = "dashed", 
      color = viridis::viridis(3, option = "cividis")[2]
    ) +
    
    annotate(
      "text", x = min(all_steps_data$date), 
      y = max(all_steps_data$steps, na.rm = TRUE) + 0.05 * max(all_steps_data$steps),
      label = paste("Average =", round(group_avg_data, 1)),
      hjust = 0, 
      color = viridis::viridis(3, option = "cividis")[2], 
      size = 4
    ) +
    
    theme_few() +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    scale_y_continuous(limits = c(0, max(all_steps_data$steps) + 0.1 * max(all_steps_data$steps)), expand = c(0,0)) +
    scale_color_viridis_d(option = "cividis") +
    theme(legend.position = "bottom")
  
}

# Call plot function
plot_raw_group(all_steps_data_exp, group_avg_exp, "Experimental group")
plot_raw_group(all_steps_data_ctrl, group_avg_ctrl, "Control group")


# 5. Outlier detection and removal ----------------------------------------

# Tag participants by group
all_steps_data_exp$group <- "gamified"
all_steps_data_ctrl$group <- "non-gamified"

# Merge both groups into one table
steps_full <- bind_rows(all_steps_data_exp, all_steps_data_ctrl)

# Compute participant-level summary
participant_summary <- steps_full %>%
  group_by(participant, group) %>%
  summarise(avg_steps = mean(steps, na.rm = TRUE), .groups = "drop")

participant_summary_sorted <- participant_summary %>%
  mutate(participant = reorder(as.character(participant), avg_steps))

# Quick intermezzo plot
ggplot(participant_summary_sorted, aes(x = participant, y = avg_steps, fill = group)) +
  geom_col() + 
  labs(
    title = paste("Participant averages plot: Steps"),
    x = "Date", y = "Total Steps") +
  theme_few() + 
  scale_fill_viridis_d(option = "cividis") +
  scale_y_continuous(limits = c(0, max(participant_summary_sorted$avg_steps) + 10), expand = c(0,0))


# Normality test: QQ plot (visual test)
# Full data
qqnorm(participant_summary$avg_steps)
qqline(participant_summary$avg_steps)

# Experimental group
qqnorm(participant_summary$avg_steps[participant_summary$group == "gamified"])
qqline(participant_summary$avg_steps[participant_summary$group == "gamified"])

# Control group
qqnorm(participant_summary$avg_steps[participant_summary$group == "non-gamified"])
qqline(participant_summary$avg_steps[participant_summary$group == "non-gamified"])

# Normality test: Shapiro-Wilk test
steps_shapiro_full <- shapiro.test(participant_summary$avg_steps) #  0.4915917 :normal
steps_shapiro_exp <- shapiro.test(participant_summary$avg_steps[participant_summary$group == "gamified"]) # p = 0.6155488 : normal
steps_shapiro_ctrl <- shapiro.test(participant_summary$avg_steps[participant_summary$group == "non-gamified"]) # p = 0.8163894 : normal
# These results show that we'll have to use Z-score and a t-test test for group comparison

print("Shapiro-Wilk normality test values:")
print(c(Experimental = steps_shapiro_exp$p.value,
        Control = steps_shapiro_ctrl$p.value,
        Combined = steps_shapiro_full$p.value))

# Outlier removal: Z-score method
participant_summary <- participant_summary %>%
  mutate(z_score = scale(avg_steps))

steps_outliers_z <- participant_summary %>%
  filter(abs(z_score) > 3)


# Outlier removal: IQR method (for robustness)
steps_iqr <- IQR(participant_summary$avg_steps, na.rm = TRUE)
steps_q1 <- quantile(participant_summary$avg_steps, 0.25, na.rm = TRUE)
steps_q3 <- quantile(participant_summary$avg_steps, 0.75, na.rm = TRUE)
steps_lower <- steps_q1 - 1.5 * steps_iqr
steps_upper <- steps_q3 + 1.5 * steps_iqr

steps_outliers_iqr <- participant_summary %>%
  filter(avg_steps < steps_lower | avg_steps > steps_upper) # me2 and me7 


# Combine outlier ids
steps_outlier_ids <- unique(c(steps_outliers_z$participant, steps_outliers_iqr$participant))


# Filter out outliers
# participant_summary_clean <- participant_summary %>%
#   filter(!participant %in% steps_outlier_ids)
# 
# steps_full_clean <- steps_full %>%
#   filter(!participant %in% steps_outlier_ids)
# 
# print("Outliers removed:")
# print(steps_outlier_ids)

# Check normality again
steps_shapiro_full_clean <- shapiro.test(participant_summary$avg_steps) # p = 0.02167 : not normal
steps_shapiro_ctrl_clean <- shapiro.test(participant_summary$avg_steps[participant_summary$group == "non-gamified"]) # p = 0.04835226 : not normal

participant_summary_clean <- participant_summary
steps_full_clean <- steps_full

# 6. Weekly aggregation ---------------------------------------------------
# These variables are reused later in 11)

steps_full_clean$week <- lubridate::floor_date(steps_full_clean$date, unit = "week", week_start = 1)
steps_weekly_avg <- steps_full_clean %>%
  group_by(week, group) %>%
  summarise(mean_steps = mean(steps, na.rm = TRUE), .groups = "drop") %>%
  filter(week > as.Date("2025-04-27"))

steps_weekly_avg$week_num <- lubridate::isoweek(steps_weekly_avg$week)
steps_weekly_avg$week_num <- steps_weekly_avg$week_num - min(steps_weekly_avg$week_num) + 1

# Daily aggregation for extra plot
steps_daily_avg <- steps_full_clean %>%
  group_by(date, group) %>%
  summarise(mean_steps = mean(steps, na.rm = TRUE), .groups = "drop") %>%
  filter(date > as.Date("2025-04-27"))


# 7. Baseline vs post-intervention -----------------------------------------

steps_exp_clean <- steps_full_clean %>%
  filter(group == "gamified") %>%
  mutate(phase = ifelse(date < as.Date("2025-05-12"), "baseline", "intervention"))

steps_baseline_vs_post <- steps_exp_clean %>%
  group_by(participant, phase) %>%
  summarise(avg_steps = mean(steps, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = phase, values_from = avg_steps)

# Wilcoxon signed-rank test
steps_prepost_wilcox <- wilcox.test(steps_baseline_vs_post$baseline, steps_baseline_vs_post$intervention, paired = TRUE) # p = 0.07422


# 8. Group comparison -----------------------------------------------------

# Wilcoxon rank-sum test
steps_group_wilcox <- wilcox.test(avg_steps ~ group, data = participant_summary_clean) # p = 0.1564


# 9. Compute effect sizes -------------------------------------------------
# Prepost (added last minute)
steps_long <- steps_baseline_vs_post %>% pivot_longer(c(baseline, intervention),
                                                  names_to = "phase", values_to = "avg_steps") %>%
  mutate(phase = factor(phase, levels = c("baseline", "intervention")))
steps_prepost_effsize <- rstatix::wilcox_effsize(steps_long, avg_steps ~ phase, paired = TRUE)



steps_effsize_mw <- rstatix::wilcox_effsize(participant_summary_clean, avg_steps ~ group) # Moderate effect size (d = 0.337)
# The effect size indicates a moderate difference in steps between groups, favoring the gamified condition.


# 10. Correlate with engagement ------------------------------------------

steps_engaged_intervention <- steps_baseline_vs_post %>%
  select(participant, intervention) %>%
  left_join(xp_data, by = "participant")

# Spearman for non-paramatric correlation
steps_cor_spearman <- cor.test(steps_engaged_intervention$intervention, steps_engaged_intervention$xp, method = "spearman") # p = 0.001312 << 0.05 : reject null
# cor = 0.9166667 


# 11. Visualise final results ---------------------------------------------

# Boxplot by group
ggplot(participant_summary_clean, aes(x = group, y = avg_steps, fill = group)) +
  geom_boxplot(color = viridis::viridis(3, option = "cividis")[2]) +
  labs(
    title = "Average Daily Steps per Participant",
    x = "Group",
    y = "Average Steps"
  ) +
  theme(legend.position = "none") +
  guides(fill = "none") +
  scale_fill_viridis_d(option = "cividis") +
  theme_few()
ggsave("steps_boxplot.png", plot = last_plot(), width = 7, height = 5, dpi = 300, units = "in")


# Weekly trend
ggplot(steps_weekly_avg, aes(x = week_num, y = mean_steps, color = group)) +
  geom_line(size = 1.2) +
  geom_point() +
  geom_vline(aes(xintercept = 1.5, linetype = "Intervention Start"), 
             color = viridis::viridis(3, option = "cividis")[2]) +
  scale_linetype_manual(name = "", values = c("Intervention Start" = 2)) +
  labs(
    title = "Weekly Group Averages of Steps",
    x = "Week Number",
    y = "Mean Steps",
    color = "Group"
  ) +
  scale_x_continuous(
    name = "Week",
    breaks = steps_weekly_avg$week_num  # Show tick for every week
  ) +
  scale_color_viridis_d(option = "cividis") +
  theme_few()
ggsave("steps_lineplot.png", plot = last_plot(), width = 7, height = 5, dpi = 300, units = "in")

# Daily trend
ggplot(steps_daily_avg, aes(x = date, y = mean_steps, color = group)) +
  geom_line(size = 1.2) +
  geom_point() +
  geom_vline(aes(xintercept = as.Date("2025-05-12"), linetype = "Intervention Start"), 
             color = viridis::viridis(3, option = "cividis")[2]) +
  scale_linetype_manual(name = "", values = c("Intervention Start" = 2)) +
  labs(
    title = "Daily Group Averages of Steps",
    x = "Date",
    y = "Mean Steps",
    color = "Group"
  ) +
  scale_color_viridis_d(option = "cividis") +
  theme_few()
ggsave("steps_lineplot_daily.png", plot = last_plot(), width = 7, height = 5, dpi = 300, units = "in")

# Paired pre/post plot for both groups
steps_avg_by_group <- steps_full_clean %>%
  mutate(phase = ifelse(date < as.Date("2025-05-12"), "baseline", "intervention")) %>%
  group_by(participant, group, phase) %>%
  summarise(avg_steps = mean(steps, na.rm = TRUE), .groups = "drop") %>%
  group_by(group, phase) %>%
  summarise(mean_steps = mean(avg_steps), .groups = "drop")

# Split the data into two data frames
control_data <- subset(steps_avg_by_group, group == "non-gamified")
gamified_data <- subset(steps_avg_by_group, group == "gamified")

ggplot(steps_avg_by_group, aes(x = phase, y = mean_steps, group = group, color = group)) +
  geom_vline(xintercept = "baseline", color = viridis::viridis(3, option = "cividis")[2], alpha = 0.5) +
  geom_vline(xintercept = "intervention", color = viridis::viridis(3, option = "cividis")[2], alpha = 0.5) +
  
  # Plot non-gamified group first (goes in the back)
  geom_line(data = control_data, aes(x = phase, y = mean_steps, group = group, color = group), size = 1.2) +
  geom_point(data = control_data, aes(x = phase, y = mean_steps, group = group, color = group), size = 3) +
  
  # Plot gamified group second (goes in front)
  geom_line(data = gamified_data, aes(x = phase, y = mean_steps, group = group, color = group), size = 1.2) +
  geom_point(data = gamified_data, aes(x = phase, y = mean_steps, group = group, color = group), size = 3) +
  
  labs(
    title = "Baseline vs. Post-Intervention Steps (Group Average)",
    x = "Phase",
    y = "Average Steps",
    color = "Group",
    linetype = element_blank()
  ) +
  scale_x_discrete(expand = c(0, 0.3)) +
  scale_y_continuous(limits = c(0, 1.5 * max(steps_avg_by_group$mean_steps)), expand = c(0, 0)) +
  scale_color_viridis_d(option = "cividis") +
  theme_few()
ggsave("steps_prepost.png", plot = last_plot(), width = 7, height = 5, dpi = 300, units = "in")

# 12. Results Summary Table -----------------------------------------------

steps_results_summary <- tibble::tibble(
  Analysis = c(
    "Outlier(s)",
    "Pre vs Post",
    "Group comparison",
    "Engagement correlation"
  ),
  `Test Used` = c(
    "IQR & Z-score",
    "Wilcoxon signed-rank test",
    "Wilcoxon rank-sum test",
    "Spearman correlation"
  ),
  Result = c(
    "me2, me7",
    paste0("p = ", signif(steps_prepost_wilcox$p.value, 4), "*",
           ", r = ", signif(steps_prepost_effsize$effsize, 3)),
    paste0("p = ", signif(steps_group_wilcox$p.value, 4),
           " r = ", signif(steps_effsize_mw$effsize, 3)),
    paste0("p = ", signif(steps_cor_spearman$p.value, 3), "***",
           ", ρ = ", signif(steps_cor_spearman$estimate, 3))
  ),
  Interpretation = c(
    "Retained",
    "Possible positive association",
    "No significant association",
    "Significant positive association"
    
  )
)

# View and save
View(steps_results_summary)
write.table(steps_results_summary, pipe("pbcopy"), sep = "\t", row.names = FALSE, quote = FALSE)
write_csv(steps_results_summary, "steps_results_summary.csv")

