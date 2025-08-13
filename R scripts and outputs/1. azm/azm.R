library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(rstatix)


# 1. Import and clean data ------------------------------------------------

clean <- function(name, may_data, june_data) {
  # Import data
  may_csv <- read_csv(may_data, show_col_types = FALSE)
  june_csv <- read_csv(june_data, show_col_types = FALSE)
  
  # Summarise both datasets: take totals for each day
  may <- may_csv[, c(1, 3)] %>%
    mutate(date = as.Date(date_time)) %>%
    group_by(date) %>%
    summarise(total_minutes = sum(total_minutes, na.rm = TRUE))
  
  june <- june_csv[, c(1, 3)] %>%
    mutate(date = as.Date(date_time)) %>%
    group_by(date) %>%
    summarise(total_minutes = sum(total_minutes, na.rm = TRUE))
  
  # Combine into one table
  azm_all <- bind_rows(may, june) %>%
    mutate(participant = name)
  
  # Filter: remove first date and limit to 2025-05_05 to 2025-06-17
  azm_all <- azm_all %>%
    arrange(date) %>%
    filter(date != min(date), date <= as.Date("2025-06-17"), date >= as.Date("2025-05-05"))
  
  return(azm_all) # Return this new table so we can save it
}

# Create table for each participant using the new clean() function
  # Experimental group
me1_azm_data <- clean("me1", 
                "Participant Fitbit data/me1/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-05-04.csv", 
                "Participant Fitbit data/me1/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-06-01.csv"
)
me2_azm_data <- clean("me2", 
                "Participant Fitbit data/me2/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-05-04.csv", 
                "Participant Fitbit data/me2/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-06-01.csv"
)
me3_azm_data <- clean("me3", 
                "Participant Fitbit data/me3/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-05-03.csv", 
                "Participant Fitbit data/me3/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-06-01.csv"
)
me4_azm_data <- clean("me4", 
                "Participant Fitbit data/me4/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-05-06.csv", 
                "Participant Fitbit data/me4/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-06-01.csv"
)
me5_azm_data <- clean("me5", 
                "Participant Fitbit data/me5/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-05-01.csv", 
                "Participant Fitbit data/me5/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-06-01.csv"
)
me6_azm_data <- clean("me6", 
                "Participant Fitbit data/me6/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-05-03.csv", 
                "Participant Fitbit data/me6/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-06-01.csv"
)
me7_azm_data <- clean("me7", 
                "Participant Fitbit data/me7/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-05-04.csv", 
                "Participant Fitbit data/me7/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-06-01.csv"
)
fe1_azm_data <- clean("fe1", 
                "Participant Fitbit data/fe1/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-05-04.csv", 
                "Participant Fitbit data/fe1/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-06-01.csv"
)
fe2_azm_data <- clean("fe2", 
                "Participant Fitbit data/fe2/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-05-06.csv", 
                "Participant Fitbit data/fe2/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-06-01.csv"
)
# Omit due to low engagement
# fe3_azm_data <- clean("fe3", 
#                 "Participant Fitbit data/fe3/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-05-07.csv", 
#                 "Participant Fitbit data/fe3/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-06-01.csv"
# )

  # Control group
mc1_azm_data <- clean("mc1", 
                "Participant Fitbit data/mc1/Active Zone Minutes - 2025-05-05.csv", 
                "Participant Fitbit data/mc1/Active Zone Minutes - 2025-06-01.csv"
)
mc2_azm_data <- clean("mc2", 
                "Participant Fitbit data/mc2/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-05-01.csv", 
                "Participant Fitbit data/mc2/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-06-01.csv"
)
mc3_azm_data <- clean("mc3", 
                "Participant Fitbit data/mc3/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-05-06.csv", 
                "Participant Fitbit data/mc3/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-06-01.csv"
)
mc4_azm_data <- clean("mc4", 
                "Participant Fitbit data/mc4/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-05-06.csv", 
                "Participant Fitbit data/mc4/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-06-01.csv"
)
mc5_azm_data <- clean("mc5", 
                "Participant Fitbit data/mc5/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-05-08.csv", 
                "Participant Fitbit data/mc5/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-06-01.csv"
)
mc6_azm_data <- clean("mc6", 
                "Participant Fitbit data/mc6/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-05-06.csv", 
                "Participant Fitbit data/mc6/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-06-01.csv"
)
mc7_azm_data <- clean("mc7", 
                "Participant Fitbit data/mc7/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-05-01.csv", 
                "Participant Fitbit data/mc7/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-06-01.csv"
)
fc1_azm_data <- clean("fc1", 
                "Participant Fitbit data/fc1/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-05-06.csv", 
                "Participant Fitbit data/fc1/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-06-01.csv"
)
fc2_azm_data <- clean("fc2", 
                "Participant Fitbit data/fc2/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-05-08.csv", 
                "Participant Fitbit data/fc2/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-06-01.csv"
)
fc3_azm_data <- clean("fc3", 
                "Participant Fitbit data/fc3/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-05-14.csv", 
                "Participant Fitbit data/fc3/Active Zone Minutes (AZM)/Active Zone Minutes - 2025-06-01.csv"
)


# 2. Combine participant data ---------------------------------------------

# Combine all the data into 1 table

  # Experimental group
all_azm_data_exp <- bind_rows(
  me1_azm_data, 
  me2_azm_data, 
  me3_azm_data, 
  me4_azm_data, 
  me5_azm_data, 
  me6_azm_data, 
  me7_azm_data, 
  fe1_azm_data, 
  fe2_azm_data, 
  # fe3_azm_data
)

  # Control group
all_azm_data_ctrl <- bind_rows(
  mc1_azm_data, 
  mc2_azm_data, 
  mc3_azm_data, 
  mc4_azm_data, 
  mc5_azm_data, 
  mc6_azm_data, 
  mc7_azm_data, 
  fc1_azm_data, 
  fc2_azm_data, 
  fc3_azm_data
)


# 3. Visualise raw data ---------------------------------------------------

plot_raw_ind <- function(data, participant_name, plot_name) {
  daily_avg <- mean(data$total_minutes, na.rm = TRUE)
  
  p <- ggplot(data, aes(x = date, y = total_minutes)) +
    
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
      y =  max(data$total_minutes, na.rm = TRUE) + 5, # "text", x = min(data$date), y =  daily_avg + 5,  
      label = paste("Average =", round(daily_avg, 1)),
      hjust = 0, 
      color = viridis::viridis(3, option = "cividis")[2], 
      size = 5
      ) +
    
    labs(
      title = paste("Daily AZM:", participant_name),
      x = "Date", y = "Total AZM") +
    
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    scale_y_continuous(limits = c(0, max(data$total_minutes) + 10), expand = c(0, 0)) + 
    theme_few()
  
  print (p)
  # ggsave(plot_name, plot = p, width = 1000, height = 500, units = "px", dpi = 300)
    
}

# Call plot function
#plot_raw_ind(me1_azm_data, "me1", "me1 eda")
#plot_raw_ind(me2_azm_data, "me2", "me2 eda")
#plot_raw_ind(me3_azm_data, "me3", "me3 eda")
#plot_raw_ind(me4_azm_data, "me4", "me4 eda")
#plot_raw_ind(me5_azm_data, "me5", "me5 eda")
#plot_raw_ind(me6_azm_data, "me6", "me6 eda")
#plot_raw_ind(me7_azm_data, "me7", "me7 eda")
#plot_raw_ind(fe1_azm_data, "fe1", "fe1 eda")
#plot_raw_ind(fe2_azm_data, "fe2", "fe2 eda")
#plot_raw_ind(fe3_azm_data, "fe3", "fe3 eda")

#plot_raw_ind(mc1_azm_data, "mc1", "mc1 eda")
#plot_raw_ind(mc2_azm_data, "mc2", "mc2 eda")
#plot_raw_ind(mc3_azm_data, "mc3", "mc3 eda")
#plot_raw_ind(mc4_azm_data, "mc4", "mc4 eda")
#plot_raw_ind(mc5_azm_data, "mc5", "mc5 eda")
#plot_raw_ind(mc6_azm_data, "mc6", "mc6 eda")
#plot_raw_ind(mc7_azm_data, "mc7", "mc7 eda")
#plot_raw_ind(fc1_azm_data, "fc1", "fc1 eda")
#plot_raw_ind(fc2_azm_data, "fc2", "fc2 eda")
#plot_raw_ind(fc3_azm_data, "fc3", "fc3 eda")



# 4. Calculate group-level averages ---------------------------------------

# Calculate average for the group

  # Experimental group
participant_avg_exp <- all_azm_data_exp %>%
  group_by(participant) %>%
  summarise(avg_azm = mean(total_minutes, na.rm = TRUE))

group_avg_exp <- mean(all_azm_data_exp$total_minutes, na.rm = TRUE)

  # Control group
participant_avg_ctrl <- all_azm_data_ctrl %>%
  group_by(participant) %>%
  summarise(avg_azm = mean(total_minutes, na.rm = TRUE))

group_avg_ctrl <- mean(all_azm_data_ctrl$total_minutes, na.rm = TRUE)

# Plot this data
plot_raw_group <- function(all_azm_data, group_avg_data, group) {
  
  ggplot(all_azm_data, aes(x = date, y = total_minutes, color = participant)) +
    geom_line(size = 1) +
    
    labs(
      title = paste("Daily AZM by Participant:", group),
      x = "Date", 
      y = "Total AZM"
      ) +
    
    geom_hline(
      yintercept = group_avg_data, 
      linetype = "dashed", 
      color = viridis::viridis(3, option = "cividis")[2]
      ) +
    
    annotate(
      "text", x = min(all_azm_data$date), 
      y = max(all_azm_data$total_minutes, na.rm = TRUE) + 5,
      label = paste("Average =", round(group_avg_data, 1)),
      hjust = 0, 
      color = viridis::viridis(3, option = "cividis")[2], 
      size = 4
      ) +
    
    theme_few() +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    scale_y_continuous(limits = c(0, max(all_azm_data$total_minutes) + 20), expand = c(0,0)) +
    theme(legend.position = "bottom")

}

# Call plot function
plot_raw_group(all_azm_data_exp, group_avg_exp, "Experimental group")
ggsave("azm_experimental.png", plot = last_plot(), width = 7, height = 5, dpi = 300, units = "in")
plot_raw_group(all_azm_data_ctrl, group_avg_ctrl, "Control group")
ggsave("azm_control.png", plot = last_plot(), width = 7, height = 5, dpi = 300, units = "in")


# 5. Outlier detection and removal ----------------------------------------

# Tag participants by group
all_azm_data_exp$group <- "gamified"
all_azm_data_ctrl$group <- "non-gamified"

# Merge both groups into one table
azm_full <- bind_rows(all_azm_data_exp, all_azm_data_ctrl)

# Compute participant-level summary
participant_summary <- azm_full %>%
  group_by(participant, group) %>%
  summarise(avg_azm = mean(total_minutes, na.rm = TRUE), .groups = "drop")

participant_summary_sorted <- participant_summary %>%
  mutate(participant = reorder(as.character(participant), avg_azm))

# Quick intermezzo plot
ggplot(participant_summary_sorted, aes(x = participant, y = avg_azm, fill = group)) +
  geom_col() + 
  labs(
    title = paste("Participant averages plot: AZM"),
    x = "Date", y = "Total AZM") +
  theme_few() + 
  scale_fill_viridis_d(option = "cividis") +
  scale_y_continuous(limits = c(0, max(participant_summary_sorted$avg_azm) + 10), expand = c(0,0))


# Normality test: QQ plot (visual test)
  # Full data -> skewed
qqnorm(participant_summary$avg_azm)
qqline(participant_summary$avg_azm)

  # Experimental group
qqnorm(participant_summary$avg_azm[participant_summary$group == "gamified"])
qqline(participant_summary$avg_azm[participant_summary$group == "gamified"])

  # Control group -> heavy right-skew : not normally distributed
qqnorm(participant_summary$avg_azm[participant_summary$group == "non-gamified"])
qqline(participant_summary$avg_azm[participant_summary$group == "non-gamified"])

# Normality test: Shapiro-Wilk test
azm_shapiro_full <- shapiro.test(participant_summary$avg_azm) # p = 0.01720314 : not normal
azm_shapiro_exp <- shapiro.test(participant_summary$avg_azm[participant_summary$group == "gamified"]) # p = 0.94885720 : normal
azm_shapiro_ctrl <- shapiro.test(participant_summary$avg_azm[participant_summary$group == "non-gamified"]) # p = 0.04835226 : not normal
# These results show that we'll have to use IQR and a wilcox test for group comparison (t-test no longer works)
print("Shapiro-Wilk normality test values:")
print(c(Experimental = azm_shapiro_exp$p.value,
        Control = azm_shapiro_ctrl$p.value,
        Combined = azm_shapiro_full$p.value))


# Outlier removal: IQR method
azm_iqr <- IQR(participant_summary$avg_azm, na.rm = TRUE)
azm_q1 <- quantile(participant_summary$avg_azm, 0.25, na.rm = TRUE)
azm_q3 <- quantile(participant_summary$avg_azm, 0.75, na.rm = TRUE)
azm_lower <- q1 - 1.5 * iqr
azm_upper <- q3 + 1.5 * iqr

azm_outliers_iqr <- participant_summary %>%
  filter(avg_azm < lower | avg_azm > upper)

# Outlier removal: Z-score method (for robustness)
participant_summary <- participant_summary %>%
  mutate(z_score = scale(avg_azm))

azm_outliers_z <- participant_summary %>%
  filter(abs(z_score) > 3) 


# Combine outlier ids
azm_outlier_ids <- unique(c(azm_outliers_z$participant, azm_outliers_iqr$participant))

# Filter out outliers
participant_summary_clean <- participant_summary %>%
  filter(!participant %in% azm_outlier_ids)

azm_full_clean <- azm_full %>%
  filter(!participant %in% azm_outlier_ids)

print("Outliers removed:")
print(azm_outlier_ids)

azm_shapiro_full_clean <- shapiro.test(participant_summary$avg_azm) # p = 0.02167 : not normal
azm_shapiro_ctrl_clean <- shapiro.test(participant_summary$avg_azm[participant_summary$group == "non-gamified"]) # p = 0.04835226 : not normal

# 6. Weekly aggregation ---------------------------------------------------
# These variables are reused later in 11)

azm_full_clean$week <- lubridate::floor_date(azm_full_clean$date, unit = "week", week_start = 1)
azm_weekly_avg <- azm_full_clean %>%
  group_by(week, group) %>%
  summarise(mean_azm = mean(total_minutes, na.rm = TRUE), .groups = "drop") %>%
  filter(week > as.Date("2025-04-27"))

azm_weekly_avg$week_num <- lubridate::isoweek(azm_weekly_avg$week)
azm_weekly_avg$week_num <- azm_weekly_avg$week_num - min(azm_weekly_avg$week_num) + 1

# Daily aggregation for extra plot
azm_daily_avg <- azm_full_clean %>%
  group_by(date, group) %>%
  summarise(mean_azm = mean(total_minutes, na.rm = TRUE), .groups = "drop") %>%
  filter(date > as.Date("2025-04-27"))


# 7. Baseline vs post-intervention -----------------------------------------

azm_exp_clean <- azm_full_clean %>%
  filter(group == "gamified") %>%
  mutate(phase = ifelse(date < as.Date("2025-05-12"), "baseline", "intervention"))

azm_baseline_vs_post <- azm_exp_clean %>%
  group_by(participant, phase) %>%
  summarise(avg_azm = mean(total_minutes, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = phase, values_from = avg_azm)

azm_prepost_wilcox <- wilcox.test(azm_baseline_vs_post$baseline, azm_baseline_vs_post$intervention, paired = TRUE) # p = 0.1055


# 8. Group comparison -----------------------------------------------------

azm_group_wilcox <- wilcox.test(avg_azm ~ group, data = participant_summary_clean) # p = 0.1333 > 0.05 : fail to reject null hypothesis


# 9. Compute effect sizes -------------------------------------------------

# Mann-Whitney effect size (r = Z / sqrt(n))
# You’ll need to extract Z manually if using wilcox.test()

azm_effsize_mw <- participant_summary_clean %>%
  rstatix::wilcox_effsize(avg_azm ~ group) # Small effect size (r = 0.356)
# The effect size (r = 0.30) indicates a small difference in AZM between groups, favoring the gamified condition.



# 10. Correlate with engagement ------------------------------------------

azm_engaged_intervention <- azm_baseline_vs_post %>%
  select(participant, intervention) %>%
  left_join(xp_data, by = "participant")

# Spearman for non-parametric correlation
azm_cor_spearman <- cor.test(azm_engaged_intervention$intervention, azm_engaged_intervention$xp, method = "spearman") # rho = 0.722, p = 0.02419 < 0.05 : reject null


# 11. Visualise final results ---------------------------------------------

# Boxplot by group
ggplot(participant_summary_clean, aes(x = group, y = avg_azm, fill = group)) +
  geom_boxplot(color = viridis::viridis(3, option = "cividis")[2]) +
  labs(
    title = "Average Daily AZM per Participant",
    x = "Group",
    y = "Average AZM"
  ) +
  theme(legend.position = "none") +
  guides(fill = "none") +
  scale_fill_viridis_d(option = "cividis") +
  theme_few()
ggsave("azm_boxplot.png", plot = last_plot(), width = 7, height = 5, dpi = 300, units = "in")


# Weekly trend
ggplot(azm_weekly_avg, aes(x = week_num, y = mean_azm, color = group)) +
  geom_line(size = 1.2) +
  geom_point() +
  geom_vline(aes(xintercept = 1.5, linetype = "Intervention Start"), 
             color = viridis::viridis(3, option = "cividis")[2]) +
  scale_linetype_manual(name = "", values = c("Intervention Start" = 2)) +
  labs(
    title = "Weekly Group Averages of AZM",
    x = "Week Number",
    y = "Mean AZM",
    color = "Group"
  ) +
  scale_x_continuous(
    name = "Week",
    breaks = azm_weekly_avg$week_num  # Show tick for every week
  ) +
  scale_color_viridis_d(option = "cividis") +
  theme_few()
ggsave("azm_lineplot.png", plot = last_plot(), width = 7, height = 5, dpi = 300, units = "in")

# Daily trend
ggplot(azm_daily_avg, aes(x = date, y = mean_azm, color = group)) +
  geom_line(size = 1.2) +
  geom_point() +
  geom_vline(aes(xintercept = as.Date("2025-05-12"), linetype = "Intervention Start"), 
             color = viridis::viridis(3, option = "cividis")[2]) +
  scale_linetype_manual(name = "", values = c("Intervention Start" = 2)) +
  labs(
    title = "Daily Group Averages of AZM",
    x = "Date",
    y = "Mean AZM",
    color = "Group"
  ) +
  scale_color_viridis_d(option = "cividis") +
  theme_few()
ggsave("azm_lineplot_daily.png", plot = last_plot(), width = 7, height = 5, dpi = 300, units = "in")

# Paired pre/post plot for both groups
azm_avg_by_group <- azm_full_clean %>%
  mutate(phase = ifelse(date < as.Date("2025-05-12"), "baseline", "intervention")) %>%
  group_by(participant, group, phase) %>%
  summarise(avg_azm = mean(total_minutes, na.rm = TRUE), .groups = "drop") %>%
  group_by(group, phase) %>%
  summarise(mean_azm = mean(avg_azm), .groups = "drop")

# Split the data into two data frames
control_data <- subset(azm_avg_by_group, group == "non-gamified")
gamified_data <- subset(azm_avg_by_group, group == "gamified")

ggplot(azm_avg_by_group, aes(x = phase, y = mean_azm, group = group, color = group)) +
  geom_vline(xintercept = "baseline", color = viridis::viridis(3, option = "cividis")[2], alpha = 0.5) +
  geom_vline(xintercept = "intervention", color = viridis::viridis(3, option = "cividis")[2], alpha = 0.5) +
  
  # Plot non-gamified group first (goes in the back)
  geom_line(data = control_data, aes(x = phase, y = mean_azm, group = group, color = group), size = 1.2) +
  geom_point(data = control_data, aes(x = phase, y = mean_azm, group = group, color = group), size = 3) +
  
  # Plot gamified group second (goes in front)
  geom_line(data = gamified_data, aes(x = phase, y = mean_azm, group = group, color = group), size = 1.2) +
  geom_point(data = gamified_data, aes(x = phase, y = mean_azm, group = group, color = group), size = 3) +
  
  labs(
    title = "Baseline vs. Post-Intervention AZM (Group Average)",
    x = "Phase",
    y = "Average AZM",
    color = "Group",
    linetype = element_blank()
  ) +
  scale_x_discrete(expand = c(0, 0.3)) +
  scale_y_continuous(limits = c(0, 80), expand = c(0, 0)) +
  scale_color_viridis_d(option = "cividis") +
  theme_few()
ggsave("azm_prepost.png", plot = last_plot(), width = 7, height = 5, dpi = 300, units = "in")



# 12. Results Summary Table -----------------------------------------------

azm_results_summary <- tibble::tibble(
  Analysis = c(
    "Outlier(s)",
    "Pre vs Post",
    "Group comparison",
    "Effect size",
    "Engagement correlation"
  ),
  `Test Used` = c(
    "IQR & Z-score",
    "Wilcoxon signed-rank test",
    "Wilcoxon rank-sum test",
    "Wilcoxon effect size",
    "Spearman correlation"
  ),
  Result = c(
    "mc7",
    paste0("p = ", signif(azm_prepost_wilcox$p.value, 4)),
    paste0("p = ", signif(azm_group_wilcox$p.value, 4)),
    paste0("r = ", signif(azm_effsize_mw$effsize, 3)),
    paste0("p = ", signif(azm_cor_spearman$p.value, 3), "*",
           ", ρ = ", signif(azm_cor_spearman$estimate, 3))
  ),
  Interpretation = c(
    "Removed",
    "No significant association",
    "No significant association",
    "Moderate effect",
    "Possible positive association"
  )
)

# View and save
View(azm_results_summary)
write.table(azm_results_summary, pipe("pbcopy"), sep = "\t", row.names = FALSE, quote = FALSE)
write_csv(azm_results_summary, "azm_results_summary.csv")
