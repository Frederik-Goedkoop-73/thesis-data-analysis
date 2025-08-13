library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(rstatix)
library(scales)


# 1. Import and clean data ------------------------------------------------

clean <- function(name, may_data, june_data) {
  # Import data
  may_csv <- read_csv(may_data, show_col_types = FALSE)
  june_csv <- read_csv(june_data, show_col_types = FALSE)
  
  # Summarise both datasets: take totals for each day
  may <- may_csv[, c(1, 2)] %>%
    mutate(date = as.Date(timestamp)) %>%
    group_by(date) %>%
    summarise(calories = sum(calories, na.rm = TRUE))
  
  june <- june_csv[, c(1, 2)] %>%
    mutate(date = as.Date(timestamp)) %>%
    group_by(date) %>%
    summarise(calories = sum(calories, na.rm = TRUE))
  
  # Combine into one table
  calories_all <- bind_rows(may, june) %>%
    mutate(participant = name)
  
  # Filter: remove first date and limit to 2025-06-17
  calories_all <- calories_all %>%
    arrange(date) %>%
    filter(date != min(date), date <= as.Date("2025-06-17"), date >= as.Date("2025-05-05"))
  
  return(calories_all) # Return this new table so we can save it
}

# Create table for each participant using the new clean() function
# Experimental group
me1_calories_data <- clean("me1", 
                        "Participant Fitbit data/me1/Physical Activity_GoogleData/calories_2025-05-04.csv",
                        "Participant Fitbit data/me1/Physical Activity_GoogleData/calories_2025-06-01.csv"
)
me2_calories_data <- clean("me2", 
                        "Participant Fitbit data/me2/Physical Activity_GoogleData/calories_2025-05-04.csv", 
                        "Participant Fitbit data/me2/Physical Activity_GoogleData/calories_2025-06-01.csv"
)
me3_calories_data <- clean("me3", 
                        "Participant Fitbit data/me3/Physical Activity_GoogleData/calories_2025-05-03.csv", 
                        "Participant Fitbit data/me3/Physical Activity_GoogleData/calories_2025-06-01.csv"
)
me4_calories_data <- clean("me4", 
                        "Participant Fitbit data/me4/Physical Activity_GoogleData/calories_2025-05-06.csv", 
                        "Participant Fitbit data/me4/Physical Activity_GoogleData/calories_2025-06-01.csv"
)
me5_calories_data <- clean("me5", 
                        "Participant Fitbit data/me5/Physical Activity_GoogleData/calories_2025-05-01.csv", 
                        "Participant Fitbit data/me5/Physical Activity_GoogleData/calories_2025-06-01.csv"
)
me6_calories_data <- clean("me6", 
                        "Participant Fitbit data/me6/Physical Activity_GoogleData/calories_2025-05-03.csv", 
                        "Participant Fitbit data/me6/Physical Activity_GoogleData/calories_2025-06-01.csv"
)
me7_calories_data <- clean("me7", 
                        "Participant Fitbit data/me7/Physical Activity_GoogleData/calories_2025-05-04.csv",
                        "Participant Fitbit data/me7/Physical Activity_GoogleData/calories_2025-06-01.csv"
)
fe1_calories_data <- clean("fe1", 
                        "Participant Fitbit data/fe1/Physical Activity_GoogleData/calories_2025-05-04.csv", 
                        "Participant Fitbit data/fe1/Physical Activity_GoogleData/calories_2025-06-01.csv"
)
fe2_calories_data <- clean("fe2", 
                        "Participant Fitbit data/fe2/Physical Activity_GoogleData/calories_2025-05-06.csv", 
                        "Participant Fitbit data/fe2/Physical Activity_GoogleData/calories_2025-06-01.csv"
)
# fe3_calories_data <- clean("fe3", 
#                         "Participant Fitbit data/fe3/Physical Activity_GoogleData/calories_2025-05-07.csv", 
#                         "Participant Fitbit data/fe3/Physical Activity_GoogleData/calories_2025-06-01.csv"
# )

# Control group
mc1_calories_data <- clean("mc1",
                        "Participant Fitbit data/mc1/calories_2025-05-05.csv", 
                        "Participant Fitbit data/mc1/calories_2025-06-01.csv"
)
mc2_calories_data <- clean("mc2", 
                        "Participant Fitbit data/mc2/Physical Activity_GoogleData/calories_2025-05-01.csv", 
                        "Participant Fitbit data/mc2/Physical Activity_GoogleData/calories_2025-06-01.csv"
)
mc3_calories_data <- clean("mc3", 
                        "Participant Fitbit data/mc3/Physical Activity_GoogleData/calories_2025-05-06.csv", 
                        "Participant Fitbit data/mc3/Physical Activity_GoogleData/calories_2025-06-01.csv"
)
mc4_calories_data <- clean("mc4", 
                        "Participant Fitbit data/mc4/Physical Activity_GoogleData/calories_2025-05-06.csv", 
                        "Participant Fitbit data/mc4/Physical Activity_GoogleData/calories_2025-06-01.csv"
)
mc5_calories_data <- clean("mc5", 
                        "Participant Fitbit data/mc5/Physical Activity_GoogleData/calories_2025-05-08.csv", 
                        "Participant Fitbit data/mc5/Physical Activity_GoogleData/calories_2025-06-01.csv"
)
mc6_calories_data <- clean("mc6", 
                        "Participant Fitbit data/mc6/Physical Activity_GoogleData/calories_2025-05-06.csv", 
                        "Participant Fitbit data/mc6/Physical Activity_GoogleData/calories_2025-06-01.csv"
)
mc7_calories_data <- clean("mc7", 
                        "Participant Fitbit data/mc7/Physical Activity_GoogleData/calories_2025-05-01.csv", 
                        "Participant Fitbit data/mc7/Physical Activity_GoogleData/calories_2025-06-01.csv"
)
fc1_calories_data <- clean("fc1", 
                        "Participant Fitbit data/fc1/Physical Activity_GoogleData/calories_2025-05-06.csv", 
                        "Participant Fitbit data/fc1/Physical Activity_GoogleData/calories_2025-06-01.csv"
)
fc2_calories_data <- clean("fc2", 
                        "Participant Fitbit data/fc2/Physical Activity_GoogleData/calories_2025-05-08.csv", 
                        "Participant Fitbit data/fc2/Physical Activity_GoogleData/calories_2025-06-01.csv"
)
fc3_calories_data <- clean("fc3",
                        "Participant Fitbit data/fc3/Physical Activity_GoogleData/calories_2025-05-14.csv",
                        "Participant Fitbit data/fc3/Physical Activity_GoogleData/calories_2025-06-01.csv"
)


# 2. Combine participant data ---------------------------------------------

# Combine all the data into 1 table

# Experimental group
all_calories_data_exp <- bind_rows(
  me1_calories_data, 
  me2_calories_data, 
  me3_calories_data, 
  me4_calories_data, 
  me5_calories_data, 
  me6_calories_data, 
  me7_calories_data, 
  fe1_calories_data, 
  fe2_calories_data, 
  #fe3_calories_data
)

# Control group
all_calories_data_ctrl <- bind_rows(
  mc1_calories_data, 
  mc2_calories_data, 
  mc3_calories_data, 
  mc4_calories_data, 
  mc5_calories_data, 
  mc6_calories_data, 
  mc7_calories_data, 
  fc1_calories_data, 
  fc2_calories_data, 
  fc3_calories_data
)


# 3. Visualise raw data ---------------------------------------------------

plot_raw_ind <- function(data, participant_name, plot_name) {
  daily_avg <- mean(data$calories, na.rm = TRUE)
  
  p <- ggplot(data, aes(x = date, y = calories)) +
    
    geom_col(fill = hue_pal()(2)[1]) +
    
    geom_hline(
      yintercept = daily_avg, 
      linetype = "dashed", 
      color = hue_pal()(2)[2]
    ) +
    
    annotate(
      "text", 
      x = min(data$date), 
      y =  max(data$calories, na.rm = TRUE) + 0.05 * max(data$calories), # "text", x = min(data$date), y =  daily_avg + 5,  
      label = paste("Average =", round(daily_avg, 1)),
      hjust = 0, 
      color = hue_pal()(2)[2], 
      size = 4
    ) +
    
    labs(
      title = paste("Daily calories:", participant_name),
      x = "Date", y = "Total calories") +
    
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    scale_y_continuous(limits = c(0, max(data$calories) + 0.1 * max(data$calories)), expand = c(0, 0)) + 
    theme_few()
  
  print (p)
  # ggsave(plot_name, plot = p, width = 1000, height = 500, units = "px", dpi = 300)
  
}

# Call plot function
# plot_raw_ind(me1_calories_data, "me1", "me1 calories eda")
# plot_raw_ind(me2_calories_data, "me2", "me2 calories eda")
# plot_raw_ind(me3_calories_data, "me3", "me3 calories eda") # me3 burned 533399.52 kcal on 2025-05-16 -> needs to be adjusted
# plot_raw_ind(me4_calories_data, "me4", "me4 calories eda")
# plot_raw_ind(me5_calories_data, "me5", "me5 calories eda")
# plot_raw_ind(me6_calories_data, "me6", "me6 calories eda")
# plot_raw_ind(me7_calories_data, "me7", "me7 calories eda")
# plot_raw_ind(fe1_calories_data, "fe1", "fe1 calories eda")
# plot_raw_ind(fe2_calories_data, "fe2", "fe2 calories eda")
# plot_raw_ind(fe3_calories_data, "fe3", "fe3 calories eda")
# 
# plot_raw_ind(mc1_calories_data, "mc1", "mc1 calories eda")
# plot_raw_ind(mc2_calories_data, "mc2", "mc2 calories eda")
# plot_raw_ind(mc3_calories_data, "mc3", "mc3 calories eda")
# plot_raw_ind(mc4_calories_data, "mc4", "mc4 calories eda")
# plot_raw_ind(mc5_calories_data, "mc5", "mc5 calories eda")
# plot_raw_ind(mc6_calories_data, "mc6", "mc6 calories eda")
# plot_raw_ind(mc7_calories_data, "mc7", "mc7 calories eda")
# plot_raw_ind(fc1_calories_data, "fc1", "fc1 calories eda")
# plot_raw_ind(fc2_calories_data, "fc2", "fc2 calories eda")
# plot_raw_ind(fc3_calories_data, "fc3", "fc3 calories eda")

# Clean me3's data: remove extreme value
me3_calories_data <- me3_calories_data %>% filter(date != as.Date("2025-05-16"))
plot_raw_ind(me3_calories_data, "me3", "me3 calories eda")

all_calories_data_exp <- bind_rows(
  me1_calories_data, 
  me2_calories_data, 
  me3_calories_data, 
  me4_calories_data, 
  me5_calories_data, 
  me6_calories_data, 
  me7_calories_data, 
  fe1_calories_data, 
  fe2_calories_data, 
  #fe3_calories_data
)


# 4. Calculate group-level averages ---------------------------------------

# Calculate average for the group

# Experimental group
participant_avg_exp <- all_calories_data_exp %>%
  group_by(participant) %>%
  summarise(avg_calories = mean(calories, na.rm = TRUE))

group_avg_exp <- mean(all_calories_data_exp$calories, na.rm = TRUE)

# Control group
participant_avg_ctrl <- all_calories_data_ctrl %>%
  group_by(participant) %>%
  summarise(avg_calories = mean(calories, na.rm = TRUE))

group_avg_ctrl <- mean(all_calories_data_ctrl$calories, na.rm = TRUE)

# Plot this data
plot_raw_group <- function(all_calories_data, group_avg_data, group) {
  
  ggplot(all_calories_data, aes(x = date, y = calories, color = participant)) +
    geom_line(size = 1) +
    
    labs(
      title = paste("Daily calories by Participant:", group),
      x = "Date", 
      y = "Total calories"
    ) +
    
    geom_hline(
      yintercept = group_avg_data, 
      linetype = "dashed", 
      color = hue_pal()(2)[2]
    ) +
    
    annotate(
      "text", x = min(all_calories_data$date), 
      y = max(all_calories_data$calories, na.rm = TRUE) + 0.05 * max(all_calories_data$calories),
      label = paste("Average =", round(group_avg_data, 1)),
      hjust = 0, 
      color = hue_pal()(2)[2], 
      size = 4
    ) +
    
    theme_few() +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    scale_y_continuous(limits = c(0, max(all_calories_data$calories) + 0.1 * max(all_calories_data$calories)), expand = c(0,0)) +
    theme(legend.position = "bottom")
  
}

# Call plot function
plot_raw_group(all_calories_data_exp, group_avg_exp, "Experimental group")
plot_raw_group(all_calories_data_ctrl, group_avg_ctrl, "Control group")


# 5. Outlier detection and removal ----------------------------------------

# Tag participants by group
all_calories_data_exp$group <- "gamified"
all_calories_data_ctrl$group <- "non-gamified"

# Merge both groups into one table
calories_full <- bind_rows(all_calories_data_exp, all_calories_data_ctrl)

# Compute participant-level summary
participant_summary <- calories_full %>%
  group_by(participant, group) %>%
  summarise(avg_calories = mean(calories, na.rm = TRUE), .groups = "drop")

participant_summary_sorted <- participant_summary %>%
  mutate(participant = reorder(as.character(participant), avg_calories))

# Quick intermezzo plot
ggplot(participant_summary_sorted, aes(x = participant, y = avg_calories, fill = group)) +
  geom_col() + 
  labs(
    title = paste("Participant averages plot: calories"),
    x = "Date", y = "Total Active Zone Minutes") +
  theme_few() + 
  scale_y_continuous(limits = c(0, max(participant_summary_sorted$avg_calories) + 10), expand = c(0,0))


# Normality test: QQ plot (visual test)
# Full data -> skewed
qqnorm(participant_summary$avg_calories)
qqline(participant_summary$avg_calories)

# Experimental group
qqnorm(participant_summary$avg_calories[participant_summary$group == "gamified"])
qqline(participant_summary$avg_calories[participant_summary$group == "gamified"])

# Control group -> heavy right-skew : not normally distributed
qqnorm(participant_summary$avg_calories[participant_summary$group == "non-gamified"])
qqline(participant_summary$avg_calories[participant_summary$group == "non-gamified"])

# Normality test: Shapiro-Wilk test
shapiro_full <- shapiro.test(participant_summary$avg_calories) # p = 0.20969082 : normal
shapiro_exp <- shapiro.test(participant_summary$avg_calories[participant_summary$group == "gamified"]) # p = 0.49661826 : normal
shapiro_ctrl <- shapiro.test(participant_summary$avg_calories[participant_summary$group == "non-gamified"]) # p = 0.07308904 : borderline / not normal
# These results show that we'll have to use IQR and a wilcox test for group comparison (t-test no longer works)
print("Shapiro-Wilk normality test values:")
print(c(Experimental = shapiro_exp$p.value,
        Control = shapiro_ctrl$p.value,
        Combined = shapiro_full$p.value))


# Outlier removal: Z-score method 
participant_summary <- participant_summary %>%
  mutate(z_score = scale(avg_calories))

calories_outliers_z <- participant_summary %>% # z limit of 2 used since this is a small dataset that is sensitive to outliers
  filter(abs(z_score) > 2) # None


# Outlier removal: IQR method (for robustness)
iqr <- IQR(participant_summary$avg_calories, na.rm = TRUE)
q1 <- quantile(participant_summary$avg_calories, 0.25, na.rm = TRUE)
q3 <- quantile(participant_summary$avg_calories, 0.75, na.rm = TRUE)
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr

calories_outliers_iqr <- participant_summary %>%
  filter(avg_calories < lower | avg_calories > upper) # None


# Combine outlier ids
calories_outlier_ids <- unique(c(calories_outliers_z$participant, calories_outliers_iqr$participant)) # None

# Don't filter out outliers
# participant_summary_clean <- participant_summary %>%
#   filter(!participant %in% calories_outlier_ids)
# 
# calories_full_clean <- calories_full %>%
#   filter(!participant %in% calories_outlier_ids)
# 
# print("Outliers removed:")
# print(calories_outlier_ids)

participant_summary_clean <- participant_summary
calories_full_clean <- calories_full

# 6. Weekly aggregation ---------------------------------------------------

calories_full_clean$week <- lubridate::floor_date(calories_full_clean$date, unit = "week", week_start = 1)
calories_weekly_avg <- calories_full_clean %>%
  group_by(week, group) %>%
  summarise(mean_calories = mean(calories, na.rm = TRUE), .groups = "drop") %>%
  filter(week > as.Date("2025-04-27"))

calories_weekly_avg$week_num <- lubridate::isoweek(calories_weekly_avg$week)
calories_weekly_avg$week_num <- calories_weekly_avg$week_num - min(calories_weekly_avg$week_num) + 1

# Daily aggregation for extra plot
calories_daily_avg <- calories_full_clean %>%
  group_by(date, group) %>%
  summarise(mean_calories = mean(calories, na.rm = TRUE), .groups = "drop") %>%
  filter(date > as.Date("2025-04-27"))

# 7. Baseline vs post-intervention -----------------------------------------

calories_exp_clean <- calories_full_clean %>%
  filter(group == "gamified") %>%
  mutate(phase = ifelse(date < as.Date("2025-05-12"), "baseline", "intervention"))

calories_baseline_vs_post <- calories_exp_clean %>%
  group_by(participant, phase) %>%
  summarise(avg_calories = mean(calories, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = phase, values_from = avg_calories)

# Wilcoxon signed-rank test
calories_prepost_wilcox <- wilcox.test(calories_baseline_vs_post$baseline, calories_baseline_vs_post$intervention, paired = TRUE) # p = 0.1934



# 8. Group comparison -----------------------------------------------------

# Wilcoxon rank-sum test
calories_group_wilcox <- wilcox.test(avg_calories ~ group, data = participant_summary_clean) # p = 0.2176 > 0.05 : fail to reject null hypothesis


# 9. Compute effect sizes -------------------------------------------------

calories_effsize_mw <- rstatix::wilcox_effsize(participant_summary_clean, avg_calories ~ group) # Small effect size (d = 0.287)


# 10. Correlate with engagement ------------------------------------------

calories_engaged <- participant_summary_clean %>%
  left_join(xp_data, by = "participant")

# Pearson for paramatric correlation
# Spearman for non-paramatric correlation
calories_cor_spearman <- cor.test(calories_engaged$avg_calories, calories_engaged$xp, method = "spearman") # p = 0.01367 < 0.05 : reject null
# cor = 0.769697


# 11. Visualise final results ---------------------------------------------

# Boxplot by group
ggplot(participant_summary_clean, aes(x = group, y = avg_calories, fill = group)) +
  geom_boxplot(color = viridis::viridis(3, option = "cividis")[2]) +
  labs(
    title = "Average Daily Calories per Participant",
    x = "Group",
    y = "Average Calories (kcal)"
  ) +
  theme(legend.position = "none") +
  guides(fill = "none") +
  scale_fill_viridis_d(option = "cividis") +
  theme_few()
ggsave("calories_boxplot.png", plot = last_plot(), width = 7, height = 5, dpi = 300, units = "in")


# Weekly trend
ggplot(calories_weekly_avg, aes(x = week_num, y = mean_calories, color = group)) +
  geom_line(size = 1.2) +
  geom_point() +
  geom_vline(aes(xintercept = 1.5, linetype = "Intervention Start"), 
             color = viridis::viridis(3, option = "cividis")[2]) +
  scale_linetype_manual(name = "", values = c("Intervention Start" = 2)) +
  labs(
    title = "Weekly Group Averages of Calories",
    x = "Week Number",
    y = "Mean Calories (kcal)",
    color = "Group"
  ) +
  scale_x_continuous(
    name = "Week",
    breaks = calories_weekly_avg$week_num  # Show tick for every week
  ) +
  scale_color_viridis_d(option = "cividis") +
  theme_few()
ggsave("calories_lineplot.png", plot = last_plot(), width = 7, height = 5, dpi = 300, units = "in")

# Daily trend
ggplot(calories_daily_avg, aes(x = date, y = mean_calories, color = group)) +
  geom_line(size = 1.2) +
  geom_point() +
  geom_vline(aes(xintercept = as.Date("2025-05-12"), linetype = "Intervention Start"), 
             color = viridis::viridis(3, option = "cividis")[2]) +
  scale_linetype_manual(name = "", values = c("Intervention Start" = 2)) +
  labs(
    title = "Daily Group Averages of Calories",
    x = "Date",
    y = "Mean Calories (kcal)",
    color = "Group"
  ) +
  scale_color_viridis_d(option = "cividis") +
  theme_few()
ggsave("calories_lineplot_daily.png", plot = last_plot(), width = 7, height = 5, dpi = 300, units = "in")

# Paired pre/post plot for both groups
calories_avg_by_group <- calories_full_clean %>%
  mutate(phase = ifelse(date < as.Date("2025-05-12"), "baseline", "intervention")) %>%
  group_by(participant, group, phase) %>%
  summarise(avg_calories = mean(calories, na.rm = TRUE), .groups = "drop") %>%
  group_by(group, phase) %>%
  summarise(mean_calories = mean(avg_calories), .groups = "drop")

# Split the data into two data frames
control_data <- subset(calories_avg_by_group, group == "non-gamified")
gamified_data <- subset(calories_avg_by_group, group == "gamified")

ggplot(calories_avg_by_group, aes(x = phase, y = mean_calories, group = group, color = group)) +
  geom_vline(xintercept = "baseline", color = viridis::viridis(3, option = "cividis")[2], alpha = 0.5) +
  geom_vline(xintercept = "intervention", color = viridis::viridis(3, option = "cividis")[2], alpha = 0.5) +
  
  # Plot non-gamified group first (goes in the back)
  geom_line(data = control_data, aes(x = phase, y = mean_calories, group = group, color = group), size = 1.2) +
  geom_point(data = control_data, aes(x = phase, y = mean_calories, group = group, color = group), size = 3) +
  
  # Plot gamified group second (goes in front)
  geom_line(data = gamified_data, aes(x = phase, y = mean_calories, group = group, color = group), size = 1.2) +
  geom_point(data = gamified_data, aes(x = phase, y = mean_calories, group = group, color = group), size = 3) +
  
  labs(
    title = "Baseline vs. Post-Intervention Calories (Group Average)",
    x = "Phase",
    y = "Average Calories (kcal)",
    color = "Group",
    linetype = element_blank()
  ) +
  scale_x_discrete(expand = c(0, 0.3)) +
  scale_y_continuous(limits = c(0, 1.5 * max(calories_avg_by_group$mean_calories)), expand = c(0, 0)) +
  scale_color_viridis_d(option = "cividis") +
  theme_few()
ggsave("calories_prepost.png", plot = last_plot(), width = 7, height = 5, dpi = 300, units = "in")


# 12. Results Summary Table -----------------------------------------------

calories_results_summary <- tibble::tibble(
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
    "None",
    paste0("p = ", signif(calories_prepost_wilcox$p.value, 4), "*"),
    paste0("p = ", signif(calories_group_wilcox$p.value, 4), "*"),
    paste0("r = ", signif(calories_effsize_mw$effsize, 3)),
    paste0("p = ", signif(calories_cor_spearman$p.value, 3), "**",
           ", ρ = ", signif(calories_cor_spearman$estimate, 3))
  ),
  Interpretation = c(
    "",
    "Possible positive association",
    "Possible positive association",
    "Moderate effect",
    "Significant positive association"
    
  )
)

# View and save
View(calories_results_summary)
write.table(calories_results_summary, pipe("pbcopy"), sep = "\t", row.names = FALSE, quote = FALSE)
write_csv(calories_results_summary, "calories_results_summary.csv")

