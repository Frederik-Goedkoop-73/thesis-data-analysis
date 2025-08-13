library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(gghalves)
library(lubridate)
library(rstatix)
library(gridExtra)


# 1. Import and clean data ------------------------------------------------

clean <- function(name, may_data, june_data, startdate) {
  # Import data
  may_csv <- read_csv(may_data, show_col_types = FALSE)
  june_csv <- read_csv(june_data, show_col_types = FALSE)
  
  # Combine and extract date
  all_data <- bind_rows(may_csv, june_csv) %>%
    mutate(date = as.Date(timestamp)) %>%
    filter(date != min(date), date <= as.Date("2025-06-17"), date >= as.Date(startdate)) # Chose 5-06 for control group -> showed abnormal stats for 5-5
  
  # Count levels per day
  daily_counts <- all_data %>%
    group_by(date, level) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(date) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    select(-count) %>%
    tidyr::pivot_wider(names_from = level, values_from = percentage, values_fill = 0) %>%
    rename_with(~tolower(gsub("_", "", .x))) %>%
    mutate(participant = name) %>%
    relocate(sedentary, .after = date)
    
  
  return(daily_counts)
}

# Create table for each participant using the new clean() function
# Experimental group
me1_activity_level_data <- clean("me1", 
                           "Participant Fitbit data/me1/Physical Activity_GoogleData/activity_level_2025-05-04.csv",
                           "Participant Fitbit data/me1/Physical Activity_GoogleData/activity_level_2025-06-01.csv",
                           "2025-05-05"
)
me2_activity_level_data <- clean("me2", 
                           "Participant Fitbit data/me2/Physical Activity_GoogleData/activity_level_2025-05-04.csv", 
                           "Participant Fitbit data/me2/Physical Activity_GoogleData/activity_level_2025-06-01.csv",
                           "2025-05-05"
)
me3_activity_level_data <- clean("me3", 
                           "Participant Fitbit data/me3/Physical Activity_GoogleData/activity_level_2025-05-03.csv", 
                           "Participant Fitbit data/me3/Physical Activity_GoogleData/activity_level_2025-06-01.csv",
                           "2025-05-05"
)
me4_activity_level_data <- clean("me4", 
                           "Participant Fitbit data/me4/Physical Activity_GoogleData/activity_level_2025-05-06.csv", 
                           "Participant Fitbit data/me4/Physical Activity_GoogleData/activity_level_2025-06-01.csv",
                           "2025-05-05"
)
me5_activity_level_data <- clean("me5", 
                           "Participant Fitbit data/me5/Physical Activity_GoogleData/activity_level_2025-05-01.csv", 
                           "Participant Fitbit data/me5/Physical Activity_GoogleData/activity_level_2025-06-01.csv",
                           "2025-05-05"
)
me6_activity_level_data <- clean("me6", 
                           "Participant Fitbit data/me6/Physical Activity_GoogleData/activity_level_2025-05-03.csv", 
                           "Participant Fitbit data/me6/Physical Activity_GoogleData/activity_level_2025-06-01.csv",
                           "2025-05-05"
)
me7_activity_level_data <- clean("me7", 
                           "Participant Fitbit data/me7/Physical Activity_GoogleData/activity_level_2025-05-04.csv",
                           "Participant Fitbit data/me7/Physical Activity_GoogleData/activity_level_2025-06-01.csv",
                           "2025-05-05"
)
fe1_activity_level_data <- clean("fe1", 
                           "Participant Fitbit data/fe1/Physical Activity_GoogleData/activity_level_2025-05-04.csv", 
                           "Participant Fitbit data/fe1/Physical Activity_GoogleData/activity_level_2025-06-01.csv",
                           "2025-05-05"
)
fe2_activity_level_data <- clean("fe2", 
                           "Participant Fitbit data/fe2/Physical Activity_GoogleData/activity_level_2025-05-06.csv", 
                           "Participant Fitbit data/fe2/Physical Activity_GoogleData/activity_level_2025-06-01.csv",
                           "2025-05-05"
)
# fe3_activity_level_data <- clean("fe3", 
#                            "Participant Fitbit data/fe3/Physical Activity_GoogleData/activity_level_2025-05-07.csv", 
#                            "Participant Fitbit data/fe3/Physical Activity_GoogleData/activity_level_2025-06-01.csv"
# )

# Control group
mc1_activity_level_data <- clean("mc1",
                           "Participant Fitbit data/mc1/activity_level_2025-05-05.csv", 
                           "Participant Fitbit data/mc1/activity_level_2025-06-01.csv",
                           "2025-05-06"
)
mc2_activity_level_data <- clean("mc2", 
                           "Participant Fitbit data/mc2/Physical Activity_GoogleData/activity_level_2025-05-01.csv", 
                           "Participant Fitbit data/mc2/Physical Activity_GoogleData/activity_level_2025-06-01.csv",
                           "2025-05-06"
)
mc3_activity_level_data <- clean("mc3", 
                           "Participant Fitbit data/mc3/Physical Activity_GoogleData/activity_level_2025-05-06.csv", 
                           "Participant Fitbit data/mc3/Physical Activity_GoogleData/activity_level_2025-06-01.csv",
                           "2025-05-06"
)
mc4_activity_level_data <- clean("mc4", 
                           "Participant Fitbit data/mc4/Physical Activity_GoogleData/activity_level_2025-05-06.csv", 
                           "Participant Fitbit data/mc4/Physical Activity_GoogleData/activity_level_2025-06-01.csv",
                           "2025-05-06"
)
mc5_activity_level_data <- clean("mc5", 
                           "Participant Fitbit data/mc5/Physical Activity_GoogleData/activity_level_2025-05-08.csv", 
                           "Participant Fitbit data/mc5/Physical Activity_GoogleData/activity_level_2025-06-01.csv",
                           "2025-05-06"
)
mc6_activity_level_data <- clean("mc6", 
                           "Participant Fitbit data/mc6/Physical Activity_GoogleData/activity_level_2025-05-06.csv", 
                           "Participant Fitbit data/mc6/Physical Activity_GoogleData/activity_level_2025-06-01.csv",
                           "2025-05-06"
)
mc7_activity_level_data <- clean("mc7", 
                           "Participant Fitbit data/mc7/Physical Activity_GoogleData/activity_level_2025-05-01.csv", 
                           "Participant Fitbit data/mc7/Physical Activity_GoogleData/activity_level_2025-06-01.csv",
                           "2025-05-06"
)
fc1_activity_level_data <- clean("fc1", 
                           "Participant Fitbit data/fc1/Physical Activity_GoogleData/activity_level_2025-05-06.csv", 
                           "Participant Fitbit data/fc1/Physical Activity_GoogleData/activity_level_2025-06-01.csv",
                           "2025-05-06"
)
fc2_activity_level_data <- clean("fc2", 
                           "Participant Fitbit data/fc2/Physical Activity_GoogleData/activity_level_2025-05-08.csv", 
                           "Participant Fitbit data/fc2/Physical Activity_GoogleData/activity_level_2025-06-01.csv",
                           "2025-05-06"
)
fc3_activity_level_data <- clean("fc3", 
                           "Participant Fitbit data/fc3/Physical Activity_GoogleData/activity_level_2025-05-14.csv", 
                           "Participant Fitbit data/fc3/Physical Activity_GoogleData/activity_level_2025-06-01.csv",
                           "2025-05-06"
)


# 2. Combine participant data ---------------------------------------------

# Combine all the data into 1 table

# Experimental group
all_activity_level_data_exp <- bind_rows(
  me1_activity_level_data, 
  me2_activity_level_data, 
  me3_activity_level_data, 
  me4_activity_level_data, 
  me5_activity_level_data, 
  me6_activity_level_data, 
  me7_activity_level_data, 
  fe1_activity_level_data, 
  fe2_activity_level_data, 
  # fe3_activity_level_data
)

# Control group
all_activity_level_data_ctrl <- bind_rows(
  mc1_activity_level_data, 
  mc2_activity_level_data, 
  mc3_activity_level_data, 
  mc4_activity_level_data, 
  #mc5_activity_level_data, 
  mc6_activity_level_data, 
  mc7_activity_level_data, 
  fc1_activity_level_data, 
  fc2_activity_level_data, 
  fc3_activity_level_data
)


# 3. Visualise raw data ---------------------------------------------------

plot_raw_ind <- function(data, participant_name, plot_name) {
  # Convert to long format for stacked bars
  data_long <- data %>%
    select(date, veryactive, moderatelyactive, lightlyactive, sedentary) %>%
    tidyr::pivot_longer(
      cols = -date,
      names_to = "activity_level",
      values_to = "percentage"
    )
  
  data_long$activity_level <- factor(
    data_long$activity_level,
    levels = c("sedentary", "lightlyactive", "moderatelyactive", "veryactive")
  )
  
  View(data_long)
  
  # Create stacked bar plot
  p <- ggplot(data_long, aes(x = date, y = percentage, fill = activity_level)) +
    geom_col() +
    scale_fill_manual(
      values = c(
        veryactive = viridis::viridis(4, option = "cividis")[4],
        moderatelyactive = viridis::viridis(4, option = "cividis")[3],
        lightlyactive = viridis::viridis(4, option = "cividis")[1],
        sedentary = viridis::viridis(4, option = "cividis")[2]
      ),
      name = "Activity Level",
      labels = c("Sedentary", "Lightly Active", "Moderately Active", "Very Active")
    ) +
    labs(
      title = paste("Daily Activity Composition:", participant_name),
      x = "Date",
      y = "Percentage of Day (%)"
    ) +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    theme_few()
  
  print(p)
  # ggsave(plot_name, plot = p, width = 1000, height = 500, units = "px", dpi = 300)
  
}


# Call plot function
# plot_raw_ind(me1_activity_level_data, "me1", "me1 activity_level eda")
# plot_raw_ind(me2_activity_level_data, "me2", "me2 activity_level eda")
# plot_raw_ind(me3_activity_level_data, "me3", "me3 activity_level eda")
# plot_raw_ind(me4_activity_level_data, "me4", "me4 activity_level eda")
# plot_raw_ind(me5_activity_level_data, "me5", "me5 activity_level eda")
# plot_raw_ind(me6_activity_level_data, "me6", "me6 activity_level eda")
# plot_raw_ind(me7_activity_level_data, "me7", "me7 activity_level eda")
# plot_raw_ind(fe1_activity_level_data, "fe1", "fe1 activity_level eda")
# plot_raw_ind(fe2_activity_level_data, "fe2", "fe2 activity_level eda")

# plot_raw_ind(mc1_activity_level_data, "mc1", "mc1 activity_level eda")
# plot_raw_ind(mc2_activity_level_data, "mc2", "mc2 activity_level eda")
# plot_raw_ind(mc3_activity_level_data, "mc3", "mc3 activity_level eda")
# plot_raw_ind(mc4_activity_level_data, "mc4", "mc4 activity_level eda")x
# plot_raw_ind(mc5_activity_level_data, "mc5", "mc5 activity_level eda")
# plot_raw_ind(mc6_activity_level_data, "mc6", "mc6 activity_level eda")
# plot_raw_ind(mc7_activity_level_data, "mc7", "mc7 activity_level eda")
# plot_raw_ind(fc1_activity_level_data, "fc1", "fc1 activity_level eda")
# plot_raw_ind(fc2_activity_level_data, "fc2", "fc2 activity_level eda")
# plot_raw_ind(fc3_activity_level_data, "fc3", "fc3 activity_level eda")


# 4. Calculate group-level averages ---------------------------------------

# Calculate average for the group
# Experimental group
participant_avg_exp <- all_activity_level_data_exp %>%
  group_by(participant) %>%
  summarise(
    avg_sedentary = mean(sedentary, na.rm = TRUE),
    avg_light = mean(lightlyactive, na.rm = TRUE),
    avg_moderate = mean(moderatelyactive, na.rm = TRUE),
    avg_very = mean(veryactive, na.rm = TRUE),
    .groups = "drop"
  )

group_avg_exp <- c(
  mean(participant_avg_exp$avg_sedentary, na.rm = TRUE),
  mean(participant_avg_exp$avg_light, na.rm = TRUE),
  mean(participant_avg_exp$avg_moderate, na.rm = TRUE),
  mean(participant_avg_exp$avg_very, na.rm = TRUE)
)
  

# Control group
participant_avg_ctrl <- all_activity_level_data_ctrl %>%
  group_by(participant) %>%
  summarise(
    avg_sedentary = mean(sedentary, na.rm = TRUE),
    avg_light = mean(lightlyactive, na.rm = TRUE),
    avg_moderate = mean(moderatelyactive, na.rm = TRUE),
    avg_very = mean(veryactive, na.rm = TRUE),
    .groups = "drop"
  )

group_avg_ctrl <- c(
  mean(participant_avg_ctrl$avg_sedentary, na.rm = TRUE),
  mean(participant_avg_ctrl$avg_light, na.rm = TRUE),
  mean(participant_avg_ctrl$avg_moderate, na.rm = TRUE),
  mean(participant_avg_ctrl$avg_very, na.rm = TRUE)
)

# Plot this data
plot_raw_group <- function(all_activity_level_data, group) {
  # Convert to long format
  data_long <- all_activity_level_data %>%
    select(date, participant, sedentary, lightlyactive, moderatelyactive, veryactive) %>%
    tidyr::pivot_longer(
      cols = c(sedentary, lightlyactive, moderatelyactive, veryactive),
      names_to = "activity_level",
      values_to = "percentage"
    )
  
  # Order legend and stacking
  data_long$activity_level <- factor(
    data_long$activity_level,
    levels = c("sedentary", "lightlyactive", "moderatelyactive", "veryactive")
  )
  
  # Group average per day across all participants (optional)
  group_avg <- data_long %>%
    group_by(date, activity_level) %>%
    summarise(mean_percentage = mean(percentage, na.rm = TRUE), .groups = "drop")
  
  # Plot
  ggplot(group_avg, aes(x = date, y = mean_percentage, fill = activity_level)) +
    geom_col() +
    scale_fill_manual(
      values = c(
        veryactive = viridis::viridis(4, option = "cividis")[4],
        moderatelyactive = viridis::viridis(4, option = "cividis")[3],
        lightlyactive = viridis::viridis(4, option = "cividis")[1],
        sedentary = viridis::viridis(4, option = "cividis")[2]
      ),
      name = "Activity Level",
      labels = c("Sedentary", "Lightly Active", "Moderately Active", "Very Active")
    ) +
    labs(
      title = paste("Daily Group Activity Composition:", group),
      x = "Date",
      y = "Percentage of Day (%)"
    ) +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    theme_few()
}

# Call plot function
plot_raw_group(all_activity_level_data_exp, "Experimental group")
#ggsave("activity_level_experimental.png", plot = last_plot(), width = 7, height = 5, dpi = 300, units = "in")
plot_raw_group(all_activity_level_data_ctrl, "Control group")
#ggsave("activity_level_control.png", plot = last_plot(), width = 7, height = 5, dpi = 300, units = "in")


# 5. Outlier detection and removal ----------------------------------------

# Tag participants by group
all_activity_level_data_exp$group <- "gamified"
all_activity_level_data_ctrl$group <- "non-gamified"

# Merge both groups into one table
activity_level_full <- bind_rows(all_activity_level_data_exp, all_activity_level_data_ctrl)

# Compute participant-level summary
participant_summary <- activity_level_full %>%
  group_by(participant, group) %>%
  summarise(
    avg_sedentary = mean(sedentary, na.rm = TRUE),
    avg_light = mean(lightlyactive, na.rm = TRUE),
    avg_moderate = mean(moderatelyactive, na.rm = TRUE),
    avg_very = mean(veryactive, na.rm = TRUE),
    .groups = "drop"
  )


# Normality test: QQ plot (visual test)
par(mfrow = c(2, 2))  # 2x2 grid

qqnorm(participant_summary$avg_sedentary, main = "Sedentary")
qqline(participant_summary$avg_sedentary)

qqnorm(participant_summary$avg_light, main = "Lightly Active")
qqline(participant_summary$avg_light)

qqnorm(participant_summary$avg_moderate, main = "Moderately Active")
qqline(participant_summary$avg_moderate)

qqnorm(participant_summary$avg_very, main = "Very Active")
qqline(participant_summary$avg_very)

# Normality test: Shapiro-Wilk test
shapiro.test(participant_summary$avg_sedentary)
shapiro.test(participant_summary$avg_light)
shapiro.test(participant_summary$avg_moderate)
shapiro.test(participant_summary$avg_very)

# Per group
participant_summary %>%
  group_by(group) %>%
  summarise(
    shapiro_sedentary = shapiro.test(avg_sedentary)$p.value,
    shapiro_light = shapiro.test(avg_light)$p.value,
    shapiro_moderate = shapiro.test(avg_moderate)$p.value,
    shapiro_very = shapiro.test(avg_very)$p.value
  )



# Outlier removal: Z-score method 
z_scores <- participant_summary %>%
  mutate(across(starts_with("avg_"), ~ scale(.)[,1], .names = "z_{.col}"))

outliers_z <- z_scores %>%
  filter(
    abs(z_avg_sedentary) > 2 |
      abs(z_avg_light) > 2 |
      abs(z_avg_moderate) > 2 |
      abs(z_avg_very) > 2
  )


# Outlier removal: IQR method (for robustness)
find_iqr_outliers <- function(df, var) {
  iqr <- IQR(df[[var]], na.rm = TRUE)
  q1 <- quantile(df[[var]], 0.25, na.rm = TRUE)
  q3 <- quantile(df[[var]], 0.75, na.rm = TRUE)
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  df %>% filter(.data[[var]] < lower | .data[[var]] > upper)
}

outliers_iqr <- bind_rows(
  find_iqr_outliers(participant_summary, "avg_sedentary"),
  find_iqr_outliers(participant_summary, "avg_light"),
  find_iqr_outliers(participant_summary, "avg_moderate"),
  find_iqr_outliers(participant_summary, "avg_very")
) %>% distinct(participant)


# Combine outlier ids
act_outlier_ids <- unique(c(outliers_z$participant, outliers_iqr$participant))

# Don't filter out outliers (flagged)


participant_summary_clean <- participant_summary
activity_level_full_clean <- activity_level_full

# 6. Weekly aggregation ---------------------------------------------------

# Add 'week' variable
activity_level_full_clean$week <- lubridate::floor_date(activity_level_full_clean$date, unit = "week", week_start = 1)

# Calculate weekly group means per activity level
activity_level_weekly_avg <- activity_level_full_clean %>%
  group_by(week, group) %>%
  summarise(
    sedentary = mean(sedentary, na.rm = TRUE),
    lightlyactive = mean(lightlyactive, na.rm = TRUE),
    moderatelyactive = mean(moderatelyactive, na.rm = TRUE),
    veryactive = mean(veryactive, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(week > as.Date("2025-04-27"))

# Add a week number
activity_level_weekly_avg$week_num <- lubridate::isoweek(activity_level_weekly_avg$week)
activity_level_weekly_avg$week_num <- activity_level_weekly_avg$week_num - min(activity_level_weekly_avg$week_num) + 1



# 7. Baseline vs post-intervention -----------------------------------------

# Tag phases within gamified group
activity_level_exp_clean <- activity_level_full_clean %>%
  filter(group == "gamified") %>%
  mutate(phase = ifelse(date < as.Date("2025-05-12"), "baseline", "intervention"))

# Compute average per activity level per phase per participant
activity_level_baseline_vs_post <- activity_level_exp_clean %>%
  group_by(participant, phase) %>%
  summarise(
    sedentary = mean(sedentary, na.rm = TRUE),
    lightlyactive = mean(lightlyactive, na.rm = TRUE),
    moderatelyactive = mean(moderatelyactive, na.rm = TRUE),
    veryactive = mean(veryactive, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(names_from = phase, values_from = c(sedentary, lightlyactive, moderatelyactive, veryactive))

act_prepost_wilcox_sed <-  wilcox.test(activity_level_baseline_vs_post$sedentary_baseline, activity_level_baseline_vs_post$sedentary_intervention, paired = TRUE)
act_prepost_wilcox_l <-  wilcox.test(activity_level_baseline_vs_post$lightlyactive_baseline, activity_level_baseline_vs_post$lightlyactive_intervention, paired = TRUE)
act_prepost_wilcox_m <-  wilcox.test(activity_level_baseline_vs_post$moderatelyactive_baseline, activity_level_baseline_vs_post$moderatelyactive_intervention, paired = TRUE)
act_prepost_wilcox_v <-  wilcox.test(activity_level_baseline_vs_post$veryactive_baseline, activity_level_baseline_vs_post$veryactive_intervention, paired = TRUE)



# 8. Group comparison -----------------------------------------------------

# Compare each activity level between groups
act_group_wilcox_sed <-  wilcox.test(avg_sedentary ~ group, data = participant_summary_clean)
act_group_wilcox_l <-  wilcox.test(avg_light ~ group, data = participant_summary_clean)
act_group_wilcox_m <-  wilcox.test(avg_moderate ~ group, data = participant_summary_clean)
act_group_wilcox_v <-  wilcox.test(avg_very ~ group, data = participant_summary_clean)



# 9. Compute effect sizes -------------------------------------------------

# Sedentary
act_group_effsize_mw_sed <- rstatix::wilcox_effsize(participant_summary_clean, avg_sedentary ~ group)

# Lightly Active
act_group_effsize_mw_l <- rstatix::wilcox_effsize(participant_summary_clean, avg_light ~ group)

# Moderately Active
act_group_effsize_mw_m <- rstatix::wilcox_effsize(participant_summary_clean, avg_moderate ~ group)

# Very Active
act_group_effsize_mw_v <- rstatix::wilcox_effsize(participant_summary_clean, avg_very ~ group)


# 10. Correlate with engagement ------------------------------------------

activity_level_engaged <- participant_summary_clean %>%
  left_join(xp_data, by = "participant")

# Pearson correlations for each activity level vs XP
act_cor_spearman_sed <- cor.test(activity_level_engaged$avg_sedentary, activity_level_engaged$xp, method = "spearman")
act_cor_spearman_l <- cor.test(activity_level_engaged$avg_light, activity_level_engaged$xp, method = "spearman")
act_cor_spearman_m <- cor.test(activity_level_engaged$avg_moderate, activity_level_engaged$xp, method = "spearman")
act_cor_spearman_v <- cor.test(activity_level_engaged$avg_very, activity_level_engaged$xp, method = "spearman")


# 11. Visualise final results ---------------------------------------------

activity_level_long <- participant_summary_clean %>%
  pivot_longer(
    cols = starts_with("avg_"),
    names_to = "activity_level",
    values_to = "average"
  ) %>%
  mutate(
    activity_level = recode(activity_level,
                            "avg_sedentary"  = "Sedentary",
                            "avg_light"      = "Lightly Active",
                            "avg_moderate"   = "Moderately Active",
                            "avg_very"       = "Very Active"
    ),
    activity_level = factor(activity_level,
                            levels = c("Sedentary","Lightly Active","Moderately Active","Very Active")
    )
  )

# One plot: x = metric, left half = gamified, right half = non-gamified
point_nudge <- 0.16

ggplot(activity_level_long, aes(x = activity_level, y = average)) +
  # Left half (gamified)
  geom_half_violin(
    data = ~ dplyr::filter(.x, group == "gamified"),
    aes(fill = group), side = "l", width = .9, trim = FALSE, alpha = .8, color = NA
  ) +
  geom_half_boxplot(
    data = ~ dplyr::filter(.x, group == "gamified"),
    side = "l", width = .2, outlier.shape = NA
  ) +
  
  # Right half (non-gamified)
  geom_half_violin(
    data = ~ dplyr::filter(.x, group == "non-gamified"),
    aes(fill = group), side = "r", width = .9, trim = FALSE, alpha = 1, color = NA
  ) +
  geom_half_boxplot(
    data = ~ dplyr::filter(.x, group == "non-gamified"),
    side = "r", width = .2, outlier.shape = NA
  ) +
  
  # Cosmetics
  scale_fill_manual(values = c("gamified" = viridis::viridis(2, option = "cividis")[1], "non-gamified" = viridis::viridis(2, option = "cividis")[2])) +
  labs(
    title = "Activity Level Distributions by Group",
    x = "Activity Level",
    y = "Average % of Day",
    fill = "Group"
  ) +
  theme_few() +
  theme(legend.position = "top")


ggsave("activity_level_violinplot.png", plot = last_plot(), width = 7, height = 5, dpi = 300, units = "in")


# Weekly trend
activity_level_weekly_long <- activity_level_weekly_avg %>%
  tidyr::pivot_longer(
    cols = c(sedentary, lightlyactive, moderatelyactive, veryactive),
    names_to = "activity_level",
    values_to = "mean_percentage"
  ) %>%
  dplyr::mutate(
    activity_level = dplyr::recode(activity_level,
                                   "sedentary"        = "Sedentary",
                                   "lightlyactive"    = "Lightly Active",
                                   "moderatelyactive" = "Moderately Active",
                                   "veryactive"       = "Very Active"
    ),
    activity_level = factor(activity_level,
                            levels = c("Sedentary","Lightly Active","Moderately Active","Very Active")
    )
  )

intervention_date <- as.Date("2025-05-12")
intervention_week <- lubridate::floor_date(intervention_date, unit = "week", week_start = 1)

# Map weeks to 1..N exactly like your week_num did
weeks_sorted <- sort(unique(activity_level_weekly_avg$week))
intervention_week_num <- which(weeks_sorted == intervention_week)
# If intervention splits a week, you can add .5 to draw between ticks:
intervention_x <- intervention_week_num - 0.5

p_faceted <- ggplot(
  activity_level_weekly_long,
  aes(x = week_num, y = mean_percentage, color = activity_level)
) +
  geom_line(size = 1.2) +
  geom_point() +
  # intervention line & legend entry (same style as your AZM plot)
  geom_vline(
    aes(xintercept = intervention_x, linetype = "Intervention Start"),
    color = viridis::viridis(3, option = "cividis")[2]
  ) +
  scale_linetype_manual(name = "", values = c("Intervention Start" = 2)) +
  labs(
    title = "Weekly Group Averages of Activity Levels",
    x = "Week Number",
    y = "Mean % of Day",
    color = "Activity Level"
  ) +
  # ticks for every observed week number
  scale_x_continuous(breaks = sort(unique(activity_level_weekly_long$week_num))) +
  scale_color_viridis_d(option = "cividis") +
  facet_wrap(~ group, ncol = 2) +
  theme_few() +
  theme(legend.position = "bottom")

ggsave("activity_level_lineplot.png", plot = p_faceted, width = 14, height = 5, dpi = 300, units = "in")


# Paired pre/post plot for experimental group
# 1) Tag phases for ALL participants (both groups), same cutoff you used
intervention_date <- as.Date("2025-05-12")

activity_levels_phased <- activity_level_full_clean %>%
  mutate(
    phase = ifelse(date < intervention_date, "baseline", "intervention"),
    phase = factor(phase, levels = c("baseline", "intervention"))
  )

# 2) Participant-level averages per phase (keeps pairing intact)
al_part_phase <- activity_levels_phased %>%
  group_by(participant, group, phase) %>%
  summarise(
    sedentary        = mean(sedentary, na.rm = TRUE),
    lightlyactive    = mean(lightlyactive, na.rm = TRUE),
    moderatelyactive = mean(moderatelyactive, na.rm = TRUE),
    veryactive       = mean(veryactive, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(sedentary, lightlyactive, moderatelyactive, veryactive),
    names_to = "activity_level",
    values_to = "mean_pct"
  ) %>%
  mutate(
    activity_level = recode(activity_level,
                            "sedentary"        = "Sedentary",
                            "lightlyactive"    = "Lightly Active",
                            "moderatelyactive" = "Moderately Active",
                            "veryactive"       = "Very Active"
    ),
    activity_level = factor(activity_level,
                            levels = c("Sedentary","Lightly Active","Moderately Active","Very Active")
    )
  )

# 3) Group-level mean across participants per phase & activity level
al_group_phase <- al_part_phase %>%
  group_by(group, phase, activity_level) %>%
  summarise(mean_pct = mean(mean_pct, na.rm = TRUE), .groups = "drop")

# 4) Split for draw order (optional: control behind, gamified in front)
control_phase  <- al_group_phase %>% filter(group == "non-gamified")
gamified_phase <- al_group_phase %>% filter(group == "gamified")

# 5) Plot: paired baseline→intervention lines, faceted by activity level
p_prepost <- ggplot(al_group_phase, aes(x = phase, y = mean_pct, group = group, color = group)) +
  # vertical guides to match your AZM example
  geom_vline(xintercept = as.numeric(factor("baseline",     levels = levels(al_group_phase$phase))),
             color = viridis::viridis(3, option = "cividis")[2], alpha = 0.5) +
  geom_vline(xintercept = as.numeric(factor("intervention", levels = levels(al_group_phase$phase))),
             color = viridis::viridis(3, option = "cividis")[2], alpha = 0.5) +
  
  # draw control first (back), then gamified (front)
  geom_line(data = control_phase,  aes(x = phase, y = mean_pct), linewidth = 1.2) +
  geom_point(data = control_phase, aes(x = phase, y = mean_pct), size = 3) +
  geom_line(data = gamified_phase, aes(x = phase, y = mean_pct), linewidth = 1.2) +
  geom_point(data = gamified_phase, aes(x = phase, y = mean_pct), size = 3) +
  
  facet_wrap(~ activity_level, nrow = 1) +
  labs(
    title = "Baseline vs Intervention — Group Means by Activity Level",
    x = "Phase",
    y = "Mean % of Day",
    color = "Group"
  ) +
  scale_x_discrete(expand = c(0, 0.3)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_color_viridis_d(option = "cividis") +
  theme_few() +
  theme(legend.position = "bottom")

print(p_prepost)

ggsave("activity_level_prepost.png", plot = last_plot(), width = 14, height = 5, dpi = 300, units = "in")
ggsave("activity_level_prepost_thin.png", plot = last_plot(), width = 7, height = 5, dpi = 300, units = "in")



# 12. Results Summary Table -----------------------------------------------

# act_results_summary_with_mc5 <- tibble::tibble(
#   Analysis = c(
#     "Sedentary",
#     "Outlier(s)",
#     "Pre vs Post",
#     "Group comparison",
#     "Effect size",
#     "Engagement correlation",
#     
#     "Lightly Active",
#     "Pre vs Post",
#     "Group comparison",
#     "Effect size",
#     "Engagement correlation",
#     
#     "Moderately Active",
#     "Pre vs Post",
#     "Group comparison",
#     "Effect size",
#     "Engagement correlation",
#     
#     "Very Active",
#     "Pre vs Post",
#     "Group comparison",
#     "Effect size",
#     "Engagement correlation"
#   ),
#   `Test Used` = c(
#     "",
#     "IQR & Z-score",
#     "Wilcoxon signed-rank test",
#     "Wilcoxon rank-sum test",
#     "Wilcoxon effect size",
#     "Spearman correlation",
#     
#     "",
#     "Wilcoxon signed-rank test",
#     "Wilcoxon rank-sum test",
#     "Wilcoxon effect size",
#     "Spearman correlation",
#     
#     "",
#     "Wilcoxon signed-rank test",
#     "Wilcoxon rank-sum test",
#     "Wilcoxon effect size",
#     "Spearman correlation",
#     
#     "",
#     "Wilcoxon signed-rank test",
#     "Wilcoxon rank-sum test",
#     "Wilcoxon effect size",
#     "Spearman correlation"
#   ),
#   Result = c(
#     "",
#     "mc5, me7",
#     paste0("p = ", signif(act_prepost_wilcox_sed$p.value, 4)),
#     paste0("p = ", signif(act_group_wilcox_sed$p.value, 4)),
#     paste0("r = ", signif(act_group_effsize_mw_sed$effsize, 3)),
#     paste0("p = ", signif(act_cor_spearman_sed$p.value, 3), "*",
#            ", ρ = ", signif(act_cor_spearman_sed$estimate, 3)),
#     
#     "",
#     paste0("p = ", signif(act_prepost_wilcox_l$p.value, 4)),
#     paste0("p = ", signif(act_group_wilcox_l$p.value, 4)),
#     paste0("r = ", signif(act_group_effsize_mw_l$effsize, 3)),
#     paste0("p = ", signif(act_cor_spearman_l$p.value, 3),
#            ", ρ = ", signif(act_cor_spearman_l$estimate, 3)),
#     
#     "",
#     paste0("p = ", signif(act_prepost_wilcox_m$p.value, 4)),
#     paste0("p = ", signif(act_group_wilcox_m$p.value, 4)),
#     paste0("r = ", signif(act_group_effsize_mw_m$effsize, 3)),
#     paste0("p = ", signif(act_cor_spearman_m$p.value, 3), "***",
#            ", ρ = ", signif(act_cor_spearman_m$estimate, 3)),
#     
#     "",
#     paste0("p = ", signif(act_prepost_wilcox_v$p.value, 4), "**"),
#     paste0("p = ", signif(act_group_wilcox_v$p.value, 4)),
#     paste0("r = ", signif(act_group_effsize_mw_v$effsize, 3)),
#     paste0("p = ", signif(act_cor_spearman_v$p.value, 3), "*",
#            ", ρ = ", signif(act_cor_spearman_v$estimate, 3))
#   ),
#   Interpretation = c(
#     "",
#     "mc5 tested, me7 retained",
#     "No significant association",
#     "No significant association",
#     "Small effect",
#     "Possible negative association",
#     
#     "",
#     "No significant association",
#     "No significant association",
#     "Small effect",
#     "Negligible association",
#     
#     "",
#     "No significant association",
#     "No significant association",
#     "Small effect",
#     "Significant positive association",
#     
#     "",
#     "Significant positive association",
#     "No significant association",
#     "Moderate effect",
#     "Possible positive association"
#     
#   )
# )
# 
# # View and save
# View(act_results_summary_with_mc5)
# write.table(act_results_summary_with_mc5, pipe("pbcopy"), sep = "\t", row.names = FALSE, quote = FALSE)
# write_csv(act_results_summary_with_mc5, "activity_level_results_summary_with_mc5.csv")


# Second analysis without mc5 (commented out data import)
act_results_summary <- tibble::tibble(
  Analysis = c(
    "Sedentary",
    "Outlier(s)",
    "Pre vs Post",
    "Group comparison",
    "Effect size",
    "Engagement correlation",
    
    "Lightly Active",
    "Pre vs Post",
    "Group comparison",
    "Effect size",
    "Engagement correlation",
    
    "Moderately Active",
    "Pre vs Post",
    "Group comparison",
    "Effect size",
    "Engagement correlation",
    
    "Very Active",
    "Pre vs Post",
    "Group comparison",
    "Effect size",
    "Engagement correlation"
  ),
  `Test Used` = c(
    "",
    "IQR & Z-score",
    "Wilcoxon signed-rank test",
    "Wilcoxon rank-sum test",
    "Wilcoxon effect size",
    "Spearman correlation",
    
    "",
    "Wilcoxon signed-rank test",
    "Wilcoxon rank-sum test",
    "Wilcoxon effect size",
    "Spearman correlation",
    
    "",
    "Wilcoxon signed-rank test",
    "Wilcoxon rank-sum test",
    "Wilcoxon effect size",
    "Spearman correlation",
    
    "",
    "Wilcoxon signed-rank test",
    "Wilcoxon rank-sum test",
    "Wilcoxon effect size",
    "Spearman correlation"
  ),
  Result = c(
    "",
    "mc5, me7",
    paste0("p = ", signif(act_prepost_wilcox_sed$p.value, 4)),
    paste0("p = ", signif(act_group_wilcox_sed$p.value, 4)),
    paste0("r = ", signif(act_group_effsize_mw_sed$effsize, 3)),
    paste0("p = ", signif(act_cor_spearman_sed$p.value, 3), "*",
           ", ρ = ", signif(act_cor_spearman_sed$estimate, 3)),
    
    "",
    paste0("p = ", signif(act_prepost_wilcox_l$p.value, 4)),
    paste0("p = ", signif(act_group_wilcox_l$p.value, 4)),
    paste0("r = ", signif(act_group_effsize_mw_l$effsize, 3)),
    paste0("p = ", signif(act_cor_spearman_l$p.value, 3),
           ", ρ = ", signif(act_cor_spearman_l$estimate, 3)),
    
    "",
    paste0("p = ", signif(act_prepost_wilcox_m$p.value, 4)),
    paste0("p = ", signif(act_group_wilcox_m$p.value, 4)),
    paste0("r = ", signif(act_group_effsize_mw_m$effsize, 3)),
    paste0("p = ", signif(act_cor_spearman_m$p.value, 3), "***",
           ", ρ = ", signif(act_cor_spearman_m$estimate, 3)),
    
    "",
    paste0("p = ", signif(act_prepost_wilcox_v$p.value, 4), "**"),
    paste0("p = ", signif(act_group_wilcox_v$p.value, 4)),
    paste0("r = ", signif(act_group_effsize_mw_v$effsize, 3)),
    paste0("p = ", signif(act_cor_spearman_v$p.value, 3), "*",
           ", ρ = ", signif(act_cor_spearman_v$estimate, 3))
  ),
  Interpretation = c(
    "",
    "mc5 removed, me7 retained",
    "No significant association",
    "No significant association",
    "Small effect",
    "Possible negative association",
    
    "",
    "No significant association",
    "No significant association",
    "Small effect",
    "Negligible association",
    
    "",
    "No significant association",
    "No significant association",
    "Small effect",
    "Significant positive association",
    
    "",
    "Significant positive association",
    "No significant association",
    "Moderate effect",
    "Possible positive association"
    
  )
)

# View and save
View(act_results_summary)
write.table(act_results_summary, pipe("pbcopy"), sep = "\t", row.names = FALSE, quote = FALSE)
write_csv(act_results_summaryt, "activity_level_results_summary.csv")
