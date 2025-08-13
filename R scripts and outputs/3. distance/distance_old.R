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
  may <- may_csv[, c(1, 2)] %>%
    mutate(date = as.Date(timestamp)) %>%
    group_by(date) %>%
    summarise(distance = sum(distance, na.rm = TRUE))
  
  june <- june_csv[, c(1, 2)] %>%
    mutate(date = as.Date(timestamp)) %>%
    group_by(date) %>%
    summarise(distance = sum(distance, na.rm = TRUE))
  
  # Combine into one table
  distance_all <- bind_rows(may, june) %>%
    mutate(participant = name)
  
  # Filter: remove first date and limit to 2025-05_05 to 2025-06-17
  distance_all <- distance_all %>%
    arrange(date) %>%
    filter(date != min(date), date <= as.Date("2025-06-17"), date >= as.Date("2025-05-05"))
  
  return(distance_all) # Return this new table so we can save it
}

# Create table for each participant using the new clean() function
# Experimental group
me1_distance_data <- clean("me1", 
                        "Participant Fitbit data/me1/Physical Activity_GoogleData/distance_2025-05-04.csv",
                        "Participant Fitbit data/me1/Physical Activity_GoogleData/distance_2025-06-01.csv"
)
me2_distance_data <- clean("me2", 
                        "Participant Fitbit data/me2/Physical Activity_GoogleData/distance_2025-05-04.csv", 
                        "Participant Fitbit data/me2/Physical Activity_GoogleData/distance_2025-06-01.csv"
)
me3_distance_data <- clean("me3", 
                        "Participant Fitbit data/me3/Physical Activity_GoogleData/distance_2025-05-03.csv", 
                        "Participant Fitbit data/me3/Physical Activity_GoogleData/distance_2025-06-01.csv"
)
me4_distance_data <- clean("me4", 
                        "Participant Fitbit data/me4/Physical Activity_GoogleData/distance_2025-05-06.csv", 
                        "Participant Fitbit data/me4/Physical Activity_GoogleData/distance_2025-06-01.csv"
)
me5_distance_data <- clean("me5", 
                        "Participant Fitbit data/me5/Physical Activity_GoogleData/distance_2025-05-01.csv", 
                        "Participant Fitbit data/me5/Physical Activity_GoogleData/distance_2025-06-01.csv"
)
me6_distance_data <- clean("me6", 
                        "Participant Fitbit data/me6/Physical Activity_GoogleData/distance_2025-05-03.csv", 
                        "Participant Fitbit data/me6/Physical Activity_GoogleData/distance_2025-06-01.csv"
)
me7_distance_data <- clean("me7", 
                        "Participant Fitbit data/me7/Physical Activity_GoogleData/distance_2025-05-04.csv",
                        "Participant Fitbit data/me7/Physical Activity_GoogleData/distance_2025-06-01.csv"
)
fe1_distance_data <- clean("fe1", 
                        "Participant Fitbit data/fe1/Physical Activity_GoogleData/distance_2025-05-04.csv", 
                        "Participant Fitbit data/fe1/Physical Activity_GoogleData/distance_2025-06-01.csv"
)
fe2_distance_data <- clean("fe2", 
                        "Participant Fitbit data/fe2/Physical Activity_GoogleData/distance_2025-05-06.csv", 
                        "Participant Fitbit data/fe2/Physical Activity_GoogleData/distance_2025-06-01.csv"
)
fe3_distance_data <- clean("fe3", 
                        "Participant Fitbit data/fe3/Physical Activity_GoogleData/distance_2025-05-07.csv", 
                        "Participant Fitbit data/fe3/Physical Activity_GoogleData/distance_2025-06-01.csv"
)

# Control group
mc1_distance_data <- clean("mc1",
                        "Participant Fitbit data/mc1/distance_2025-05-05.csv", 
                        "Participant Fitbit data/mc1/distance_2025-06-01.csv"
)
mc2_distance_data <- clean("mc2", 
                        "Participant Fitbit data/mc2/Physical Activity_GoogleData/distance_2025-05-01.csv", 
                        "Participant Fitbit data/mc2/Physical Activity_GoogleData/distance_2025-06-01.csv"
)
mc3_distance_data <- clean("mc3", 
                        "Participant Fitbit data/mc3/Physical Activity_GoogleData/distance_2025-05-06.csv", 
                        "Participant Fitbit data/mc3/Physical Activity_GoogleData/distance_2025-06-01.csv"
)
mc4_distance_data <- clean("mc4", 
                        "Participant Fitbit data/mc4/Physical Activity_GoogleData/distance_2025-05-06.csv", 
                        "Participant Fitbit data/mc4/Physical Activity_GoogleData/distance_2025-06-01.csv"
)
mc5_distance_data <- clean("mc5", 
                        "Participant Fitbit data/mc5/Physical Activity_GoogleData/distance_2025-05-08.csv", 
                        "Participant Fitbit data/mc5/Physical Activity_GoogleData/distance_2025-06-01.csv"
)
mc6_distance_data <- clean("mc6", 
                        "Participant Fitbit data/mc6/Physical Activity_GoogleData/distance_2025-05-06.csv", 
                        "Participant Fitbit data/mc6/Physical Activity_GoogleData/distance_2025-06-01.csv"
)
mc7_distance_data <- clean("mc7", 
                        "Participant Fitbit data/mc7/Physical Activity_GoogleData/distance_2025-05-01.csv", 
                        "Participant Fitbit data/mc7/Physical Activity_GoogleData/distance_2025-06-01.csv"
)
fc1_distance_data <- clean("fc1", 
                        "Participant Fitbit data/fc1/Physical Activity_GoogleData/distance_2025-05-06.csv", 
                        "Participant Fitbit data/fc1/Physical Activity_GoogleData/distance_2025-06-01.csv"
)
fc2_distance_data <- clean("fc2", 
                        "Participant Fitbit data/fc2/Physical Activity_GoogleData/distance_2025-05-08.csv", 
                        "Participant Fitbit data/fc2/Physical Activity_GoogleData/distance_2025-06-01.csv"
)
fc3_distance_data <- clean("fc3", 
                        "Participant Fitbit data/fc3/Physical Activity_GoogleData/distance_2025-05-14.csv", 
                        "Participant Fitbit data/fc3/Physical Activity_GoogleData/distance_2025-06-01.csv"
)


# 2. Combine participant data ---------------------------------------------

# Combine all the data into 1 table

# Experimental group
all_distance_data_exp <- bind_rows(
  me1_distance_data, 
  me2_distance_data, 
  me3_distance_data, 
  me4_distance_data, 
  me5_distance_data, 
  me6_distance_data, 
  me7_distance_data, 
  fe1_distance_data, 
  fe2_distance_data, 
  fe3_distance_data
)

# Control group
all_distance_data_ctrl <- bind_rows(
  mc1_distance_data, 
  mc2_distance_data, 
  mc3_distance_data, 
  mc4_distance_data, 
  mc5_distance_data, 
  mc6_distance_data, 
  mc7_distance_data, 
  fc1_distance_data, 
  fc2_distance_data, 
  fc3_distance_data
)


# 3. Visualise raw data ---------------------------------------------------

plot_raw_ind <- function(data, participant_name, plot_name) {
  daily_avg <- mean(data$distance, na.rm = TRUE)
  
  p <- ggplot(data, aes(x = date, y = distance)) +
    
    geom_col(fill = hue_pal()(2)[1]) +
    
    geom_hline(
      yintercept = daily_avg, 
      linetype = "dashed", 
      color = hue_pal()(2)[2]
    ) +
    
    annotate(
      "text", 
      x = min(data$date), 
      y =  max(data$distance, na.rm = TRUE) + 0.05 * max(data$distance), # "text", x = min(data$date), y =  daily_avg + 5,  
      label = paste("Average =", round(daily_avg, 1)),
      hjust = 0, 
      color = hue_pal()(2)[2], 
      size = 4
    ) +
    
    labs(
      title = paste("Daily distance:", participant_name),
      x = "Date", y = "Total distance") +
    
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    scale_y_continuous(limits = c(0, max(data$distance) + 0.1 * max(data$distance)), expand = c(0, 0)) + 
    theme_few()
  
  print (p)
  # ggsave(plot_name, plot = p, width = 1000, height = 500, units = "px", dpi = 300)
  
}

# Call plot function
plot_raw_ind(me1_distance_data, "me1", "me1 distance eda")
plot_raw_ind(me2_distance_data, "me2", "me2 distance eda")
plot_raw_ind(me3_distance_data, "me3", "me3 distance eda")
plot_raw_ind(me4_distance_data, "me4", "me4 distance eda")
plot_raw_ind(me5_distance_data, "me5", "me5 distance eda")
plot_raw_ind(me6_distance_data, "me6", "me6 distance eda")
plot_raw_ind(me7_distance_data, "me7", "me7 distance eda")
plot_raw_ind(fe1_distance_data, "fe1", "fe1 distance eda")
plot_raw_ind(fe2_distance_data, "fe2", "fe2 distance eda")
plot_raw_ind(fe3_distance_data, "fe3", "fe3 distance eda")

plot_raw_ind(mc1_distance_data, "mc1", "mc1 distance eda")
plot_raw_ind(mc2_distance_data, "mc2", "mc2 distance eda")
plot_raw_ind(mc3_distance_data, "mc3", "mc3 distance eda")
plot_raw_ind(mc4_distance_data, "mc4", "mc4 distance eda")
plot_raw_ind(mc5_distance_data, "mc5", "mc5 distance eda")
plot_raw_ind(mc6_distance_data, "mc6", "mc6 distance eda")
plot_raw_ind(mc7_distance_data, "mc7", "mc7 distance eda")
plot_raw_ind(fc1_distance_data, "fc1", "fc1 distance eda")
plot_raw_ind(fc2_distance_data, "fc2", "fc2 distance eda")
plot_raw_ind(fc3_distance_data, "fc3", "fc3 distance eda")


# 4. Calculate group-level averages ---------------------------------------

# Calculate average for the group

# Experimental group
participant_avg_exp <- all_distance_data_exp %>%
  group_by(participant) %>%
  summarise(avg_distance = mean(distance, na.rm = TRUE))

group_avg_exp <- mean(all_distance_data_exp$distance, na.rm = TRUE)

# Control group
participant_avg_ctrl <- all_distance_data_ctrl %>%
  group_by(participant) %>%
  summarise(avg_distance = mean(distance, na.rm = TRUE))

group_avg_ctrl <- mean(all_distance_data_ctrl$distance, na.rm = TRUE)

# Plot this data
plot_raw_group <- function(all_distance_data, group_avg_data, group) {
  
  ggplot(all_distance_data, aes(x = date, y = distance, color = participant)) +
    geom_line(size = 1) +
    
    labs(
      title = paste("Daily distance by Participant:", group),
      x = "Date", 
      y = "Total distance"
    ) +
    
    geom_hline(
      yintercept = group_avg_data, 
      linetype = "dashed", 
      color = hue_pal()(2)[2]
    ) +
    
    annotate(
      "text", x = min(all_distance_data$date), 
      y = max(all_distance_data$distance, na.rm = TRUE) + 0.05 * max(all_distance_data$distance),
      label = paste("Average =", round(group_avg_data, 1)),
      hjust = 0, 
      color = hue_pal()(2)[2], 
      size = 4
    ) +
    
    theme_few() +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    scale_y_continuous(limits = c(0, max(all_distance_data$distance) + 0.1 * max(all_distance_data$distance)), expand = c(0,0)) +
    theme(legend.position = "bottom")
  
}

# Call plot function
plot_raw_group(all_distance_data_exp, group_avg_exp, "Experimental group")
plot_raw_group(all_distance_data_ctrl, group_avg_ctrl, "Control group")


# 5. Outlier detection and removal ----------------------------------------

# Tag participants by group
all_distance_data_exp$group <- "gamified"
all_distance_data_ctrl$group <- "non-gamified"

# Merge both groups into one table
distance_full <- bind_rows(all_distance_data_exp, all_distance_data_ctrl)

# Compute participant-level summary
participant_summary <- distance_full %>%
  group_by(participant, group) %>%
  summarise(avg_distance = mean(distance, na.rm = TRUE), .groups = "drop")

participant_summary_sorted <- participant_summary %>%
  mutate(participant = reorder(as.character(participant), avg_distance))

# Quick intermezzo plot
ggplot(participant_summary_sorted, aes(x = participant, y = avg_distance, fill = group)) +
  geom_col() + 
  labs(
    title = paste("Participant averages plot: distance"),
    x = "Date", y = "Total Active Zone Minutes") +
  theme_few() + 
  scale_y_continuous(limits = c(0, max(participant_summary_sorted$avg_distance) + 10), expand = c(0,0))


# Normality test: QQ plot (visual test)
# Full data -> skewed
qqnorm(participant_summary$avg_distance)
qqline(participant_summary$avg_distance)

# Experimental group
qqnorm(participant_summary$avg_distance[participant_summary$group == "gamified"])
qqline(participant_summary$avg_distance[participant_summary$group == "gamified"])

# Control group -> heavy right-skew : not normally distributed
qqnorm(participant_summary$avg_distance[participant_summary$group == "non-gamified"])
qqline(participant_summary$avg_distance[participant_summary$group == "non-gamified"])

# Normality test: Shapiro-Wilk test
shapiro_full <- shapiro.test(participant_summary$avg_distance) # p = 0.1537252 : normal
shapiro_exp <- shapiro.test(participant_summary$avg_distance[participant_summary$group == "gamified"]) # p = 0.3359341 : normal
shapiro_ctrl <- shapiro.test(participant_summary$avg_distance[participant_summary$group == "non-gamified"]) # p = 0.4310262 : normal
# These results show that we'll have to use IQR and a wilcox test for group comparison (t-test no longer works)
print("Shapiro-Wilk normality test values:")
print(c(Experimental = shapiro_exp$p.value,
        Control = shapiro_ctrl$p.value,
        Combined = shapiro_full$p.value))


# Outlier removal: Z-score method 
participant_summary <- participant_summary %>%
  mutate(z_score = scale(avg_distance))

outliers_z <- participant_summary %>% # z limit of 2 used since this is a small dataset that is sensitive to outliers
  filter(abs(z_score) > 2) # Returns me7 : z_score = 2.36


# Outlier removal: IQR method (for robustness)
iqr <- IQR(participant_summary$avg_distance, na.rm = TRUE)
q1 <- quantile(participant_summary$avg_distance, 0.25, na.rm = TRUE)
q3 <- quantile(participant_summary$avg_distance, 0.75, na.rm = TRUE)
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr

outliers_iqr <- participant_summary %>%
  filter(avg_distance < lower | avg_distance > upper)


# Combine outlier ids
outlier_ids <- unique(c(outliers_z$participant, outliers_iqr$participant))

# Don't filter out outliers
# participant_summary_clean <- participant_summary %>%
#   filter(!participant %in% outlier_ids)
# 
# distance_full_clean <- distance_full %>%
#   filter(!participant %in% outlier_ids)
# 
# print("Outliers removed:")
# print(outlier_ids)

participant_summary_clean <- participant_summary
distance_full_clean <- distance_full

# 6. Weekly aggregation ---------------------------------------------------

distance_full_clean$week <- lubridate::floor_date(distance_full_clean$date, unit = "week", week_start = 1)
distance_weekly_avg <- distance_full_clean %>%
  group_by(week, group) %>%
  summarise(mean_distance = mean(distance, na.rm = TRUE), .groups = "drop") %>%
  filter(week > as.Date("2025-04-27"))

distance_weekly_avg$week_num <- lubridate::isoweek(distance_weekly_avg$week)
distance_weekly_avg$week_num <- distance_weekly_avg$week_num - min(distance_weekly_avg$week_num) + 1

# Daily aggregation for extra plot
distance_daily_avg <- distance_full_clean %>%
  group_by(date, group) %>%
  summarise(mean_distance = mean(distance, na.rm = TRUE), .groups = "drop") %>%
  filter(date > as.Date("2025-04-27"))


# 7. Baseline vs post-intervention -----------------------------------------

distance_exp_clean <- distance_full_clean %>%
  filter(group == "gamified") %>%
  mutate(phase = ifelse(date < as.Date("2025-05-12"), "baseline", "intervention"))

distance_baseline_vs_post <- distance_exp_clean %>%
  group_by(participant, phase) %>%
  summarise(avg_distance = mean(distance, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = phase, values_from = avg_distance)

# Paired t-test
t.test(distance_baseline_vs_post$baseline, distance_baseline_vs_post$intervention, paired = TRUE) # ∆ = -1454.279
# While not statistically significant at the 0.05 level, 
# the data indicate a trend toward increased physical activity in the gamified group post-intervention (p = 0.05959).


# 8. Group comparison -----------------------------------------------------

# Independent samples
t.test(avg_distance ~ group, data = participant_summary_clean, var.equal = TRUE) # p = 0.1659 > 0.05 : fail to reject null hypothesis


# 9. Compute effect sizes -------------------------------------------------

# Cohend's d effect size (d = mean diff (exp - ctrl) / standard deviation (ctrl))

t_test_result <- rstatix::t_test(participant_summary_clean, avg_distance ~ group) %>%
  rstatix::add_significance()

effect_size <- rstatix::cohens_d(participant_summary_clean, avg_distance ~ group) # Moderate effect size (d = 0.646)
# The effect size (d = 0.646) indicates a moderate difference in distance between groups, favoring the gamified condition.


# 10. Correlate with engagement ------------------------------------------

distance_engaged <- participant_summary_clean %>%
  left_join(xp_data, by = "participant")

# Pearson for paramatric correlation
cor.test(distance_engaged$avg_distance, distance_engaged$xp, method = "pearson") # p = 5e-04 << 0.05 : reject null
# cor = 0.8931674 


# 11. Visualise final results ---------------------------------------------

# Boxplot by group
ggplot(participant_summary_clean, aes(x = group, y = avg_distance, fill = group)) +
  geom_boxplot() +
  labs(
    title = "Average Daily Distance per Participant",
    x = "Group",
    y = "Average Distance (m)"
  ) +
  theme(legend.position = "none") +
  guides(fill = "none") +
  theme_few()
ggsave("boxplot_distance.png", plot = last_plot(), width = 7, height = 5, dpi = 300, units = "in")


# Weekly trend
ggplot(distance_weekly_avg, aes(x = week_num, y = mean_distance, color = group)) +
  geom_line(size = 1.2) +
  geom_point() +
  geom_vline(aes(xintercept = 1.5, linetype = "Intervention Start"), 
             color = hue_pal()(3)[2]) +
  scale_linetype_manual(name = "", values = c("Intervention Start" = 2)) +
  labs(
    title = "Weekly Group Averages of Distance",
    x = "Week Number",
    y = "Mean Distance (m)",
    color = "Group"
  ) +
  scale_x_continuous(
    name = "Week",
    breaks = distance_weekly_avg$week_num  # Show tick for every week
  ) +
  theme_few()
ggsave("lineplot_distance.png", plot = last_plot(), width = 7, height = 5, dpi = 300, units = "in")

# Daily trend
ggplot(distance_daily_avg, aes(x = date, y = mean_distance, color = group)) +
  geom_line(size = 1.2) +
  geom_point() +
  geom_vline(aes(xintercept = as.Date("2025-05-12"), linetype = "Intervention Start"), 
             color = hue_pal()(3)[2]) +
  scale_linetype_manual(name = "", values = c("Intervention Start" = 2)) +
  labs(
    title = "Daily Group Averages of Distance",
    x = "Date",
    y = "Mean Distance (m)",
    color = "Group"
  ) +
  theme_few()
ggsave("lineplot_daily_distance.png", plot = last_plot(), width = 7, height = 5, dpi = 300, units = "in")

# Paired pre/post plot for experimental group
distance_baseline_vs_post %>%
  tidyr::pivot_longer(cols = c("baseline", "intervention"), names_to = "phase", values_to = "avg_distance") %>%
  ggplot(aes(x = phase, y = avg_distance, group = participant )) +
  geom_line(color = hue_pal()(2)[1]) +
  geom_point(color = hue_pal()(2)[1]) +
  labs(
    title = "Baseline vs. Post-Intervention Distance (Gamified Group)",
    x = "Phase",
    y = "Average Distance (m)"
  ) +
  theme(legend.position = "none") +
  scale_x_discrete(expand = c(0,0.3)) +
  theme_few()
ggsave("prepost_distance.png", plot = last_plot(), width = 7, height = 5, dpi = 300, units = "in")

