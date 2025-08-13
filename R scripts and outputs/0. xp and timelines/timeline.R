# For this script I will be working with the data from AZM.R

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(scales)
library(gridExtra)

# List of participant dataframes, adjust with your actual data
control_participant_list <- list(
  fc1_azm_data, 
  fc2_azm_data, 
  fc3_azm_data,
  mc1_azm_data,
  mc2_azm_data,
  mc3_azm_data,
  mc4_azm_data,
  mc5_azm_data,
  mc6_azm_data,
  mc7_azm_data
  )

experiment_participant_list <- list(
  fe1_azm_data, 
  fe2_azm_data, 
  fe3_azm_data,
  me1_azm_data,
  me2_azm_data,
  me3_azm_data,
  me4_azm_data,
  me5_azm_data,
  me6_azm_data,
  me7_azm_data
)

# Function to plot the curves based on the data inserted

plot_timeline <- function(data, plotname, colorindex, segment, isExperimental) {
  # Function to extract the start date for each participant
  get_start_date <- function(df) {
    df %>%
      arrange(date) %>%             # Ensure data is sorted by date
      slice(1) %>%                  # Take the first row (earliest date)
      select(participant, date)    # Select relevant columns
  }
  
  # Apply function to each participant and bind results
  start_dates <- bind_rows(lapply(data, get_start_date))
  
  # Convert to proper date format
  start_dates$date <- ymd(start_dates$date)
  start_dates$end_date <- as.Date("2025-06-17")  # Study end date
  print(start_dates)
  
  if (segment && !isExperimental) {
    # Create a timeline plot function
    ggplot(start_dates, aes(x = date, y = reorder(participant, date))) +
    geom_segment(aes(x = date, xend = end_date, yend = participant),
                 color = viridis::viridis(2, option = "cividis")[colorindex], linewidth = 3) +
    geom_vline(xintercept = as.Date("2025-06-17"), color = viridis::viridis(12, option = "cividis")[9]) +
      
    labs(
      title = plotname,
      x = "Date",
      y = "Participant"
    ) +
    theme(legend.position = "none") +
    theme_few()
  }
  else if (segment && isExperimental) {
    # Create a timeline plot function
    ggplot(start_dates, aes(x = date, y = reorder(participant, date))) +
      geom_segment(aes(x = date, xend = end_date, yend = participant),
                   color = viridis::viridis(2, option = "cividis")[colorindex], linewidth = 3) +
      geom_vline(xintercept = as.Date("2025-05-12"), color = viridis::viridis(12, option = "cividis")[2]) +
      geom_vline(xintercept = as.Date("2025-05-25"), color = viridis::viridis(12, option = "cividis")[5]) +
      geom_vline(xintercept = as.Date("2025-06-17"), color = viridis::viridis(12, option = "cividis")[9]) +
      
    labs(
      title = plotname,
      x = "Date",
      y = "Participant"
    ) +
    theme(legend.position = "none") +
    theme_few()
  }
  else {
    # Create a timeline plot function
    ggplot(start_dates, aes(x = date, y = reorder(participant, date))) +
      geom_point(size = 3, color = viridis::viridis(2, option = "cividis")[colorindex]) +
      labs(
        title = plotname,
        x = "Start Date",
        y = "Participant"
      ) +
      xlim(as.Date("2025-05-01"), as.Date("2025-05-15")) +
      theme_few()
  }
    

}



# Plot timelines
# plot_timeline(experiment_participant_list, "Experimental group start dates", 1, FALSE, TRUE)
# plot_timeline(control_participant_list, "Control group start dates", 2, FALSE, FALSE)
# plot_timeline(experiment_participant_list, "Experimental group dates", 1, TRUE, TRUE)
# plot_timeline(control_participant_list, "Control group dates", 2, TRUE, FALSE)


# Combined chart ----------------------------------------------------------
plot_combined_timeline <- function(data1, data2, plotname) {
  # Function to extract start and end date
  get_dates <- function(df, group_label) {
    df %>%
      arrange(date) %>%
      slice(1) %>%
      mutate(end_date = as.Date("2025-06-17"),
             group = group_label) %>%
      select(participant, date, end_date, group)
  }
  
  # Extract data for both groups
  d1 <- bind_rows(lapply(data1, get_dates, group_label = "Experimental"))
  d2 <- bind_rows(lapply(data2, get_dates, group_label = "Control"))
  all_dates <- bind_rows(d1, d2)
  all_dates$date <- ymd(all_dates$date)
  
  # Reference lines for key dates
  ref_lines <- data.frame(
    date = as.Date(c("2025-05-11", "2025-05-25", "2025-06-17")),
    label = c("Baseline End", "Badges Added", "Study End")
  )
  
  # Plot
  ggplot(all_dates, aes(x = date, xend = end_date, y = reorder(participant, date), yend = participant, color = group)) +
    geom_segment(linewidth = 3) +
    geom_vline(data = ref_lines, aes(xintercept = date, color = label), linetype = 1) +
    scale_color_manual(
      name = "Legend",
      values = c(
        "Experimental" = viridis::viridis(2, option = "cividis")[1], 
        "Control" = viridis::viridis(2, option = "cividis")[2], 
        "Baseline End" = viridis::viridis(12, option = "cividis")[2], 
        "Badges Added" = viridis::viridis(12, option = "cividis")[5], 
        "Study End" = viridis::viridis(12, option = "cividis")[9]
      )) +
    
    labs(
      title = plotname,
      x = "Date",
      y = "Participant",
      color = "Group"
    ) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    theme_few()
}


# plot_combined_timeline(experiment_participant_list, control_participant_list, "Combined Participant Timelines")


# Make a nice graphic

plot1 <- plot_timeline(experiment_participant_list, "Experimental group dates", 1, TRUE, TRUE)
plot2 <- plot_timeline(control_participant_list, "Control group dates", 2, TRUE, FALSE)
plot3 <- plot_combined_timeline(experiment_participant_list, control_participant_list, "Combined Participant Timelines")
plot4 <- plot_combined_timeline(experiment_participant_list, control_participant_list, "Participant Recruitment Timeline")


# Create a layout: 2 columns, 2 rows
layout <- rbind(c(1, 2),
                c(3, 3))  # plot 3 spans both columns

# Arrange with custom layout
timeline_plots <- grid.arrange(
  plot1, plot2, plot3,
  layout_matrix = layout
)

ggsave("timeline_plots.png", plot = timeline_plots, width = 14, height = 10, dpi = 300, units = "in")
ggsave("timeline_combined.png", plot = plot4, width = 7, height = 4, dpi = 300, units = "in")
