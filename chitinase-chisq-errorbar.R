rm(list = ls())
setwd("C:/Users/Emmanuel Tibok/Desktop/Tibok/eeChange/Emma Ransome/Stats_Thesis")

library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)

### Location Comparison

chitinasel <- read.csv("Chitinase.csv")
thresholdl <- 0  # Set an appropriate threshold value based on your data
chitinasel$activity <- ifelse(chitinasel$Final > thresholdl, "Active", "Inactive")

activityl_table <- table(chitinasel$activity, chitinasel$Location)
print(activityl_table)

# Perform Fisher's Exact Test
fisher_result <- fisher.test(activityl_table)
print(fisher_result)

# Calculate proportions and standard errors
activityl_df <- as.data.frame(activityl_table)
colnames(activityl_df) <- c("Activity", "Location", "Count")

# Calculate proportions and standard errors
activityl_df <- activityl_df %>%
  group_by(Location) %>%
  mutate(Proportion = Count / sum(Count),
         SE = sqrt((Proportion * (1 - Proportion)) / sum(Count)))

### Substrate Comparison w/o JB

chitinases <- read.csv("chitinase-no-jb.csv")
thresholds <- 0  
chitinases$activity <- ifelse(chitinases$Final > thresholds, "Active",
                              "Inactive")

activitys_table <- table(chitinases$activity, chitinases$Substrate)
print(activitys_table)

# Perform Fisher's Exact Test
fisher_result <- fisher.test(activitys_table)
print(fisher_result)

# Calculate proportions and standard errors
activitys_df <- as.data.frame(activitys_table)
colnames(activitys_df) <- c("Activity", "Substrate", "Count")

# Calculate proportions and standard errors
activitys_df <- activitys_df %>%
  group_by(Substrate) %>%
  mutate(Proportion = Count / sum(Count),
         SE = sqrt((Proportion * (1 - Proportion)) / sum(Count)))

### Visualisation

# Create the side-by-side bar graph for Location with proportions and error bars
clocation <- ggplot(activityl_df, aes(x = Location, y = Proportion,
                                      fill = Activity)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8),
           width = 0.7) +
  geom_errorbar(aes(ymin = Proportion - SE, ymax = Proportion + SE), 
                width = 0.2, position = position_dodge(width = 0.8)) +
  labs(title = "Proportion of Bacteria Producing BNA by Location",
       x = "Location",
       y = "Response to Assay",
       fill = "Activity") +
  scale_y_continuous(limits = c(0, 1)) +  # Adjust y-axis to proportion scale (0-1)
  scale_fill_manual(values = c("Inactive" = "#ffccff", "Active" = "#ff66ff")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
        plot.title = element_text(size = 9, hjust = 0.5),
        legend.position = "right")

# Create the side-by-side bar graph for Substrate with proportions and error bars
csubstrate <- ggplot(activitys_df, aes(x = Substrate, y = Proportion,
                                       fill = Activity)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8),
           width = 0.7) +
  geom_errorbar(aes(ymin = Proportion - SE, ymax = Proportion + SE), 
                width = 0.2, position = position_dodge(width = 0.8)) +
  labs(title = "Proportion of Bacteria Producing BNA by Substrate",
       x = "Substrate",
       y = "Response to Assay",
       fill = "Activity") +
  scale_y_continuous(limits = c(0, 1)) +  # Adjust y-axis to proportion scale (0-1)
  scale_fill_manual(values = c("Inactive" = "#ffccff", "Active" = "#ff66ff")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 9, angle = 35, hjust = 1),
        plot.title = element_text(size = 9, hjust = 0.5),
        legend.position = "none")

#### Combine Plots

final_plot <- plot_grid(
  plot_grid(csubstrate, clocation, labels = c("A", "B"),
            ncol = 2, rel_widths = c(0.9, 1.22)),
  ncol = 1,
  rel_heights = c(1, 0.1)  # Adjust the heights of the plots and legend
)

print(final_plot)

### Getting count 
isolate_no <- chitinasel %>%
  group_by(Location, activity) %>%
  summarise(count = n())
print(isolate_no)
