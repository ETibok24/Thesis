rm(list = ls())
setwd("C:/Users/Emmanuel Tibok/Desktop/Tibok/eeChange/Emma Ransome/Stats_Thesis")

library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)

### Location Comparison

cellulasel <- read.csv("Cellulase.csv")
thresholdl <- 0  # Set an appropriate threshold value based on your data
cellulasel$activity <- ifelse(cellulasel$Final > thresholdl, "Active", "Inactive")

activityl_table <- table(cellulasel$activity, cellulasel$Location)
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

cellulases <- read.csv("cellulase-no-jb.csv")
thresholds <- 0  
cellulases$activity <- ifelse(cellulases$Final > thresholds, "Active", "Inactive")

activitys_table <- table(cellulases$activity, cellulases$Substrate)
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
clocation <- ggplot(activityl_df, aes(x = Location, y = Proportion, fill = Activity)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = Proportion - SE, ymax = Proportion + SE), 
                width = 0.2, position = position_dodge(width = 0.8)) +
  labs(title = "Proportion of Bacteria Producing BG by Location",
       x = "Location",
       y = "Response to Assay",
       fill = "Activity") +
  scale_y_continuous(limits = c(0, 1)) +  # Adjust y-axis to proportion scale (0-1)
  scale_fill_manual(values = c("Inactive" = "#ffccff", "Active" = "#ff66ff")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        plot.title = element_text(size = 12, hjust = 0.5),
        legend.position = "right")

# Create the side-by-side bar graph for Substrate with proportions and error bars
csubstrate <- ggplot(activitys_df, aes(x = Substrate, y = Proportion, fill = Activity)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = Proportion - SE, ymax = Proportion + SE), 
                width = 0.2, position = position_dodge(width = 0.8)) +
  labs(title = "Proportion of Bacteria Producing BG by Substrate",
       x = "Substrate",
       y = "Response to Assay",
       fill = "Activity") +
  scale_y_continuous(limits = c(0, 1)) +  # Adjust y-axis to proportion scale (0-1)
  scale_fill_manual(values = c("Inactive" = "#ffccff", "Active" = "#ff66ff")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 35, hjust = 1),
        plot.title = element_text(size = 12, hjust = 0.5),
        legend.position = "none")

#### Combine Plots

final_plot <- plot_grid(
  plot_grid(csubstrate, clocation, labels = c("A", "B"),
            ncol = 2, rel_widths = c(0.9, 1.22)),
  ncol = 1,
  rel_heights = c(1, 0.1)  # Adjust the heights of the plots and legend
)

print(final_plot)

ggsave("cellulaseerrorbar.png", plot = final_plot, 
       width = 10, height = 6, dpi = 300)  # Set dpi to 300 for high qualityprint

### Getting count 
isolate_no <- cellulasel %>%
  group_by(Location, activity) %>%
  summarise(count = n())
print(isolate_no)
