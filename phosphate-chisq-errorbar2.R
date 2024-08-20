rm(list = ls())
setwd("C:/Users/Emmanuel Tibok/Desktop/Tibok/eeChange/Emma Ransome/Stats_Thesis")

library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)

phosphatel <- read.csv("Phosphate-Full.csv")
phosphates <- read.csv("phosphate-no-jb.csv")

# Calculate the proportions and standard error for locations
location_summary <- phosphatel %>%
  group_by(Location, Growth) %>%
  summarise(Count = n()) %>%
  group_by(Location) %>%
  mutate(Proportion = Count / sum(Count),
         SE = sqrt((Proportion * (1 - Proportion)) / sum(Count)))

# Plot with error bars for location
plocation <- ggplot(location_summary, aes(x = Location, y = Proportion, fill = Growth)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Proportion - SE, ymax = Proportion + SE), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  labs(title = "Proportion of PSB by Location",
       y = "Response to Assay",
       x = "Location",
       fill = "Transparent Zone") +
  scale_y_continuous(limits = c(0, 1), 
                     breaks = seq(0, 1, by = 0.25)) +  # Ensure 1.00 is included
  scale_fill_manual(values = c("N" = "#ffccff", "Y" = "#ff66ff")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 40, hjust = 1),
        plot.title = element_text(size = 9.3, hjust = 0.5),
        legend.position = "right")

# Substrate data processing for proportions and SE
substrate_summary <- phosphates %>%
  group_by(Substrate, Growth..Final.) %>%
  summarise(Count = n()) %>%
  group_by(Substrate) %>%
  mutate(Proportion = Count / sum(Count),
         SE = sqrt((Proportion * (1 - Proportion)) / sum(Count)))

# Plot with error bars for substrate
psubstrate <- ggplot(substrate_summary, aes(x = Substrate, y = Proportion, fill = Growth..Final.)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = Proportion - SE, ymax = Proportion + SE), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  labs(title = "Proportion of PSB by Substrate",
       y = "Response to Assay",
       x = "Substrate") +
  scale_fill_manual(values = c("N" = "#ffccff", "Y" = "#ff66ff")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 9, angle = 35, hjust = 1),
        plot.title = element_text(size = 10, hjust = 0.5),
        legend.position = "none")

# Combine the plots and place the legend between them
final_plot <- plot_grid(
  plot_grid(psubstrate, plocation, labels = c("A", "B"),
            ncol = 2, rel_widths = c(0.9, 1.31)),
  ncol = 1,
  rel_heights = c(1, 0.1)  # Adjust the heights of the plots and legend
)

# Display the final combined plot
print(final_plot)

# Calculating exact proportions for reference
# Create the contingency table for substrates
substrate_table <- table(phosphates$Growth..Final., phosphates$Substrate)

# Calculate the proportion of bacteria that can solubilise phosphate for each substrate
substrate_proportions <- prop.table(substrate_table, margin = 2) * 100

# Print the proportion of growth
print(round(substrate_proportions, 2))  # Rounded to 2 decimal places

# Create the contingency table for locations
location_table <- table(phosphatel$Growth, phosphatel$Location)

# Calculate the proportion of bacteria that can solubilise phosphate for each location
location_proportions <- prop.table(location_table, margin = 2) * 100

# Print the proportion of growth
print(round(location_proportions, 2))  # Rounded to 2 decimal places
