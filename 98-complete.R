rm(list = ls())
setwd("C:/Users/Emmanuel Tibok/Desktop/Tibok/eeChange/Emma Ransome/Stats_Thesis")

library(dplyr)
library(ggplot2)
library(nortest)
library(gridExtra)

# Read the CSV file
isolatel <- read.csv("Isolate98.csv")

# Check the structure of the data
str(isolatel)

# View the first few rows of the data
head(isolatel)

# Create a new data frame with the number of isolates for each location and substrate
isolate_nol <- isolatel %>%
  group_by(Location) %>%
  summarise(count = n())

head(isolate_nol)

# Stat. for Count vs. Location. Kruskal chosen as data size is small.
# One-way ANOVA can't produce p-value as the model fits too perfectly (no Df)
kruskal_testl <- kruskal.test(count ~ Location, data = isolate_nol)
print(kruskal_testl)

# Plot as bar since boxplot doesn't visualise findings clearly.
l98 <- ggplot(isolate_nol, aes(x = Location, y = count, fill = Location)) +
  geom_bar(stat = "identity") +
  labs(title = "Bacteria Counts by Location",
       x = "Location",
       y = "Bacteria Count") +
  scale_fill_manual(values = c("Cawsands" = "#cc99ff", 
                               "Drake's Island" = "#99ccff",
                               "Jennycliff Bay" = "#ff9999")) +
  ylim(0, 60) +  # Set y-axis limits to the same scale
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        theme(legend.title = element_text(size = 5)),
        legend.text = element_text(size = 7))

###

# Read the CSV file
isolates <- read.csv("98-substrate.csv")

# Check the structure of the data
str(isolates)

# View the first few rows of the data
head(isolates)

# Create a new data frame with the number of isolates for each location and substrate
isolate_nos <- isolates %>%
  group_by(Substrate) %>%
  summarise(count = n())

head(isolate_nos)

# Stat. for Count vs. Location. Kruskal chosen as data size is small.
# One-way ANOVA can't produce p-value as the model fits too perfectly (no Df)
kruskal_tests <- kruskal.test(count ~ Substrate, data = isolate_nos)
print(kruskal_tests)

# Plot as bar since boxplot doesn't visualise findings clearly.
s98 <- ggplot(isolate_nos, aes(x = Substrate, y = count, fill = Substrate)) +
  geom_bar(stat = "identity") +
  labs(title = "Bacteria Counts by Substrate",
       x = "Substrate",
       y = "Bacteria Count") +
  scale_fill_manual(values = c("Treated Sediment" = "#ffcc99", 
                               "Root" = "#ffff99",
                               "Natural Sediment" = "#99ffcc")) +
  ylim(0, 60) +  # Set y-axis limits to the same scale
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7, angle = 35, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        theme(legend.title = element_text(size = 5)),
        legend.text = element_text(size = 7))

# Arrange the plots side by side
grid.arrange(s98, l98, ncol = 2)

ggsave("98complete300dpi.png", plot = combined_plot, 
       width = 10, height = 6, dpi = 300)  # Set dpi to 300 for high quality
