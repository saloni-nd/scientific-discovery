library(tidyverse)
library(scales)
library(viridis)

# Data source: CDC Wonder database https://wonder.cdc.gov/natality.html
# Natality for 2007 - 2022 (expanded)
# Group by: Month, Year
# Select delivery characteristics: All years

# Import data
file_path <- "" # Replace with path to file

births_raw <- read_tsv(paste0(file_path, "Natality-2007-2022-expanded.txt"), col_names = TRUE)

# Only keep rows where Notes is blank (NA)
births_cleaned <- births_raw %>% 
  filter(is.na(Notes))

# Make month a factor - order them in reverse order so December is shown at the bottom of the chart
births_cleaned <- births_cleaned %>%
  mutate(Month = factor(Month, levels = unique(Month[order(-`Month Code`)])))

# Create discrete intervals for number of births - sequence from 260,000 to 400,000 in intervals of 20,000
births_cleaned$Births_factor <- cut(births_cleaned$Births,
                                  breaks = seq(260000, 400000, by = 20000),
                                  include.lowest = TRUE,
                                  labels = paste(seq(260, 380, by = 20), seq(280, 400, by = 20), sep = "–"))

# Ensure Births_factor is a factor
births_cleaned$Births_factor <- as.factor(births_cleaned$Births_factor)

# Create heatmap
ggplot(births_cleaned, aes(x = Year, y = Month, fill = Births_factor)) +
  geom_tile(color = "white") +
  scale_fill_viridis_d(option = "magma", guide = guide_legend(reverse = TRUE)) + # Use magma for discrete scale
  theme_minimal() +
  labs(title = "Births rise between July and September",
       subtitle = "Number of births in the United States by month and year",
       caption = "Data source: CDC Wonder database 2007-2022\nChart by Saloni Dattani",
       x = "",
       y = "",
       fill = "Number of births\n(thousands)") +
  scale_x_continuous(breaks = unique(births_cleaned$Year)) + 
  theme(axis.text.x = element_text(angle = 0, size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(), # Remove gridlines
        panel.grid.minor = element_blank(), # Remove gridlines
        plot.title = element_text(size = 20)) # Large title

