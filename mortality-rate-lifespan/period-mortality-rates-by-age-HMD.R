# Open libraries
library(tidyverse)
library(scales)
library(viridis)
library(RColorBrewer)

# !!! Download and replace this with path to folder
data_folder <- ""

# Data source:
# https://www.mortality.org/

# Choose countries, then go to Period data > Death Rates > 1x10
# set filename to Mx_1x10_(name of country).txt

countries <- c("Italy", "Spain", "USA")
period_mortality <- list()


# Import period data
for (country in countries) {
  
  # Import and rename cols
  period_mortality[[country]] <- read_table(paste0(data_folder, "Mx_1x10_", country, ".txt"), skip=2, na = ".")
  colnames(period_mortality[[country]]) <- c("Year", "Age", "Female", "Male", "Total")
  
  period_mortality[[country]] <- period_mortality[[country]] %>%
    mutate(Country = country) %>%
    mutate(Type = "Period")
  
}

# Join into single df
period_mortality <- do.call(rbind.data.frame, period_mortality)

# Gather to make it long format and get only values for total
period_mortality <- gather(period_mortality, "Sex", "Rate", Female:Total)

mortality <- period_mortality

# Reformat
mortality$Age <- as.integer(mortality$Age)
mortality$Rate <- as.numeric(mortality$Rate)
mortality$Country <- as.factor(mortality$Country)
mortality$Sex <- as.factor(mortality$Sex)
mortality$Type <- as.factor(mortality$Type)

# Select decades to show, retain only Total
# Define the decades you are interested in
decade_starts <- seq(1900, 2000, by = 20)
decade_ranges <- sapply(decade_starts, function(x) paste(x, x+9, sep="-"))

# Filter the data to include only rows with the specified decades
mortality_d <- mortality %>%
  filter(Year %in% decade_ranges) %>%
  filter(Sex == 'Total')

mortality_d$Year <- as.factor(mortality_d$Year)

# Remove mortality rates of 0 as they cant be logged and are invalid
mortality_d <- mortality_d %>%
  filter(Rate > 0)

# Plot period mortality rates
ggplot(data=mortality_d, aes(color=Year, x=Age, y=Rate)) +
  # Choose line or smoothed line or points
  geom_line(aes(color=Year),size=1,alpha=1) +
  geom_point(data=filter(mortality_d, Age==0), aes(color=Year,x=Age,y=Rate), size=1, show.legend=FALSE)+
  # Limit to 95 because ages above 100 are noisy and go above 100%
  coord_cartesian(xlim=c(0,95)) +
  facet_grid(cols=vars(Country)) +
  theme_classic() +
  theme(strip.background = element_blank()) +
  scale_x_continuous(breaks = seq(0, 100, by=10)) +
  scale_y_continuous(labels = scales::percent, trans='log2', breaks = c(0.0001, 0.001, 0.01, 0.1, 1)) +
  labs(title = "Annual death rate by age", 
       subtitle = "Period death rate, per 100,000 people", 
       y = "",
       x = "Age",
       color = "Decade",
       caption = "Source: Max Planck Institute for Demographic Research (Germany), University of California, Berkeley (USA), and French Institute for Demographic Studies (France).\n(data downloaded on [12 Sep 2023])\nChart by Saloni Dattani\nAvailable at: code.scientificdiscovery.dev") 

ggsave(paste0(data_folder, "annual-mortality-time-countries.svg"), width = 12, height = 6)

