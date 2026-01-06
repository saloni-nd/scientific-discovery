library(tidyverse)
library(scales)
library(viridis)
library(ggrepel)
library(data.table)
library(extrafont)

# Import spreadsheet
file_path <- "scientific-discovery/vaccination_timeline/"

vax <- read_csv(paste0(file_path, "vaccine-discovery-dataset-types.csv"), skip = 0)

# Remove withdrawn vaccines
vax <- vax %>% 
  filter(is.na(NA_reason)) %>% 
  filter(Combination_vaccine != "TRUE") %>%
  filter(Vaccine_type != "Antitoxin serum") 

# Arrange by year
vax <- vax %>%
  arrange(Year) 

# Give vaccines an ID number - all vaccines for the same disease get the same ID
setDT(vax)[, id := .GRP, by = Name]

vax <- as.tibble(vax)

# Label first vaccine for each disease
vax$First <- +(!duplicated(vax$Name))

# Change the names of Vaccine_type if they have multiple types (i.e. they have a + sign) to `multiple`
vax <- vax %>% mutate(Vaccine_type = if_else(str_detect(Vaccine_type, "\\+"),
                                "Multiple",
                                Vaccine_type))

# Select pathogen colors
pathogen_colors <- c(Bacteria = "#363b8f", 
                  Virus = "#cee0dc", 
                  Parasite ="#f4d06f")

# Select vaccine type colors
vaccine_type_colors <- c(
  "Inactivated"    = "#4E79A7",
  "Live attenuated"= "#72B569",
  "Conjugate"      = "#F8A0CB",
  "Polysaccharide" = "#F28E2B",
  "mRNA"           = "#B07AA1",
  "Multiple"       = "#9D9D9D",
  "Protein subunit"= "#89C6D5",
  "Toxoid"         = "#EDC949",
  "Viral vector"   = "#AF7E4C"
)

# Create the main timeline plot
# Note: Switch fill to Vaccine_type or Organism, to color by vaccine type or pathogen respectively
plot <- ggplot(data = vax, aes(x=Year, y=id, label=Name, fill=Vaccine_type)) +
  # Colored points with black border
  geom_point(data = filter(vax %>% arrange(desc(Year)), First == 0),  # descending order so that left-most dots are plotted last
             color="black", 
             size=2.5, 
             pch=21, 
             stroke=0.8) +
  geom_point(data = filter(vax, First == 1), 
             color="black", 
             size=2.5, 
             pch=21, 
             stroke=0.8) +
  # Show text only for first vaccine
  geom_text(data=filter(vax, First == 1), hjust=1, nudge_x=-4, size=3) + 
  scale_fill_manual(values=vaccine_type_colors) + # to color by vaccine type
  #scale_fill_manual(values=pathogen_colors) + # to color by pathogen
  theme_classic() +
  scale_x_continuous(breaks= seq(1770,2020,by=10), 
                     labels = c(rep("",3), 1800, rep("",4), 
                                1850, rep("",4), 
                                1900, rep("",4),
                                1950, rep("",4),
                                2000, rep("",2))) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(size = 20)) +
  labs(title="Vaccine discovery",
       subtitle="The year when each vaccine was licensed for the first time.\nSubsequent vaccines for the same pathogen or disease are shown on the same row.",
       x="",
       y="",
       caption="Data source: Dattani (2023)\nAvailable at: code.scientificdiscovery.dev",
       fill="Vaccine type") + # switch between Organism and Vaccine type
  coord_cartesian(xlim=c(1770,2024))

print(plot)

# Save the plot with a name
ggsave(filename = paste0(file_path, "vaccine-discovery-bytype.svg"), 
       plot = plot, 
       dpi = 300,  
       height = 7, width = 7)


# Create a table to show the number of new vaccines per decade
vax <- vax %>%
  mutate(Decade = floor(Year / 10) * 10)

### Code to create histogram of number of vaccines per decade

# Filter only first vaccines
first_vax <- vax %>% filter(First == 1)

# Create summary table
# Switch from first_vax to vax to choose between only the first vaccine of each disease vs. all vaccines
summary_table <- first_vax %>%
  group_by(Decade) %>%
  summarise(
    Num_Vaccines = n(),
    Vaccine_Names = paste(Name, collapse = ", ")
  ) %>%
  arrange(Decade) %>%
  # complete the table so each decade is shown including if they have 0 vaccines in them
  complete(Decade = seq(min(Decade), max(Decade), by = 10), 
           fill = list(Num_Vaccines = 0))

# Print the summary table
print(summary_table)

# Generate a histogram of the number of vaccines using the summary table
plot_hist <- ggplot(summary_table, aes(x = Decade, y = Num_Vaccines)) +
  geom_col(fill = "#69b3a2", color = "black") +  # Use geom_col instead of geom_histogram
  geom_smooth(method = "loess", se = TRUE, color = "blue", size = 1.2) +  # smoothing line
  scale_x_continuous(
    breaks = seq(1790, 2030, by = 10),  # Tick for every decade
    minor_breaks = seq(1790, 2030, by = 10),  # Ensure ticks at each decade
    labels = ifelse(seq(1790, 2030, by = 10) %% 50 == 0, 
                    paste0(seq(1790, 2030, by = 10), "s"), 
                    "")  # Labels only for every 50 years
  ) +
  scale_y_continuous(expand = c(0, 0)) +  # Removes extra padding at y=0
  coord_cartesian(ylim = c(0, NA)) + # start at 0
  labs(title = "Number of first vaccines per decade",
       x = "Decade",
       y = "") +
  theme_classic(base_size = 12) +  # Switch to classic theme to keep ticks
  theme(panel.grid = element_blank(),  # Removes gridlines
        axis.ticks.length = unit(0.2, "cm"),  # Adjust tick length
        plot.title = element_text(size = 14, hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),  # Keep text size readable
        axis.text.y = element_text(size = 10))

# Display the updated histogram
print(plot_hist)

# Save the plot with a name
ggsave(filename = paste0(file_path, "vaccine-discovery-decade-hist-first.svg"), 
       plot = plot_hist, 
       dpi = 300,  
       height = 4, width = 5)

### Code to create histogram of number of vaccines per decade by type
summary_table_type <- vax %>%
  group_by(Decade, Vaccine_type) %>%
  summarise(
    Num_Vaccines = n(),
    Vaccine_Names = paste(Name, collapse = ", ")
  ) %>%
  arrange(Decade)

# Derive the order of first appearance by Year
type_order <- summary_table_type %>%
  group_by(Vaccine_type) %>%
  summarise(First_year = min(Decade * 10), .groups = "drop") %>%
  arrange(First_year) %>%
  pull(Vaccine_type)

# Apply that order to Vaccine_type factor, reversing the order so first-appearing types are at the bottom
summary_table_type <- summary_table_type %>%
  mutate(Vaccine_type = factor(Vaccine_type, levels = rev(type_order)))

# Generate a histogram of the number of vaccines using the summary table
plot_hist_type <- ggplot(summary_table_type, aes(x = Decade,
                                                 y = Num_Vaccines,
                                                 fill = Vaccine_type)) +
  geom_col(stat = "identity") +
  scale_fill_manual(values = vaccine_type_colors) +   # use your custom palette
  labs(x = "Decade",
       y = "Number of vaccines",
       fill = "Vaccine type") +
  scale_x_continuous(
    breaks = seq(1790, 2030, by = 10),  # Tick for every decade
    minor_breaks = seq(1790, 2030, by = 10),  # Ensure ticks at each decade
    labels = ifelse(seq(1790, 2030, by = 10) %% 50 == 0, 
                    paste0(seq(1790, 2030, by = 10), "s"), 
                    "")  # Labels only for every 50 years
  ) +
  scale_y_continuous(expand = c(0, 0)) +  # Removes extra padding at y=0
  labs(title = "Number of vaccines per decade by type",
       x = "Decade",
       y = "") +
  theme_classic(base_size = 12) +  # Switch to classic theme to keep ticks
  theme(panel.grid = element_blank(),  # Removes gridlines
        axis.ticks.length = unit(0.2, "cm"),  # Adjust tick length
        plot.title = element_text(size = 14, hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),  # Keep text size readable
        axis.text.y = element_text(size = 10))

print(plot_hist_type)

# Save the plot with a name
ggsave(filename = paste0(file_path, "vaccine-discovery-histogram-bytype.svg"), 
       plot = plot_hist_type, 
       dpi = 300,  
       height = 7, width = 7)
