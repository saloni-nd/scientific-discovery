# Open libraries
library(tidyverse)
library(scales)

# Data source:
# CDC Wonder https://wonder.cdc.gov/
# Underlying cause of death -> group by: single-year age group, ICD chapter
# Tick 'Percent of Total Deaths'
# Download and save to data_folder

# !!! Download and replace this with path to folder
data_folder <- ""

# Import
raw_df <- read_tsv(paste0(data_folder, "underlying-cause-of-death-single-year-2018-2021-both-sexes.txt"))
colnames(raw_df) <- c("Notes", "Age_long", "Age",  "ICD_long", "ICD", "Deaths_n", "Population", "Death_crude_rate", "Pct_deaths")

# Recode vars
coded_df <- raw_df

coded_df$Age <- as.numeric(coded_df$Age)
#coded_df$Gender <- as.factor(coded_df$Gender)
coded_df$ICD_long <- as.factor(coded_df$ICD_long)
coded_df$Death_crude_rate <- as.numeric(coded_df$Death_crude_rate)
coded_df$Population <- as.numeric(coded_df$Population)

# Rename ICD_long because theyre too long
rename_vector <- c(
  "Certain conditions originating in the perinatal period" = "Perinatal conditions",
  "Certain infectious and parasitic diseases" = "Infectious diseases",
  "Codes for special purposes" = "Special ICD codes",
  "Congenital malformations, deformations and chromosomal abnormalities" = "Birth disorders",
  "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism" = "Blood disorders",
  "Diseases of the circulatory system" = "Cardiovascular diseases",
  "Diseases of the digestive system" = "Digestive diseases",
  "Diseases of the genitourinary system" = "Genitourinary diseases",
  "Diseases of the musculoskeletal system and connective tissue" = "Musculoskeletal diseases",
  "Diseases of the nervous system" = "Nervous system diseases",
  "Diseases of the respiratory system" = "Respiratory diseases",
  "Diseases of the ear and mastoid process" = "Ear diseases",
  "Diseases of the skin and subcutaneous tissue" = "Skin diseases",
  "Endocrine, nutritional and metabolic diseases" = "Endocrine diseases",
  "External causes of morbidity and mortality" = "External causes",
  "Mental and behavioural disorders" = "Mental and behavioural",
  "Neoplasms" = "Cancers",
  "Pregnancy, childbirth and the puerperium" = "Pregnancy conditions",
  "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified" = "Ill-defined conditions"
)

# Remove NAs
coded_df <- coded_df %>% 
  filter(#!is.na(Gender), 
    !is.na(Age_long), 
    !is.na(ICD), 
    !is.na(Deaths_n), 
    !is.na(Population))

# Calculate the % of deaths in each age and gender group that are in each ICD code
coded_df <- coded_df %>%
  group_by(Age) %>% #, Gender) %>%
  mutate(Total_Deaths_Group = sum(Deaths_n)) %>%
  ungroup() %>%
  mutate(Percentage_Deaths_ICD = (Deaths_n / Total_Deaths_Group) * 100)

# Apply the renaming
coded_df <- coded_df %>%
  mutate(ICD_long = recode(ICD_long, !!!rename_vector)) 

# Order the ICD categories in alphabetical order
coded_df$ICD_long <- factor(coded_df$ICD_long, levels = sort(levels(coded_df$ICD_long)))

# Remove ear diseases, which is negligible or zero
coded_df <- coded_df %>% filter(ICD_long != "Ear diseases")

# Define a manual palette with 20 distinct colors
my_colors <- c("#dbdb8d", "#ff9896", "#1f77b4", "#c49c94", "#7f7f7f",
               "#c7c7c7", "#d62728", "#e377c2", "#f7b6d2", "#c5b0d5",
               "#98df8a", "#9edae5", "#ffbb78", "#9467bd", "#aec7e8",
               "#17becf", "#bcbd22", "#8c564b", "#2ca02c", "#ff7f0e")

# 1. Create chart showing share of deaths from each cause
ggplot(coded_df, aes(x = Age, y = Percentage_Deaths_ICD, fill = ICD_long)) +
  #geom_bar(stat = "identity", position = "fill", alpha = 0.7) +
  geom_area(position = "fill", alpha = 0.7) + 
  #facet_wrap(~ Gender_long, scales = "free_y", nrow = 2) + 
  scale_fill_manual(values = my_colors) + 
  scale_x_continuous(breaks = seq(0, 100, by = 20)) + # X-axis breaks at multiples of 20
  scale_y_continuous(breaks = seq(0, 1, by=0.2)) + # Y-axis breaks for geom_area version (in decimal share)
  #scale_y_continuous(breaks = seq(0, 1, by=0.2), labels = scales::label_percent()) + # Y-axis breaks for geom_bar version (in percentages)
  labs(
    title = "How do causes of death vary with age?",
    subtitle = "The share of deaths from each ICD cause of death category, between 2018-2021 in the United States",
    x = "Age",
    y = "",
    fill = "Cause of death category",
    caption = "Data source: CDC Wonder database, using data on the underlying cause of death from 2018–2021\nChart by Saloni Dattani"
  ) +
  theme_minimal() + 
  guides(fill = guide_legend(title.position = "top")) +
  theme(
    strip.text.x = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "right", # Place legend at the right
    legend.box = "vertical", # Arrange legend items horizontally
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.y = element_blank(), # Remove y-axis major grid lines
    panel.grid.minor.y = element_blank() # Remove y-axis minor grid lines 
    #axis.text.y = element_text(margin = margin(r = -20)) # Move y-axis text closer to the axis
  )  

ggsave(paste0(data_folder, "cod_lifespan_share.svg"), width=8, height=6)
ggsave(paste0(data_folder, "cod_lifespan_share.png"), width=8, height=6)

# 2. Create chart showing number of deaths from each cause
ggplot(coded_df, aes(x = Age, y = Deaths_n, fill = ICD_long)) +
  geom_bar(stat = "identity", alpha = 0.7) + # Number of deaths
  scale_fill_manual(values = my_colors) + 
  scale_x_continuous(breaks = seq(0, 100, by = 20)) + # X-axis breaks at multiples of 20
  scale_y_continuous(labels = comma) + #y-axis labels with commas for thousand separator
  labs(
    title = "How do causes of death vary with age?",
    subtitle = "The number of deaths from each ICD cause of death category, between 2018-2021 in the United States",
    x = "Age",
    y = "",
    fill = "ICD cause of death category",
    caption = "Data source: CDC Wonder database, using data on the underlying cause of death from 2018–2021\nChart by Saloni Dattani"
  ) +
  theme_minimal() + 
  guides(fill = guide_legend(title.position = "top")) +
  theme(
    strip.text.x = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "right", # Place legend at the right
    legend.box = "vertical", # Arrange legend items horizontally
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.y = element_blank(), # Remove y-axis major grid lines
    panel.grid.minor.y = element_blank(), # Remove y-axis minor grid lines 
    axis.text.y = element_text(margin = margin(r = -20)) # Move y-axis text closer to the axis
  )  




# 3. Create line charts showing death rate from each cause
ggplot(coded_df, aes(x = Age, y = Death_crude_rate, color = ICD_long)) +
  geom_line() + # Death rate
  facet_wrap(~ ICD_long, scales = "free_y") + 
  scale_color_manual(values = my_colors) + 
  scale_x_continuous(breaks = seq(0, 100, by = 20)) + # X-axis breaks at multiples of 20
  # scale_y_log10() + # Remove hash to apply log scale to y-axis
  labs(
    title = "How do causes of death vary with age?",
    subtitle = "The crude death rate per 100,000 from each ICD cause of death category, between 2018-2021 in the United States",
    x = "Age",
    y = "",
    color = "ICD cause of death category",
    caption = "Data source: CDC Wonder database, using data on the underlying cause of death from 2018–2021\nChart by Saloni Dattani"
  ) +
  theme_minimal() + 
  guides(fill = guide_legend(title.position = "top")) +
  theme(
    strip.text.x = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "none", # Remove legend
    plot.title = element_text(face = "bold", size = 16))

