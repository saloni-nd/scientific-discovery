library(tidyverse)
library(scales)
library(viridis)
library(ggrepel)
library(data.table)

# Import xlsx spreadsheet
file_path <- ""

# Write path to save files
save_folder <- ""

vax <- read_csv(paste0(file_path, "dataset.csv"), skip = 0)

# Remove withdrawn vaccines
vax <- vax %>% filter(is.na(NA_reason))

# Arrange by year
vax <- vax %>%
          arrange(Year) 

# Give vaccines an ID number - all vaccines for the same disease get the same ID
setDT(vax)[, id := .GRP, by = Name]

vax <- as.tibble(vax)

# Label first vaccine for each disease
vax$First <- +(!duplicated(vax$Name))


# Select colors
group.colors <- c(Bacteria = "#38AABA", 
                  Virus = "#BC8E5A", 
                  Parasite ="#970046")


ggplot(data=vax, aes(x=Year,y=id, label=Name)) +
  # Colored points with black border
  geom_point(aes(fill=Organism), color="black", size=2.5, pch=21, stroke=0.8) +
  # Show text only for first vaccine
  geom_text(hjust=1, nudge_x=-4, data=filter(vax,First==1), size=3) + 
  scale_fill_manual(values=group.colors) +
 # geom_text_repel(force=0.5, nudge_x=0.2) +
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
  labs(title="Progress in vaccine technology",
       subtitle="The year when each vaccine was licensed for the first time.\nSubsequent vaccines for the same pathogen or disease are shown on the same row.",
       x="",
       y="",
       caption="Source: Dattani (2023)",
       color="Target organism") +
  coord_cartesian(xlim=c(1770,2023))
  
ggsave(paste0(file_path, "Vaccine_timeline.svg"),height=8,width=10)
  
  
