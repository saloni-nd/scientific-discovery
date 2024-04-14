# Open libraries
library(openxlsx)
library(tidyverse)
library(scales)
library(viridis)
library(ggrepel)

print(sessionInfo())

# Data source:
# Table 3 from Gomes, M., Begum, R., Sati, P., Dikshit, R., Gupta, P. C., Kumar, R., Sheth, J., Habib, A., & Jha, P. (2017). Nationwide Mortality Studies To Quantify Causes Of Death: Relevant Lessons From India’s Million Death Study. Health Affairs, 36(11), 1887–1895. https://doi.org/10.1377/hlthaff.2017.0635
# https://www.healthaffairs.org/doi/10.1377/hlthaff.2017.0635
# Save as an xlsx file

# Import xlsx spreadsheet
file_path <- # Replace with path to file
save_folder <- # Replace with path to folder

cod <- read.xlsx(xlsxFile = file_path, sheet = 1, startRow=3)

# Rename cols
colnames(cod) <- c("Cause", "RGI", "Resample", "Home", "Hospital")

# Change to long format
cod <- cod %>%
        gather(Place, Percentage, 4:5)

# Percentage
cod$Percentage <- cod$Percentage / 100

# Plot bar chart with dodged bars
ggplot(data=cod, aes(x=Cause, 
                     y=Percentage, 
                     fill=Place)) +
  geom_col(position=position_dodge()) +
  coord_flip() +
  theme_classic() +
  labs(title = "People who die outside hospitals tend to die from different causes", 
       subtitle = "Causes of death in India (2001-2003)",
       caption = "Chart by Saloni Dattani\nSource: Nationwide Mortality Studies To Quantify Causes of Death: Relevant Lessons from India's Million Death Study (Gomes et al., 2017)",
       y="Percentage of deaths", x="") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0),
        plot.caption = element_text(hjust = 0),
        text = element_text(size = 15)) +  
  guides(fill=guide_legend(title="Place of death")) +
  scale_fill_manual(values = c("#883039", "#DA959C")) +
  scale_y_continuous(labels = scales::percent) 
  

