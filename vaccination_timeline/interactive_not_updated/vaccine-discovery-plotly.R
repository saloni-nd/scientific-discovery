# Load necessary libraries
library(plotly)
library(tidyverse)
library(scales)
library(viridis)
library(ggrepel)
library(data.table)

# Import and prepare data
vax <- read_csv("vaccine-discovery-dataset.csv")
vax <- vax %>% filter(is.na(NA_reason))
setDT(vax)[, id := .GRP, by = Name]
vax <- as.tibble(vax)
vax$First <- +(!duplicated(vax$Name))

# Define color mapping
group.colors <- c(Bacteria = "#38AABA", Virus = "#BC8E5A", Parasite = "#970046")

# Create the plot using ggplot
p <- ggplot(data=vax, aes(x=Year, y=id, label=Name)) +
  geom_point(aes(fill=Organism, text=paste(Name, " vaccine", "<br>Year: ", Year, "<br>Target: ", Organism, "<br>Developer: ", Inventor)), color="black", size=2.5, pch=21, stroke=0.3) +
  scale_fill_manual(values=group.colors) +
  theme_classic() +
  scale_x_continuous(breaks=seq(1800,2020,by=10), labels = c(1800, rep("",4), 1850, rep("",4), 1900, rep("",4), 1950, rep("",4), 2000, rep("",2))) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), plot.title = element_text(size = 20))

# Convert ggplot object to plotly for interactivity
plotly_plot <- ggplotly(p, tooltip="text")


# Modify layout to include title, subtitle as annotations, and source caption
plotly_plot <- plotly_plot %>%
    layout(title = list(text = paste0("Vaccine discovery",
                                        "<br>",
                                        "<sup>",
                                        "The year when each vaccine was licensed for the first time. Subsequent vaccines for the same pathogen or disease are shown on the same row.",
                                        "</sup>"))) %>%
  layout(
      list(
        text = "Source: Dattani (2023)",
        x = 0.5,
        xref = "paper",
        y = -0.15,  # below the bottom edge of the plot area
        yref = "paper",
        showarrow = "false",
        align = "center",
        xanchor = "center",
        font = list(size = 12)
      ),
    margin = list(t = 100, b = 100),  # Add top and bottom margins
    xaxis = list(title = ""),
    yaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE)
  )

# Save the Plotly plot as an HTML file
htmlwidgets::saveWidget(plotly_plot, "vaccine_discovery_chart.html", selfcontained = TRUE)

# Print the embed code for the HTML
cat("<iframe src='vaccine_discovery_chart.html' width='100%' height='400'></iframe>")
