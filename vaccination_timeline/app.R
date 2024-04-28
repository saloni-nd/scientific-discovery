#install.packages(c("rsconnect", "shiny", "plotly", "shinythemes"))

library(shiny)
library(plotly)
library(tidyverse)
library(scales)
library(viridis)
library(ggrepel)
library(data.table)
library(shinythemes)


# Define UI for app
ui <- fluidPage(
  theme = shinytheme("flatly"), # Optional: Adds a nice theme
  titlePanel("Vaccine discovery: an interactive chart"),
  
  # Help text with hyperlink
  fluidRow(
    column(12, 
           wellPanel(
             HTML("The year when each vaccine was licensed for the first time. A selection of subsequent vaccines are shown.<br><br>
                  Hover over points for more details.<br><br>
                  See the <a href='https://code.scientificdiscovery.dev' target='_blank'>data and code</a>. If you'd like to add to this dataset, or point out an error, get in touch by email (saloni@ourworldindata.org).")
           )
    )
  ),
  
  fluidRow(
    #column(12,
  # Create a box to control the height and with of the viewport
  # Here the width is 50% of the page and 33% of the height.
  div(style="width:50vw;height:33vh;padding-top:33%;position:absolute;",
      # Create another box and fill it.
      div(style="position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;",
  # Plot 
  plotlyOutput("plot", height = "100%", width = "100%")))
    #)
  )
)

# Define server logic
server <- function(input, output) {
  
  # Import data (for demonstration, assume data is directly loaded)
   vax <- read_csv("vaccine-discovery-dataset.csv")
  # Your data loading logic here
   
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
   
   # Create breaks and labels to match original plot
   ticks <- tibble(year = seq(1770, 2020, by=10))
   tick_labs <- ticks |> 
     mutate(year = case_when(year %in% c(1800,1850,1900,1950,2000) ~ 
                               as.character(year),
                                   TRUE ~ ""))
   
  output$plot <- renderPlotly({
    # Assume 'vax' is your data frame ready from the script above
    
    p <- ggplot(data=vax, aes(x=Year, y=id, label=Name)) +
      geom_point(aes(fill=Organism, 
                     text=paste(Name, " vaccine", "<br>Year: ", Year, "<br>Target: ", Organism, "<br>Inventor: ", Inventor)), 
                 color="black", size=2.5, pch=21, stroke=0.3) +
      geom_text_repel(data=filter(vax, First == 1),
                      box.padding = 0.5,
                      point.padding = 0.5,
                      direction = "x",
                      hjust = 1,
                      nudge_x = -0.5,
                      size = 3) +
      scale_fill_manual(values=group.colors) +
      theme_classic() +
      scale_x_continuous(breaks= ticks$year, labels = tick_labs$year) + # Change to match original plot
      theme(axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            plot.title = element_text(size = 20)) +
      labs(title="",
           subtitle="",
           x="", y="", caption="Source: Dattani (2023)")
    
    # Convert ggplot object to plotly for interactivity
    ggplotly(p, tooltip="text") #%>% 
      #layout(height="100%")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
