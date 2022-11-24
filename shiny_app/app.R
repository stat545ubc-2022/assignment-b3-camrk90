library(shiny)
library(shinyWidgets)
library(ggplot2)
library(tidyverse)
grizzly<- read.csv("grizzlybear_2019_conservationranking_results.csv") %>%
  arrange(Region)
ui <- fluidPage(

  #title
  titlePanel("2018 Grizzly Bear Conservation Statistics"),

  #background - feature 1
    #this adds a neat visual to the page and quickly lets the user know that we are talking about grizzly bears
  setBackgroundImage(
    src = "https://www.biologicaldiversity.org/assets/img/species/mammals/GrizzlyBearRobinSilver.jpg"
  ),

  #sidebar
    #feature 2 - Region select allows users to look at population size by region
  sidebarLayout(
    sidebarPanel(
      selectInput("regionInput", "Region",
                  c(grizzly$Region), selected = grizzly[2, 1]),
      h5("Select a region above to display the estimated population size for each within-region unit")
    ),

    #main panel
    mainPanel(
      br(), br(),
      br(), br(),
      br(), br(),
      br(), br(),
      br(), br(),
      br(), br(),
      br(), br(),
      plotOutput("consplot", width = "100%")
    )
  ))

server <- function(input, output) {

  #filters the grizzle dataset by Region for use with the selectInput feature
  by_region <- reactive({
    grizzly %>%
      filter(Region == input$regionInput)
  })

  #population estimate plot - feature 3
    #this gives users a visually-easy glimpse of grizzly population sizes per unit, per region for the year of 2018
  output$consplot<- renderPlot({
    ggplot(by_region(), aes(GBPU, PopnEst2018)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(expand = expansion(mult = c(0, .1))) +
      labs(y="Population Estimate", x="Within-Region Population Unit") +
      theme(panel.background = element_blank()) +
      theme(axis.line = element_line(colour = "black", size = 0.5))
  })


}
shinyApp(ui = ui, server = server)
