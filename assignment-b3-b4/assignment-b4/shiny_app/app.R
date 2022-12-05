library(shiny)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)
library(tidyverse)
grizzly<- read.csv("grizzlybear_2019_conservationranking_results.csv") %>%
  arrange(Region)

ui <- fluidPage(

  #title
  h1("2018 Grizzly Bear Conservation in British Columbia"),

  #this call is required to hide the plot when nothing is selected, as per the default when the page opens
  useShinyjs(),

  #background
  #this adds a neat visual to the page and quickly lets the user know that we are talking about grizzly bears
  setBackgroundImage(src = "https://techcouver.com/wp-content/uploads/2020/11/S1I1596_web_khutzemale7_-Eric-Sambol-1.jpg"),

  #sidebar
  sidebarLayout(
    sidebarPanel(
      img(src="https://www.biologicaldiversity.org/assets/img/species/mammals/GrizzlyBearRobinSilver.jpg",
          width = "100%"),

      #prints instructions for the user
      h5("Welcome! This page offers a quick insight into grizzly conservation efforts in BC. Choose a parameter and a region to get started."),
      a(href = "https://www.catalogue.data.gov.bc.ca/dataset/grizzly-bear-conservation-ranking-results",
        "To access the dataset, click here",
        target = "_blank"),

      #provides parameter (y axis) input for the user
      varSelectInput("paramInput", "Parameter",
                   select(grizzly, c("PopnEst2018", "Female_Popn_2018", "Overal_Threat")),
                   selected = NULL),

      #provides input that filters the dataset by region
      selectInput("regionInput", "Region",
                  c(grizzly$Region),
                  selected = NULL)),

    #main panel
    mainPanel(
      plotOutput("consplot", width = "100%")
    ),
  ))

server <- function(input, output) {

 #this code hides the plot if nothing in the regionInput box is selected
   observe({
    if (input$regionInput == "") {
      hide("consplot")
    } else {
      show("consplot")
   }
  })

  #filters the grizzly dataset by Region for use with the selectInput feature
  by_region <- reactive({
    grizzly %>%
      filter(Region == input$regionInput)
  })

  #population estimate plot
  #this gives users a visually-easy glimpse of grizzly population size, female pop size, and threat level
  output$consplot<- renderPlot({
    if(input$paramInput == "PopnEst2018"){
    popn_plot<- ggplot(by_region(), aes(GBPU, PopnEst2018)) +
      geom_bar(stat = "identity") +
      labs(y="Population Estimate", x="Within-Region Population Unit") +
      theme(panel.background = element_blank(),
            axis.line = element_line(colour = "black", size = 0.5),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    popn_plot
    } else if(input$paramInput == "Female_Popn_2018") {
      fem_plot<- ggplot(by_region(), aes(GBPU, Female_Popn_2018)) +
        geom_bar(stat = "identity") +
        labs(y="Female Population Estimate", x="Within-Region Population Unit") +
        theme(panel.background = element_blank(),
              axis.line = element_line(colour = "black", size = 0.5),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
      fem_plot
    } else if(input$paramInput == "Overal_Threat") {
      threat_plot<- ggplot(by_region(), aes(GBPU, Overal_Threat)) +
        geom_bar(stat = "identity") +
        labs(y="Threat Rating", x="Within-Region Population Unit") +
        theme(panel.background = element_blank(),
              axis.line = element_line(colour = "black", size = 0.5),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
      threat_plot
    }
  })


}
shinyApp(ui = ui, server = server)
