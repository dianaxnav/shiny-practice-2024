# load packages ----
library(shiny)
library(palmerpenguins)
library(tidyverse)
library(DT)

# user interface ----
ui <- fluidPage(
  
  # app title ----
  tags$h1("My App Title"),
  
  #add app subtitle ----
  h4(strong("Exploring Arctic Penguin Data")), 
  
  # body mass slider input ----
  sliderInput(inputId = "body_mass_input",
              label = "Select a range of body masses (g):", 
              min = 2700,
              max = 6300,
              value = c(3000, 4000)),
  
  # body mass plot output 
  plotOutput(outputId = "bodyMass_scatterplot_output"),
  
  #add input to ui for check box input for year ----
  checkboxGroupInput(inputId = "year_input", label = "Select year(s):", 
                     choices = c("2007", "2008", "2009"),
                     selected = c("2007", "2008")),
  
  #DT output ----
  DT::dataTableOutput(outputId = "penguin_DT_output")
  
)

# server ----
server <- function(input, output) {
  
  #filter body masses ----
  body_mass_df <- reactive({
  
    penguins %>% 
      filter(body_mass_g %in% c(input$body_mass_input[1]:input$body_mass_input[2]))
  })
  
  # render penguin scatter plot ----
  output$bodyMass_scatterplot_output <- renderPlot({
    
    #add ggplot code here
    ggplot(na.omit(penguins),
           aes(x = flipper_length_mm, y = bill_length_mm,
               color = species, shape = species)) +
      geom_point() +
      scale_color_manual(values = c("pink", "cornflowerblue", "indianred")) +
      scale_shape_manual(values = c("Adelie" = 19, "Gentoo" = 15, "Chinstrap" = 17)) +
      labs(x = "Flipper Length (mm)", y = "Bill Length (mm)", color = "Penguin species", shape = "Penguin Species") +
      theme_minimal() +
      theme(legend.position = c(0.85, 0.2),
                    legend.background = element_rect(color = "white"))
    
})
  
  #filter for years 
  years_df <- reactive({
    
    penguins %>% 
      filter(year %in% c(input$year_input))
    
  })
  
  #render DT table ----
  output$penguin_DT_output <- 
    DT::renderDataTable({
      
      DT::datatable(years_df())
    })
}

# combine ui and server into app ----
shinyApp(ui = ui, server = server)
