# user interface
ui <- navbarPage(
  title = "LTER Animal Data Explorer",
  # (Page 1) intro tabPanel ----
  tabPanel(title = "About this page",
           #intro text fluidRow 
           fluidRow(
             column(1),
             column(10, includeMarkdown("text/about.md")),
             column(1)),
           
           #end intro to text files 
           hr(),
           includeMarkdown("text/footer.md")
           
             #END fluidRow
  ), # END (Page 1) intro tabPanel
  
  # (Page 2) data viz tabPanel ----
  tabPanel(title = "Explore the Data",
           # tabsetPanel to contain tabs for data viz ----
           tabsetPanel(
             #trout tabPanel ----
             tabPanel(title = "Trout",
                      # trout sidebarlayout
                      sidebarLayout(
                        #trout sidebarPanel ----
                        sidebarPanel(
                          #channel type pickerInput
                          pickerInput(inputId = "channel_type_input",
                                      label = "Select channel type(s):",
                                      choices = unique(clean_trout$channel_type),
                                      selected = c("cascade", "pool"),
                                      options = pickerOptions(actionsBox = TRUE),
                                      multiple = TRUE),
                        
                          #section check box group buttons ---- 
                          checkboxGroupButtons(inputId = "section_input",
                                               label = "Select a sampling section(s):",
                                               choices = c("clear out forest", "old growth forest"),
                                               selected = c("clear out forest", "old growth forest"),
                                               individual = FALSE,
                                               justified = TRUE,
                                               size = "sm",
                                               checkIcon = list(yes = icon("check"),
                                                                no = icon("xmark")))
                        ), # END trout sidebarPanel
                        # trout mainPanel ----
                        mainPanel(
                          #trout scatterplot output 
                          plotOutput(outputId = 
                                       "trout_scatterplot_output") %>% 
                            withSpinner(color = "magenta", type = 1)
                        )# END trout mainPanel
                      )# END trout sidebarLayout
             ), # END trout tabPanel
             
             
             
             #penguin tabPanel ----
             tabPanel(title = "Penguins",
                      # penguin sidebarLayout ----
                      sidebarLayout(
                        # penguin sidebarPanel ----
                        sidebarPanel(
                          #island pickerInput ----
                          pickerInput(inputId = "penguin_island_input", label = "Select an island(s):",
                                      choices = c("Torgersen", "Dream", "Biscoe"),
                                      selected = c("Torgersen", "Dream", "Biscoe"),
                                      options = pickerOptions(actionsBox = TRUE),
                                      multiple = TRUE), # END island pickerInput
                          #bin number sliderInput ----
                          sliderInput(inputId = "bin_num_input", label = "Select number of bins:",
                                      value = 25, max = 100, min = 1), # END bin number sliderInput
                        ), # END penguin sidebarPanel
                        # penguin mainPanel ----
                        mainPanel(
                          plotOutput(outputId = "flipperLength_histogram_output") %>% 
                            withSpinner(color = "magenta", type = 1)
                        ) # END penguin mainPanel
                      ) # END penguin sidebarLayout
             ) # END penguin tabPanel
           ) # END tabsetPanel
  ) # END (Page 2 data viz tabPanel)
) # END navbarPage