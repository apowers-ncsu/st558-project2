# This app will enable exploration of data representing mobile device attributes and behaviors.
# For further references, see associated [Quarto file](static-app-prototype.qmd).

#includes
library(shiny)
library(bslib)

# global vars
numVars <- c("Apps",
             "ScreenTime_hr",
             "AppUsageTime_hr",
             "DataUsage_MB",
             "BatteryUsage_mAh")

# UI definition
### somewhere must use dynamic text
ui <- fluidPage(

    # Application title panel
    titlePanel("Project 2: Exploring Mobile Device Data"),

    #author
    h3("Andy Powers"),
    h4(
      tags$a(
        href="mailto:apowers@ncsu.edu",
        "apowers@ncsu.edu")
      ),

    # sidebar with widgets for subsetting
    ### 2+ categorical vars: offer select levels as well as 'all'
    ### 2+ numeric variable: dynamic method for slider 
    ### action button to subset per selections (vs auto-sub as edited)
    sidebarLayout(
        sidebarPanel(
          
          #categorical var selections
          h3("Categorical Variables"),
          
          ### model
          radioButtons(
            inputId = "model",
            label = "Model",
            choices = c("~ All ~",attributes(data$Model)$levels)
          ),
          
          ### OS
          radioButtons(
            inputId = "os",
            label = "OS",
            choices = c("~ All ~",attributes(data$OS)$levels)
          ),
          
          ### Gender
          radioButtons(
            inputId = "gender",
            label = "Gender",
            choices = c("~ All ~",attributes(data$Gender)$levels)
          ),
          
          ### age groups
          checkboxGroupInput(
            inputId = "ageGroup",
            label = "Age Group",
            choices = attributes(data$AgeGroup)$levels,
            selected = attributes(data$AgeGroup)$levels
          ),          
          
          ### classes
          checkboxGroupInput(
            inputId = "usageClass",
            label = "Usage Class",
            choices = attributes(data$UsageClass)$levels,
            selected = attributes(data$UsageClass)$levels
          ),          
          
          #numerical var selections
          h3("Numerical Variables"),
          
          #select any 2 from list
          selectizeInput(
            inputId = "numVars",
            label = "Select 0-2 variables to subset",
            choices = numVars,
            multiple = TRUE,
            options = list(maxItems = 2,
                           plugins = c("clear_button")
                           )
            ),
          
          #slider1
          uiOutput("slider1"),

          #slider2
          uiOutput("slider2"),
          
          #"go" button
          actionButton(
            inputId = "process",
            label = "Process Selections"
          )
                    
        ),

        # main panel with tabs
        mainPanel(
          navset_pill(
            
            ### About tab, default start
            ##### Purpose of app
            ##### Data, source, link
            ##### tell purpose of sidebar and each tab
            ##### relevant picture
            nav_panel(
              title = "About",
              h4("Purpose"),
              p("This Shiny app should facilitate smooth and dynamic analysis of the dataset described below. Through reactive methods, the user can easily subset and filter data and re-query to programmatically update plots and summaries."),
              
              h4("Data Source"),
              p("This app will explore mobile device usage data from",
                tags$a(
                  href="https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset",
                  "Kaggle"),
                ". Their site does not describe the sampling method or sources, but it summarizes the dataset well:"
              ),
              tags$blockquote(
                "This dataset provides a comprehensive analysis of mobile device usage patterns and user behavior classification. It contains 700 samples of user data, including metrics such as app usage time, screen-on time, battery drain, and data consumption. Each entry is categorized into one of five user behavior classes, ranging from light to extreme usage, allowing for insightful analysis and modeling."
              ),
              
              h4("Controls"),
              p("blah"),
              #image
              ),
            
            ### Data Download tab
            ##### display with dataTableOutput / renderDataTable
            ##### subset per selections/action button
            ##### save data via downloadButton
            nav_panel(
              title = "Data Download",
              p("Display the data using DT::dataTableOutput() with DT::renderDataTable()
∗ The data should be subsetted when the user selects a subset (and clicks the previously mentioned
button)
∗ Save the (possibly subsetted) data as a file (.csv is fine but whatever you’d like) by using a
downloadButton() (")
              ),
            
            ### Data Exploration tab
            ##### obtain the num/graph summaries from the prototype build contents
            ##### design as I like - subtabs, dynamic UI, etc.
            ##### subset when selected/action
            ##### user can display cat or num var summaries
            ##### user selects any cat or num vars to summarize, plus any modifying the summary
            ####### for ex: select num var for num summary, plus cat var to summarize across
            ####### for ex: plots, select which var on x or y, coloring, etc.
            ##### account for error messages, plus loading spinners for waits
            nav_panel(
              title = "Data Exploration",
              p("The tab will allow the user to obtain the numeric and graphical
summaries noted from ‘Prepare for Your App’ section
2
∗ I want you to design how this part looks - whether you use subtabs, dynamic UI elements,
etc.
∗ The data should be subsetted when the user selects a subset (and clicks the previously mentioned
button)
∗ The user should be able to choose to display the categorical data summaries or the numeric
variable summaries (you can choose to display graphs and numbers together or separately).
∗ The user should be able to select which (categorical or numeric) variables they are summarizing
and which variables are modifying the summary where appropriate.
· For instance, they should be able to select numeric variable to find the means, medians,
sds, etc. and select the categorical variable that these summaries should be found across.
For plots, they should be able to select which variable might be on the x or y axis, the
variable used for coloring, those kinds of things!
∗ You should account for any error messages that pop up while changing the widgets (as done
in the notes) and use loading spinners or other methods to display for any plot that takes a
while to render.")
              ),
            
            nav_spacer(),
            nav_menu(
              title = "Links",
              nav_item("link_shiny"),
              nav_item("link_posit")
            )
          )
           #plots etc.
        )
    )
)

# server definition
### somewhere must use dynamic text
server <- function(input, output, session) {

    #output$distPlot <- renderPlot({
        
    #})
}

# Run the application 
shinyApp(ui = ui, server = server)
