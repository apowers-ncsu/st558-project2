# This app will enable exploration of data representing mobile device attributes and behaviors.
# For further references, see associated [Quarto file](static-app-prototype.qmd).

#includes
library(shiny)
library(bslib)
library(DT)

#vars and core data read
source("myhelpers.R")

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
            inputId = "inModel",
            label = "Model",
            choices = c("~ All ~",attributes(dt$Model)$levels)
          ),
          
          ### OS
          radioButtons(
            inputId = "inOS",
            label = "OS",
            choices = c("~ All ~",attributes(dt$OS)$levels)
          ),
          
          ### Gender
          radioButtons(
            inputId = "inGender",
            label = "Gender",
            choices = c("~ All ~",attributes(dt$Gender)$levels)
          ),
          
          ### age groups
          checkboxGroupInput(
            inputId = "inAgeGroup",
            label = "Age Group",
            choices = attributes(dt$AgeGroup)$levels,
            selected = attributes(dt$AgeGroup)$levels
          ),          
          
          ### classes
          checkboxGroupInput(
            inputId = "inUsageClass",
            label = "Usage Class",
            choices = attributes(dt$UsageClass)$levels,
            selected = attributes(dt$UsageClass)$levels
          ),          
          
          #numerical var selections
          h3("Numerical Variables"),
          
          #select any 2 from list
          selectizeInput(
            inputId = "inNumVars",
            label = "Select 0-2 variables to subset",
            choices = numVars,
            multiple = TRUE,
            options = list(maxItems = 2,
                           plugins = c("clear_button")
                           )
            ),
          
          #slider1
          uiOutput("outSlider1"),

          #slider2
          uiOutput("outSlider2"),
          
          #"go" button
          actionButton(
            inputId = "inProcessButton",
            label = "Process Selections"
          )
                    
        ),

        # main panel with tabs
        mainPanel(
          navset_pill(
            
            ### About tab, default start
            nav_panel(
              title = "About",
              p(),
              
              #header image
              tags$div(
                style="text-align:center",
                img(src="https://cdn.pixabay.com/photo/2016/11/23/13/40/cellphone-1852898_1280.jpg",width="100px")
              ),
                            
              ##### Purpose of app
              h4("Purpose"),
              p("This Shiny app should facilitate smooth and dynamic analysis of the dataset described below. Through reactive methods, the user can easily subset and filter data and re-query to programmatically update plots and summaries."),
              
              ##### Data, source, link
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
              
              ##### tell purpose of sidebar and each tab
              h4("App Components"),
              h5("Sidebar"),
              p("Define changes to the dataset query, including categorical and numerical subsetting. When data subsets are ready, click Process Selections to run and update the dataset and displays."),
              h5("About"),
              p("Information about the app design and usage."),
              h5("Data Download"),
              p("Display dataset returned by the latest execution of the Process Selections button, incorporating applicable subsets. User may also download the data as a .csv file."),
              h5("Data Exploration"),
              p("Area for exploratory data analysis using numerical and graphical summaries. User may adjust certain plot elements and variables displayed."),
              
              #kaggle img/link
              tags$a(
                href="https://www.kaggle.com/",
                img(src="https://www.kaggle.com/static/images/site-logo.svg",height="30px")
              )
            ),
            
            ### Data Download tab
            nav_panel(
              title = "Data Download",
              p(),
              
              ##### save data via downloadButton
              actionButton(
                inputId = "inDownloadButton",
                label = "Download Table as .csv"
              ),
              
              ##### display with dataTableOutput / renderDataTable
              DTOutput(outputId="outDTOutput")
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
              p(),
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
### somewhere must use dynamic text#############################################
server <- function(input, output, session) {
  


  #define data update reactive "function" to watch for action button in sidebar
  dt_update <- reactive({
    x = input$inProcessButton
  })
  
  output$outDTOutput <- renderDT({
    dt_update()
    print(dt)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
