# This app will enable exploration of data representing mobile device attributes and behaviors.
# For further references, see associated [Quarto file](static-app-prototype.qmd).

#includes
library(shiny)
library(bslib)
library(DT)
library(tidyverse)

#vars and core data read into variable 'dt'
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
          h3("Subset Configuration"),
          
          #categorical var selections
          h4("Categorical"),
          
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
          h4("Numerical"),
          
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
          
          #little dynamic text to note and "warn" about sliders
          textOutput(
            outputId = "outSliderText"
          ),
          
          #slider1 - conditional, only if inNumVars length >=1
          conditionalPanel(
            condition = "input.inNumVars.length >= 1",
            uiOutput("outSlider1")  
          ),
          
          #slider2 - conditional, only if inNumVars length ==2
          conditionalPanel(
            condition = "input.inNumVars.length == 2",
            uiOutput("outSlider2")  
          ),

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
                img(src="https://cdn.pixabay.com/photo/2016/11/23/13/40/cellphone-1852898_1280.jpg",width="200px")
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
              downloadButton(
                outputId = "outDownloadButton",
                label = "Download Table as .csv"
              ),
              p(),
                            
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

              h3("Display Configuration"),
              column(
                12,         
                ### LEFT COL: pick summary type
                column(
                  6,
                  h4("Summary"),
                  radioButtons(
                    inputId = "inSummaryType",
                    label = "Type",
                    choices = c("Categorical", "Numerical")
                  ),
                  #show numerical selection if appropriate
                  conditionalPanel(
                    condition = "input.inSummaryType == 'Numerical'",
                    uiOutput("outSummaryNumVar"),
                    uiOutput("outSummaryNumVarGroupBy")
                  ),                
                  #show categorical selection if appropriate
                  conditionalPanel(
                    condition = "input.inSummaryType == 'Categorical'",
                    uiOutput("outSummaryCatVar1"),
                    uiOutput("outSummaryCatVar2")
                  )
                  
                ),  
                
                ### RIGHT COL: choose vars to use in plots 
                column(
                  6,
                  h4("Plots"),
                  selectizeInput(
                    inputId = "inXVar",
                    label = "X-axis",
                    choices = numVars,
                    multiple = FALSE,
                    width = 200
                  ),
                  selectizeInput(
                    inputId = "inYVar",
                    label = "Y-axis",
                    choices = numVars,
                    multiple = FALSE,
                    width = 200
                  ),               
                  selectizeInput(
                    inputId = "inZVar",
                    label = "Extra (Color/Fill)",
                    choices = c("~ None ~",catVars),
                    multiple = FALSE,
                    width = 200
                  )               
                )
              ),
              
              h3("Summaries"),
              #render table per selections
              conditionalPanel(
                condition = "input.inSummaryType == 'Categorical'",
                DTOutput(outputId="outSummaryCategorical")
              ),
              conditionalPanel(
                condition = "input.inSummaryType == 'Numerical'",
                DTOutput(outputId="outSummaryNumerical")
              ),              
              
              h3("Plots")
              ###
              #So, first is a space to select your variables using checkboxes
              #with groups for cat/num to clarify
              #these selections will automatically update (so no isolate stuff)

              #PULL WHAT I DID FROM PROTOTYPE!!!
              #next, select to show EITHER? cat or num summaries?
              
              #need a structure for display, grouping, etc.
              
              #also select the vars to SUMMARIZE
              #vs those categories to GROUP ACROSS
              
              #plots
              #PULL WHAT I DID FROM PROTOTYPE!!!
              #allow selection of the x, y, color - yikes
              
              #last of all, add in error checking and spinner viewers
              
            )

          )
           #plots etc.
        )
    )
)

# server definition
### somewhere must use dynamic text#############################################
server <- function(input, output, session) {
  
  #Make reactive environment subset version of dt
  dt_subset <- reactive({
    dt |>
      
      #subset according to categorical selections
      filter(
        if (input$inModel == "~ All ~") TRUE else input$inModel == dt$Model,
        if (input$inOS == "~ All ~") TRUE else input$inOS == dt$OS,
        if (input$inGender == "~ All ~") TRUE else input$inGender == dt$Gender,
        if (length(input$inAgeGroup) == 0) TRUE else dt$AgeGroup %in% input$inAgeGroup,
        if (length(input$inUsageClass) == 0) TRUE else dt$UsageClass %in% input$inUsageClass,
        #within slider1 range, if present
        if (length(input$inNumVars)>=1)
          (dt[input$inNumVars[1]] >= input$inSlider1[1]) & (dt[input$inNumVars[1]] <= input$inSlider1[2])
        else TRUE,
        #within slider2 range, if present
        if (length(input$inNumVars)==2) 
          (dt[input$inNumVars[2]] >= input$inSlider2[1]) & (dt[input$inNumVars[2]] <= input$inSlider2[2])
        else TRUE
      )
      
  })

  #listen for button before taking action
  observeEvent(input$inProcessButton,{
    
    dt_updated <- isolate(dt_subset())
    
    #data table update
    output$outDTOutput <- renderDT({
      dt_updated
    }) 
    
    #output / render for download function
    output$outDownloadButton <- downloadHandler(
      filename = function() {
        paste('data-',
              Sys.Date(),
              '.csv',
              sep=''
        )
      },
      content = function(con) {
        write.csv(dt_updated, con)
      }
    )
    
    #render dynamic text note about any sliders
    output$outSliderText <- renderText(
      if(length(input$inNumVars)>=1)
        paste(
          "Slider range reflects unfiltered data. ",
          "Per the most recent application of Process Selections, the available data ranges for each numerical var were: ",
          "[",
          input$inNumVars[1],
          ": ",
          min(dt_updated[input$inNumVars[1]]),
          "..",
          max(dt_updated[input$inNumVars[1]]),
          "]  ",
          if(length(input$inNumVars)==2) {
            paste(
              "[",
              input$inNumVars[2],
              ": ",
              min(dt_updated[input$inNumVars[2]]),
              "..",
              max(dt_updated[input$inNumVars[2]]),
              "]  ",
              sep=''
            )
          },
          sep=''
        )      
    )
    
    #BASE output for categorical table 
    output$outSummaryCategorical <- renderDT({
      dt_updated |>
        group_by(catVars[1]) |>
        summarize(count = n())
    }) 
    
    #BASE output for numerical table
    output$outSummaryNumerical <- renderDT({
      dt_updated
        ##### update here with proper num summary stuff
    })
    
    #observe event of any edits / instances of IDs of interest?
    #observeEvent(input$inSummaryType, {print("HI")
    observe({
      output$outSummaryCategorical <-
        if (input$inSummaryCatVar2 == "~ None ~") {
          renderDT(
            dt_updated |> 
              group_by(!!sym(input$inSummaryCatVar1)) |>
              summarize(count = n())
          )
        } else {
          renderDT(
            dt_updated |> 
              group_by(!!sym(input$inSummaryCatVar1),!!sym(input$inSummaryCatVar2)) |>
              summarize(count = n()) |>
              pivot_wider(names_from = !!sym(input$inSummaryCatVar2), values_from = count)
          )
        }
      
    })

  })

  #render sliders
  output$outSlider1 <- renderUI({
    #####improve error checking?######
    #to avoid warnings, check value exists
    if(length(input$inNumVars)>=1) {
      
      min1=min(dt[input$inNumVars[1]])
      max1=max(dt[input$inNumVars[1]])
      
      sliderInput(
        "inSlider1",
        min=min1,
        max=max1,
        value=c(min1,max1),
        label=input$inNumVars[1]
      )
    }
  })
  output$outSlider2 <- renderUI({
    #to avoid warnings, check value exists
    if(length(input$inNumVars)==2) {
      
      min2=min(dt[input$inNumVars[2]])
      max2=max(dt[input$inNumVars[2]])
      
      sliderInput(
        "inSlider2",
        min=min2,
        max=max2,
        value=c(min2,max2),
        label=input$inNumVars[2]
      )
    }
  })
  
  #categorical var summary choices
  #first one is req'd
  output$outSummaryCatVar1 <- renderUI({
    selectizeInput(
      inputId = "inSummaryCatVar1",
      label = "Categorical Variable #1",
      choices = catVars,
      multiple = FALSE,
      width = 200
    )
  })
  output$outSummaryCatVar2 <- renderUI({
    selectizeInput(
      inputId = "inSummaryCatVar2",
      label = "Categorical Variable #2",
      choices = c("~ None ~",catVars),#[catVars != input$inSummaryCatVar1]), #awesome easy way to restrict a duplicate!
      multiple = FALSE,
      width = 200
    )     
  })
  
  #numerical var summary choices
  output$outSummaryNumVar <- renderUI({
    selectizeInput(
      inputId = "inSummaryNumVar",
      label = "Numerical Variable",
      choices = numVars,
      multiple = FALSE,
      width = 200
    )
  })
  output$outSummaryNumVarGroupBy <- renderUI({
    selectizeInput(
      inputId = "inSummaryNumVarGroupBy",
      label = "Group by",
      choices = c("~ None ~",catVars),
      multiple = FALSE,
      width = 200
    )     
  })
  


}

# Run the application 
shinyApp(ui = ui, server = server)
