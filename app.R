# This app will enable exploration of data representing mobile device attributes and behaviors.
# For further references, see associated [Quarto file](static-app-prototype.qmd).

#includes
library(shiny)
library(bslib)
library(DT)
library(tidyverse)

#vars and core data read into variable 'dt'
source("myhelpers.R")

#######################################################
# UI
#######################################################
ui <- fluidPage(

  # title
  titlePanel("Project 2: Exploring Mobile Device Data"),

  #author
  h3("Andy Powers"),
  h4(
    tags$a(
      href="mailto:apowers@ncsu.edu",
      "apowers@ncsu.edu")
    ),
  
  #typical sidebar style setup
  sidebarLayout(

    ##########################################
    # Sidebar
    ##########################################
    sidebarPanel(
      width=2,
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
      
      ### usage classes
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
        options = list(
          maxItems = 2,
          plugins = c("clear_button")
        )
      ),
      
      #little dynamic text to note and "warn" about sliders
      textOutput(
        outputId = "outSliderText"
      ),
      textOutput(
        outputId = "outSliderTextRange1"
      ),
      textOutput(
        outputId = "outSliderTextRange2"
      ),
      br(),
            
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
    
    ##########################################
    # Main Panel
    ##########################################
    mainPanel(
      
      #using pill tab structure
      navset_pill(
        
        ##########################################
        # About - Tab (default)
        ##########################################
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
        
        ##########################################
        # Data Download - Tab
        ##########################################
        nav_panel(
          title = "Data Download",
          p(),
          
          ##### save data via downloadButton
          downloadButton(
            outputId = "outDownloadButton",
            label = "Download Table as .csv"
          ),
          p(),
                        
          ##### display DT
          DTOutput(outputId="outDTOutput")
        ),
        
        ##########################################
        # Data Exploration - Tab
        ##########################################
        nav_panel(
          title = "Data Exploration",
          p(),
          
          ##########################################
          # Display Configuration - Section
          ##########################################
          h3("Display Configuration"),
          column(
            12,     
            
            #left column for Summary settings
            column( 
              6,
              h4("Summary"),
              
              #select cat or num
              radioButtons(
                inputId = "inSummaryType",
                label = "Type",
                choices = c("Categorical", "Numerical")
              ),
              
              #show numerical options if appropriate
              conditionalPanel(
                condition = "input.inSummaryType == 'Numerical'",
                uiOutput("outSummaryNumVar"),
                uiOutput("outSummaryNumVarGroupBy")
              ),            
              
              #show categorical options if appropriate
              conditionalPanel(
                condition = "input.inSummaryType == 'Categorical'",
                uiOutput("outSummaryCatVar1"),
                uiOutput("outSummaryCatVar2")
              )
            ),  
            
            #right column for Plot settings
            column(
              6,
              h4("Plots"),
              
              #x var - numeric, defaults to item 1, required
              selectizeInput(
                inputId = "inXVar",
                label = "Primary (x)",
                choices = numVars,
                multiple = FALSE,
                width = 200,
                selected = numVars[1]
              ),
              
              #y var - numeric, defaults to item 2, required
              selectizeInput(
                inputId = "inYVar",
                label = "Secondary (y)",
                choices = numVars,
                multiple = FALSE,
                width = 200,
                selected = numVars[2]
              ),               
              
              #"z" var - category, defaults to item 1, may select special 'none' item
              selectizeInput(
                inputId = "inZVar",
                label = "Category (Color/Fill/Facet)",
                choices = c("~ None ~",catVars),
                multiple = FALSE,
                width = 200,
                selected = catVars[1]
              ),
              
              #extra var z2 - category used only for box/whisker chart, defaults to item 2, required
              selectizeInput(
                inputId = "inZVar2",
                label = "Category 2 (Whisker only)",
                choices = catVars,
                multiple = FALSE,
                width = 200,
                selected = catVars[2]
              ) 
            )
          ),
          
          ##########################################
          # Summaries - Section
          ##########################################          
          h3("Summaries"),
          
          #render table as cat or numeric, per selection
          conditionalPanel(
            condition = "input.inSummaryType == 'Categorical'",
            DTOutput(outputId="outSummaryCategorical")
          ),
          conditionalPanel(
            condition = "input.inSummaryType == 'Numerical'",
            DTOutput(outputId="outSummaryNumerical")
          ),              
          
          ##########################################
          # Plots - Section
          ##########################################
          h3("Plots"),
          
          #split two plots to half-width
          column(
            12,
            
            #density plot
            column(
              6,
              plotOutput(
                outputId = "outDensityPlot"
              )
            ),
            
            #filled density plot
            column(
              6,
              plotOutput(
                outputId = "outFilledDensityPlot"
              )
            )
          ),
          
          #split two plots to half-width          
          column(
            12,
            
            #whisker plot
            column(
              6,
              plotOutput(
                outputId = "outBoxWhiskerPlot"
              )
            ),
            
            #scatter plot
            column(
              6,
              plotOutput(
                outputId = "outScatterPlot"
              )
            )
          ),
          
          #full width bin plot
          column(
            12,
            plotOutput(
              outputId = "outBin2DPlot"
            )
          ),
          
          #full width step plot with facets
          column(
            12,
            plotOutput(
              outputId = "outStepPlot"
            )
          )
        )
      )
    )
  )
)

#######################################################
# Server
#######################################################
server <- function(input, output, session) {
  
  #######################################################
  # Data Table Subset - reactive
  #######################################################
  dt_subset <- reactive({
    
    #start with full data table from source, subset by cat/num sidebar selections
    dt |> 
      filter(
        #categorical subsets
        if (input$inModel == "~ All ~") TRUE else input$inModel == dt$Model,
        if (input$inOS == "~ All ~") TRUE else input$inOS == dt$OS,
        if (input$inGender == "~ All ~") TRUE else input$inGender == dt$Gender,
        if (length(input$inAgeGroup) == 0) TRUE else dt$AgeGroup %in% input$inAgeGroup,
        if (length(input$inUsageClass) == 0) TRUE else dt$UsageClass %in% input$inUsageClass,
        
        #numeric slider1 range, if present
        if (length(input$inNumVars)>=1)
          (dt[input$inNumVars[1]] >= input$inSlider1[1]) & (dt[input$inNumVars[1]] <= input$inSlider1[2])
        else TRUE,
        
        #numeric slider2 range, if present
        if (length(input$inNumVars)==2) 
          (dt[input$inNumVars[2]] >= input$inSlider2[1]) & (dt[input$inNumVars[2]] <= input$inSlider2[2])
        else TRUE
      )
  })

  #######################################################
  # Observe Event for Button Press - control all relevant updates
  #######################################################
  observeEvent(input$inProcessButton,{
    
    #save subsetted dt
    dt_updated <- isolate(dt_subset())
    
    #render updated dt
    output$outDTOutput <- renderDT({
      dt_updated
    }) 
    
    #######################################################
    # Slider Notes - dynamic text
    #######################################################
    
    #line1
    output$outSliderText <- renderText(
      if(length(input$inNumVars)>=1)
        paste(
          "Slider ranges based on unfiltered data. As of last subset, available ranges were: ",
          sep=''
        )      
    )
    
    #slider1 range
    output$outSliderTextRange1 <- renderText(
      if(length(input$inNumVars)>=1)
        paste(
          input$inNumVars[1],
          ": [",
          min(dt_updated[input$inNumVars[1]]),
          "..",
          max(dt_updated[input$inNumVars[1]]),
          "]",
          sep=''
        )      
    )   
    
    #slider2 range
    output$outSliderTextRange2 <- renderText(
      if(length(input$inNumVars)==2)
        paste(
          input$inNumVars[2],
          ": [",
          min(dt_updated[input$inNumVars[2]]),
          "..",
          max(dt_updated[input$inNumVars[2]]),
          "]  ",
          sep=''
        )      
    )      
    
    #######################################################
    # RENDER ALL Data Exploration - REQUIRES tab field opened/exists
    #######################################################
    observe({ 
      req(input$inSummaryCatVar1)
      
      #######################################################
      # Render categorical summary 
      #######################################################
      output$outSummaryCategorical <-
        
        #render into a 1-way contingency table
        if (input$inSummaryCatVar2 == "~ None ~") { 
          renderDT({
            dt_updated |> 
              group_by(!!sym(input$inSummaryCatVar1)) |>
              summarize(count = n())
          })
        
        #render into a 2-way contingency table
        } else {
          renderDT({
            dt_updated |> 
              group_by(!!sym(input$inSummaryCatVar1),!!sym(input$inSummaryCatVar2)) |>
              summarize(count = n()) |>
              pivot_wider(names_from = !!sym(input$inSummaryCatVar2), values_from = count)
          })
        }
      
      #######################################################
      # Render numerical summary - REQUIRES 'numerical' button initiated
      #######################################################
      observe({ 
        req(input$inSummaryNumVarGroupBy)
        output$outSummaryNumerical <- 
          
          #render as aggregate numerical summary
          if (input$inSummaryNumVarGroupBy == "~ None ~") { 
            renderDT({
              dt_updated |>
                
                #keep numvar
                select(!!sym(input$inSummaryNumVar)) |>
                
                #summarize with key metrics
                summarize(across(
                  where(is.numeric),
                  list("mean" = ~ round(mean(.x),digits=3),
                       "median" = ~ round(median(.x),digits=3),
                       "sd" = ~ round(sd(.x),digits=3),
                       "iqr" = ~ round(IQR(.x),digits=3),
                       "n" = ~ n()
                  )
                ))
            })
            
          #render as grouped num summary by cat grouping var
          } else {
            renderDT({
              dt_updated |>
                
                #keep numvar and grouping cat var
                select(!!sym(input$inSummaryNumVar),
                       !!sym(input$inSummaryNumVarGroupBy)) |>
                
                #group by cat var
                group_by(!!sym(input$inSummaryNumVarGroupBy)) |>
                
                #summarize with key metrics
                summarize(across(
                  where(is.numeric),
                  list("mean" = ~ round(mean(.x),digits=3),
                       "median" = ~ round(median(.x),digits=3),
                       "sd" = ~ round(sd(.x),digits=3),
                       "iqr" = ~ round(IQR(.x),digits=3),
                       "n" = ~ n()
                  )
                ))
            })
          }
      })
      
      #######################################################
      # Render density plot
      #######################################################
      output$outDensityPlot <- renderPlot({
        g <- ggplot(dt_updated)
        g + 
          geom_density(aes(x = !!sym(input$inXVar))) +
          labs(
            title=
              paste(
                "Density by ",
                input$inXVar,
                sep=""
              ),
            x=input$inXVar,
            y="Density"
          )
      })
      
      #######################################################
      # Render filled density plot
      #######################################################
      output$outFilledDensityPlot <- renderPlot({
        g <- ggplot(dt_updated)
        g + 
          geom_density(
            aes(x = !!sym(input$inXVar),
                fill = !!sym(input$inZVar)),
            position = "fill") +
          labs(
            title=
              paste(
                "Filled Density by ",
                input$inXVar,
                " per ",
                input$inZVar,
                sep=""
              ),
            x=input$inXVar,
            y=
              paste(
                "Density per ",
                input$inZVar,
                sep=""
              )
          )
      })
      
      #######################################################
      # Render box-and-whisker plot
      #######################################################
      output$outBoxWhiskerPlot <- renderPlot({
        g <- ggplot(dt_updated)
        g + 
          geom_boxplot(
            aes(x=!!sym(input$inZVar2),
                y=!!sym(input$inYVar),
                fill=!!sym(input$inZVar)
            )
          ) +
          labs(
            title=
              paste(
                input$inZVar2,
                " by ",
                input$inYVar,
                " per ",
                input$inZVar,
                sep=""
              )
          )
      })
      
      #######################################################
      # Render scatter plot
      #######################################################
      output$outScatterPlot <- renderPlot({
        g <- ggplot(dt_updated)
        
        #leave out the color if not selected
        if(input$inZVar == "~ None ~")
          g + 
            geom_point(
              aes(x=!!sym(input$inXVar),
                  y=!!sym(input$inYVar)
              )
            ) +
            labs(
              title=
                paste(
                  input$inXVar,
                  " by ",
                  input$inYVar,
                  sep=""
                )
            )
        
        #include color if z var non-'none'
        else
          g + 
          geom_point(
            aes(x=!!sym(input$inXVar),
                y=!!sym(input$inYVar),
                color=!!sym(input$inZVar)
            )
          ) +
          labs(
            title=
              paste(
                input$inXVar,
                " by ",
                input$inYVar,
                " per ",
                input$inZVar,
                sep=""
              )
          )
      })
      
      #######################################################
      # Render bin-2d plot
      #######################################################
      output$outBin2DPlot <- renderPlot({
        g <- ggplot(dt_updated)
        g + 
          geom_bin2d(
            aes(x=!!sym(input$inXVar),
                y=!!sym(input$inYVar)
            ),
            binwidth=0.25
          ) +
          labs(
            title=
              paste(
                input$inXVar,
                " by ",
                input$inYVar,
                sep=""
              )
          )
      })
      
      #step
      output$outStepPlot <- renderPlot({
        g <- ggplot(dt_updated)
        g + 
          geom_step(
            aes(x=!!sym(input$inXVar),
                y=!!sym(input$inYVar)
            )
          ) +
          labs(
            title=
              paste(
                input$inXVar,
                " by ",
                input$inYVar,
                " faceted by ",
                input$inZVar,
                sep=""
              )
          ) +
          facet_wrap(. ~ .data[[input$inZVar]])
            
      })

    })
    
    #download csv button
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
