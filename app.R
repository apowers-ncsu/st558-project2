# This app will enable exploration of data representing mobile device attributes and behaviors.
# For further references, see associated [Quarto file](static-app-prototype.qmd).

library(shiny)

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
            #inputs here
        ),

        # main panel with tabs
        
        ### About tab, default start
        ##### Purpose of app
        ##### Data, source, link
        ##### tell purpose of sidebar and each tab
        ##### relevant picture
        
        ### Data Download tab
        ##### display with dataTableOutput / renderDataTable
        ##### subset per selections/action button
        ##### save data via downloadButton
        
        ### Data Exploration tab
        ##### obtain the num/graph summaries from the prototype build contents
        ##### design as I like - subtabs, dynamic UI, etc.
        ##### subset when selected/action
        ##### user can display cat or num var summaries
        ##### user selects any cat or num vars to summarize, plus any modifying the summary
        ####### for ex: select num var for num summary, plus cat var to summarize across
        ####### for ex: plots, select which var on x or y, coloring, etc.
        ##### account for error messages, plus loading spinners for waits
        mainPanel(
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
