# app.R
library(shiny)
library(ggplot2)
library(palmerpenguins)
library(shinythemes)

# Data from palmerpenguins R package
peng_df <- penguins

# UI ---------------------------------------------------------------------------

ui <- navbarPage(
  "Palmer Penguins App",
  theme = shinytheme("flatly"),
  ## Plots Tab -----------------------------------------------------------------
  tabPanel("Plots", # tabPanel elements need to be nested in another UI such as navbarPage() or tabsetPanel()
           sidebarLayout( # Note that sidebarPanel() and mainPanel() are within sidebarLayout(), I think I need sidebarLayout() to make the sidebar tabPanels work
             ### Sidebar -------------------------------------------------------
             sidebarPanel(
               width = 3, # Default width is 4, I like 3 :)
               tabsetPanel(
                 id = "SidePanelPlots",
                 #### Data Input -----------------------------------------------
                 tabPanel("Data Input",
                          p(), # I use this often to just add a small bit of buffer space between some elements
                          conditionalPanel( # Conditional panel dependent on main panel tab selected, '1' relates to the 'value' of this panel in the main panel
                            condition = "input.MainPanelPlots == '1'",
                            # columns are a nice way to place multiple UI elements on the same line
                            # columns need to be in a fluidRow() and the max total width of the row is 12
                            fluidRow(
                              column(6,
                                     selectInput("xvar", "X-axis:", 
                                                 choices = names(peng_df)[3:6], 
                                                 selected = "bill_length_mm"),
                                     selectInput("colorvar", "Color by:", 
                                                 choices = c("species", "island", "sex"), 
                                                 selected = "species")
                              ),
                              column(6,
                                     selectInput("yvar", "Y-axis:", 
                                                 choices = names(peng_df)[3:6], 
                                                 selected = "bill_depth_mm"),
                                     radioButtons("size_option", "Point size:",
                                                  choices = c("Fixed" = "fixed", "By variable" = "var"),
                                                  selected = "fixed", inline = T),
                                     conditionalPanel( # Conditional panel dependent on radioButtons
                                       condition = "input.size_option == 'fixed'",
                                       numericInput("fixed_size", "Set dot size:", value = 3, min = 1, max = 10, step = 0.5)
                                     ),
                                     conditionalPanel(
                                       condition = "input.size_option == 'var'",
                                       selectInput("sizevar", "Size by variable:",
                                                   choices = names(peng_df)[3:6],
                                                   selected = "flipper_length_mm")
                                     )
                              )
                            )
                          ),
                          conditionalPanel(
                            condition = "input.MainPanelPlots == '2'",
                            fluidRow(
                              column(6,
                                     selectInput("histvar", "Choose variable:",
                                                 choices = names(peng_df)[3:6],
                                                 selected = "flipper_length_mm")
                              ),
                              column(6,
                                     numericInput("bins", "Number of bins:", min = 5, max = 50, value = 20)
                              )
                            ),
                            fluidRow(
                              column(6,
                                     checkboxInput("facet_species", "Facet by category?", value = FALSE),
                                     checkboxInput("fill_species", "Color by category?", value = FALSE)
                              ),
                              column(6,
                                     conditionalPanel( # Conditional panel dependent on checkboxInput
                                       condition = "input.facet_species == true",
                                       selectInput("facet_by", "Facet by:",
                                                   choices = c("species", "island", "sex"))
                                     ),
                                     conditionalPanel(
                                       condition = "input.fill_species == true",
                                       selectInput("fill_by", "Fill by:",
                                                   choices = c("species", "island", "sex"))
                                     )
                              )
                            )
                            )
                          ),
                 #### Figure Settings ------------------------------------------
                 tabPanel("Figure Settings",
                          conditionalPanel(
                            condition = "input.MainPanelPlots == '1'",
                            
                            ),
                          conditionalPanel(
                            condition = "input.MainPanelPlots == '2'",
                            
                          ),
                          # I have it using the same parameters for each plot, but you can also split to conditional
                          # Just make sure the id for the UI element is distinct
                          h4("Font Sizes"),
                          fluidRow(
                            column(4,
                                   numericInput("xTitleSize","X-Axis Title:",
                                                value = 14, step = 1),
                                   numericInput("xTickSize","X-Axis Ticks:",
                                                value = 12, step = 1)
                            ),
                            column(4,
                                   numericInput("yTitleSize","Y-Axis Title:",
                                                value = 14, step = 1),
                                   numericInput("yTickSize","y-Axis Ticks:",
                                                value = 12, step = 1)
                            ),
                            column(4,
                                   numericInput("legendTitle","Legend Title:",
                                                value = 14, step = 1),
                                   numericInput("legendText","Legend Text:",
                                                value = 12, step = 1)
                            )
                          ),
                          h4("Figure Download Size"),
                          fluidRow(
                            column(4,
                                   numericInput("FigHeight","Height:",value = 8,min = 1,step = 1)
                                   ),
                            column(4,
                                   numericInput("FigWidth","Width:",value = 10,min = 1,step = 1)
                                   ),
                            column(4,
                                   selectInput("FigUnits","Units",choices = c("in", "cm", "mm", "px"))
                                   )
                          )
               )
             )
             ),
             ### MainPanel -----------------------------------------------------
             mainPanel(
               tabsetPanel(
                 id = "MainPanelPlots",
                 tabPanel("Scatter Plot",
                          plotOutput("scatterPlot", height = "500px", width = "100%"),
                          p(),
                          downloadButton("dnldScatterPlot", "Download Plot SVG"),
                          value = 1 # This is the value to use when making the conditionalPanel() '1'
                 ),
                 tabPanel("Histogram",
                          plotOutput("histPlot", height = "500px", width = "100%"),
                          p(),
                          downloadButton("dnldhistPlot", "Download Plot SVG"),
                          value = 2
                 )
               ),
               p(),
               # div() is helpful to set some HTML elements
               div(DT::dataTableOutput("PenguinDataTable"), style = "font-size:14px"),
               downloadButton("dnldPenguinDataTable", "Download Table")
             )
           )
  )
  )

server <- function(input, output, session) {
  
  # Penguin Data ---------------------------------------------------------------
  
  # Ideas:
  ## Maybe add a tab for subsetting the data
  ### Could use shinyWidgets to facilitate
  
  output$PenguinDataTable <- DT::renderDataTable({
    DT::datatable(peng_df,
                  extensions = 'Scroller',
                  options = list(lengthMenu = c(5,10, 20, 100, 1000),
                                 pageLength = 10,
                                 scrollX = T),
                  rownames = F
    )
  })
  output$dnldPenguinDataTable <- downloadHandler(
    filename = function() {
      data_format <- format(Sys.Date(),"%Y%m%d")
      paste0("PengiunData_",data_format,".txt")
    },
    content = function(file) {
      write.table(peng_df,file, sep = '\t', row.names = F)
    }
  )
  
  
  
  # Scatter --------------------------------------------------------------------
  
  # Ideas:
  ## Could add point annotation feature with the table below the plot
  ### Select a row or multiple rows and use ggrepel package to annotate the point on the plot
  
  scatterPlot_react <- reactive({
    # I like to assign my variables to names at the beginning because this helps in the troubleshooting step explained below
    # If these were not saved to names, they would not be in the environment, and would not be saved
    # You don't have to do this, I know it can get long where there are so many parameters, or you don't have to list all of them here
    peng_df <- peng_df
    xVar <- input$xvar
    yVar <- input$yvar
    colVar <- input$colorvar
    sizeOpt <- input$size_option
    sizeVar <- input$sizevar
    fixedSizeVar <- input$fixed_size
    xTitle <- input$xTitleSize
    xTick <- input$xTickSize
    yTitle <- input$yTitleSize
    yTick <- input$yTickSize
    legTitle <- input$legendTitle
    legText <- input$legendText
    
    # This is a helpful trick to saving the environment within this reactive
    # This will be saved in the directory of the app.R file
    # You can then click on the file and it will load the items into your environment
    # This helps you see what is happening in the backend and manually work through issues
    #save(list = ls(), file = "scatterPlot_react.RData", envir = environment())
    
    # Base mapping
    # Using !!sym() around variable names allows you to call a variable in the ggplot function
    # Without !!sym(xVar) the function would look for the xVar label in the plot data
    # But it is not there, but it is a symbol that represents the actual name and will find it like that
    aes_map <- aes(x = !!sym(xVar), y = !!sym(yVar), color = !!sym(colVar))
    # Add size mapping depending on user choice
    if (sizeOpt == "var") {
      req(sizeVar)
      aes_map <- modifyList(aes_map, aes(size = !!sym(sizeVar)))
      p <- ggplot(peng_df, aes_map) +
        geom_point(na.rm = TRUE) +
        theme_minimal()
    } else {
      p <- ggplot(peng_df, aes_map) +
        geom_point(size = fixedSizeVar, na.rm = TRUE) +
        theme_minimal()
    }
    p <- p +
      theme(axis.title.x = element_text(size = xTitle),
            axis.text.x = element_text(size = xTick),
            axis.title.y = element_text(size = yTitle),
            axis.text.y = element_text(size = yTick),
            legend.title = element_text(size = legTitle),
            legend.text = element_text(size = legText))
    p
  })
  output$scatterPlot <- renderPlot({
    req(scatterPlot_react())
    scatterPlot_react()
  })
  output$dnldScatterPlot <- downloadHandler(
    filename = function() {
      data_format <- format(Sys.Date(),"%Y%m%d")
      paste0("PengiunData_ScatterPlot",data_format,".svg")
    },
    content = function(file) {
      ggsave(file,scatterPlot_react(), height = input$FigHeight, width = input$FigWidth, units = input$FigUnits)
    }
  )
  
  
  
  # Histogram ------------------------------------------------------------------
  histPlot_react <- reactive({
    histVar <- input$histvar
    binN <- input$bins
    fillOpt <- input$fill_species
    fillVar <- input$fill_by
    facetOpt <- input$facet_species
    facetVar <- input$facet_by
    xTitle <- input$xTitleSize
    xTick <- input$xTickSize
    yTitle <- input$yTitleSize
    yTick <- input$yTickSize
    legTitle <- input$legendTitle
    legText <- input$legendText
    
    aes_map <- aes(x = !!sym(histVar))
    if (fillOpt != "None") {
      aes_map <- modifyList(aes_map, aes(fill = !!sym(fillVar)))
    }
    p <- ggplot(peng_df, aes_map) +
      geom_histogram(bins = binN, color = "white", na.rm = TRUE) +
      theme_minimal()
    if (facetOpt) {
      p <- p + facet_wrap(as.formula(paste("~", facetVar)))
    }
    p <- p +
      theme(axis.title.x = element_text(size = xTitle),
            axis.text.x = element_text(size = xTick),
            axis.title.y = element_text(size = yTitle),
            axis.text.y = element_text(size = yTick),
            legend.title = element_text(size = legTitle),
            legend.text = element_text(size = legText))
    p
  })
  output$histPlot <- renderPlot({
    req(histPlot_react())
    histPlot_react()
  })
  output$dnldhistPlot <- downloadHandler(
    filename = function() {
      data_format <- format(Sys.Date(),"%Y%m%d")
      paste0("PengiunData_Histogram",data_format,".svg")
    },
    content = function(file) {
      ggsave(file,histPlot_react(), height = input$FigHeight, width = input$FigWidth, units = input$FigUnits)
    }
  )
  
  
  
}

shinyApp(ui, server)
