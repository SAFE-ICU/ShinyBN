library('bnlearn')
library('networkD3')
library('rhandsontable')
library('shiny')
library('shinydashboard')
library('dplyr')
library('visNetwork')
library('shinyWidgets')
library('missRanger')
library('tools')
library('shinyalert')
library('shinycssloaders')
source('error.bar.R')
library('shinythemes')


myDashboardHeader <- function (..., title = NULL, titleWidth = NULL, disable = FALSE,
                               .list = NULL) {
  items <- c(list(...), .list)
  # lapply(items, tagAssert, type = "li", class = "dropdown")
  titleWidth <- validateCssUnit(titleWidth)
  custom_css <- NULL
  if (!is.null(titleWidth)) {
    custom_css <- tags$head(tags$style(HTML(gsub("_WIDTH_",
                                                 titleWidth, fixed = TRUE, "\n      @media (min-width: 768px) {\n .main-header > .navbar {\n  margin-left: _WIDTH_;text-align: left;\n }\n        .main-header .logo {\n          width: _WIDTH_;\n        }\n      }\n    "))))
  }
  tags$header(class = "main-header", custom_css, style = if (disable)
    "display: none;", span(class = "logo", title), tags$nav(class = "navbar navbar-static-top",
                                                            role = "navigation", span(shiny::icon("bars"), style = "display:none;"),
                                                           # a(href = "#", class = "sidebar-toggle", `data-toggle` = "offcanvas",
                                                           #  role = "button", span(class = "sr-only", "Toggle navigation")),
                                                            div(class = "navbar-custom-menu", tags$ul(class = "nav navbar-nav",
                                                                                                      items))))
}

dashboardPage(skin = "blue",
              myDashboardHeader(title = "ShinyBN",
                                titleWidth = "400"

              ),
              dashboardSidebar(width = 40,
                               sidebarMenu(id = "sidebarMenu",
                                           menuItem(text = "",
                                                    tabName = "About",
                                                    icon = icon("info")
                                           ),
                                           menuItem(text = "",
                                                    icon = shiny::icon("globe"),
                                                    tabName = "structure"
                                           )


                               )
              ),
              dashboardBody(id ="dashboardBody",
                            tags$head(tags$link(rel = "stylesheet",type = "text/css", href = "style.css")),
                            useShinyalert(),
                            #shinythemes::themeSelector(),
                            #theme = shinytheme("united"),
                            tags$script(HTML("$('body').addClass('fixed');")),
                            shinydashboard::tabItems(
                            shinydashboard::tabItem(tabName = "About",
                                                      fluidRow(box(#title = "",
                                                                   status = "primary",
                                                                   width = 12,
                                                                   div(style = 'overflow-y: scroll'),
                                                                   #shiny::h4("Note*: The Demo version of the app is running for the Iris data set freely available in R"),
                                                                   shiny::h1("ShinyBN"),
                                                                   shiny::h3("Bayesian Network Modelling and Inferencing"),
                                                                   shiny::h5("ShinyBN is a ",
                                                                            shiny::a(href = 'https://rstudio.github.io/shinydashboard/', 'shinydashboard'),
                                                                            " for Bayesian network modeling and inferencing, ",
                                                                            "powered by",
                                                                             shiny::a(href = 'http://www.bnlearn.com', 'bnlearn'),
                                                                             'and',
                                                                             shiny::a(href = 'http://datastorm-open.github.io/visNetwork/', 'visNetwork'),
                                                                             '.'
                                                                   ),
                                                                   shiny::h5('This app is a more general version of the ',
                                                                             shiny::a(href = 'https://github.com/paulgovan/BayesianNetwork', 'BayesianNetwork'),
                                                                             'web app.')

                                                                   )
                                                               )
                                                      ),
                              shinydashboard::tabItem(tabName = "structure",
                                                      shiny::fluidRow(
                                                        shiny::column(
                                                          width = 3,
                                                          offset = -10,
                                                          style='padding:0px;margin:0px',
                                                          tabBox(width=12,id="control_tabs",
                                                                 tabPanel("Data",
                                                                          shiny::helpText("Select prefered input format(RData suggested for data > 100mb)"),
                                                                          shiny::selectInput('format','Data Format',c(".RData",".CSV")),
                                                                          shiny::helpText("Upload your data:"),
                                                                          shiny::fileInput('dataFile',
                                                                                           strong('File Input:'),
                                                                                           accept = c('.RData','.csv')
                                                                                           ),
                                                                          shiny::helpText("Impute Missing Data"),
                                                                          actionButton('impute','impute missingess'),
                                                                          shiny::helpText('Discretize Data'),
                                                                          shiny::selectInput('dtype','Discretization Type',c("interval", "quantile")),
                                                                          actionButton('discretize',"Discretize")
                                                                          ),
                                                                 tabPanel("Graph",
                                                                          status = "primary",
                                                                          helpText("update graph to view selected chain of inference"),

                                                                          shiny::fluidRow(shiny::column(12,actionButton('graphBtn', 'Update Graph'))),
                                                                          sliderInput("degree", "chain of neighbors",
                                                                                      min = 1, max = 5,
                                                                                      value = 2
                                                                                      ),
                                                                          shiny::selectInput('graph_layout','Layout',"layout_nicely"),
                                                                          shiny::helpText("Save your network graph"),
                                                                          actionButton('saveBtn2','Save Graph'),
                                                                          textInput('path2','Enter Directory with file Name', value = "file type .csv", width = NULL, placeholder = NULL)
                                                                          ),
                                                                 tabPanel("Learning",
                                                                          status = "primary",
                                                                          shiny::helpText("Learn structure or upload learned structure"),
                                                                          shinyWidgets::radioGroupButtons(inputId = "net",
                                                                                                          choices = c("Learn Structure" = 2,
                                                                                                                      "Upload Structure" = 1),
                                                                                                          selected = 1,
                                                                                                          justified = FALSE
                                                                          ),

                                                                          # Conditional panel for uploading structure
                                                                          shiny::conditionalPanel(
                                                                            condition = "input.net == 1",
                                                                            # File input
                                                                            shiny::p("Note: Upload .RData file"),
                                                                            shiny::fileInput(
                                                                              'structFile',
                                                                              strong('File Input:'),
                                                                              accept = c('.RData')
                                                                            )
                                                                          ),

                                                                          # Conditional panel for learning structure
                                                                          shiny::conditionalPanel(
                                                                            condition = "input.net == 2",
                                                                            shiny::helpText("Select a structural learning algorithm:"),
                                                                            # Structural learning algorithm input select
                                                                            shiny::selectizeInput(
                                                                              inputId = "alg",
                                                                              shiny::h5("Learning Algorithm:"),
                                                                              choices = list(
                                                                                "Score-based Learning" =
                                                                                  c("Hill Climbing" = "hc",
                                                                                    "Tabu" = "tabu"),
                                                                                "Constraint-based Learning" =
                                                                                  c("Grow-Shrink" = "gs",
                                                                                    "Incremental Association" = "iamb",
                                                                                    "Fast IAMB" = "fast.iamb",
                                                                                    "Inter IAMB" = "inter.iamb"
                                                                                  ),
                                                                                "Hybrid Learning" =
                                                                                  c("Max-Min Hill Climbing" = "mmhc",
                                                                                    "2-phase Restricted Maximization" = 'rsmax2'
                                                                                  ),
                                                                                "Local Discovery Learning" =
                                                                                  c("Max-Min Parents and Children" = 'mmpc',
                                                                                    "Semi-Interleaved HITON-PC" = "si.hiton.pc",
                                                                                    "ARACNE" = "aracne",
                                                                                    "Chow-Liu" = "chow.liu"
                                                                                  )
                                                                              )
                                                                            ),
                                                                            sliderInput("boot", "Bootstrap replicates",
                                                                                        min = 1, max = 1000,
                                                                                        value = 10),
                                                                            sliderInput("SampleSize", "Proportion of sample for Bootstrap:",
                                                                                        min = 0, max = 1,
                                                                                        value = 0.7),
                                                                            sliderInput("edgeStrength", "Edge Strength",
                                                                                        min = 0, max = 1,
                                                                                        value = 0.5),
                                                                            sliderInput("directionStrength", "Direction Confidence:",
                                                                                        min = 0, max = 1,
                                                                                        value = 0.5),


                                                                            actionButton('learnBtn', 'Learn'),
                                                                            actionButton('learnSBtn','Learn simple'),
                                                                            shiny::helpText("Save your learned structure to save time"),
                                                                            actionButton('saveBtn','Save Structure'),
                                                                            textInput('path','Enter Directory with file Name', value = "file type .RData", width = NULL, placeholder = NULL)

                                                                          )
                                                                 ),
                                                                 tabPanel("Inference",
                                                                          status = "primary",
                                                                          shiny::helpText("Display plot in inferece plot tab"),
                                                                          shiny::fluidRow(shiny::column(3,actionButton('plotBtn', 'Simple')),shiny::column(4,actionButton('plotStrengthBtn', 'Confidence'))),
                                                                          shiny::helpText("No, of iterations run for confidence plot"),
                                                                          sliderInput("numInterval", "No. of confidence intervals",
                                                                                      min = 1, max = 500,
                                                                                      value = 25
                                                                          ),
                                                                          helpText("Select evidence to add to the model:"),
                                                                          shiny::fluidRow(shiny::column(6,actionButton('insertBtn', 'Insert')),
                                                                                          shiny::column(6,actionButton('removeBtn', 'Remove'))
                                                                          ),
                                                                          shiny::fluidRow(shiny::column(6,tags$div(id = 'placeholder1')),
                                                                                          shiny::column(6,tags$div(id = 'placeholder2'))
                                                                          ),
                                                                          helpText("Select an event of interest:"),
                                                                          shiny::selectInput("event",
                                                                                             label = shiny::h5("Event Node:"),
                                                                                             "")
                                                                          )

                                                                 )

                                                        ),
                                                        shiny::column(
                                                          width = 9,
                                                          height = "auto",
                                                          offset = -1,
                                                          style='padding:0px;margin:0px',
                                                          tabBox(id = "visula_tabs",
                                                                 width = 12,

                                                                 tabPanel("Network Graph",
                                                                          withSpinner(visNetworkOutput("netPlot",height = "600px")), color= "#ff69b4"),
                                                                 tabPanel("Inference Plot",
                                                                          withSpinner(plotOutput("distPlot",height = "600px")), color="#ff69b4")
                                                                 )
                                                          )

                                                      )
                              )

)
)
)
