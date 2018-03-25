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
library('rintrojs')
library('arules')
library('rcompanion')
library('psych')
library('DescTools')
library("DT")
library("linkcomm")
library('igraph')
library("parallel")
library("snow")
library("shinyBS")
source('error.bar.R')
source('graph.custom.R')
source('graph.custom.assoc.R')
source('custom.discretize.R')
source('check.NA.R')
source('check.discrete.R')
source('custom.association.R')
source('custom.Modules.R')


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
                                titleWidth = "400",
                                tags$li(class = "dropdown", bsButton("homeIntro", label = NULL, icon = icon("question-circle", lib="font-awesome"), style = "primary", size = "large")),
                                tags$li(class = "dropdown", bsButton("homeIntro", label = NULL, icon = icon("info"), style = "primary", size = "large"))
              ),
              dashboardSidebar(width = 50,
                               sidebarMenu(id = "sidebarMenu",
                                           menuItem(text = "",
                                                    tabName = "Home",
                                                    icon = icon("home")
                                           ),
                                           menuItem(text = "",
                                                    icon = shiny::icon("globe"),
                                                    tabName = "Structure"
                                           ),
                                           menuItem(text = "",
                                                    icon = shiny::icon("github"),
                                                    href = "https://github.com/SAFE-ICU/ShinyBN"),
                                          menuItem(text = "",
                                                    icon = shiny::icon("info"),
                                                  tabName = "About"))
                               ),
              dashboardBody(id ="dashboardBody",
                            # Include shinyalert Ui
                            useShinyalert(),
                            # Include introjs UI
                            rintrojs::introjsUI(),
                            #shinythemes::themeSelector(),
                            #theme = shinytheme("united"),
                            tags$script(HTML("$('body').addClass('fixed');")),
                            shinydashboard::tabItems(
                            shinydashboard::tabItem(tabName = "Home",
                                                      fluidRow(box(#title = "",
                                                                   status = "primary",
                                                                   width = 12,
                                                                   div(style="text-align:center",
                                                                       shiny::img(src = "placeholder-logo.png",height = 110,width = 110),
                                                                       shiny::h1("ShinyBN"),
                                                                       shiny::h2("Democratizing Bayesian Networks for Data Driven Decisions")
                                                                   ),
                                                                   br(),
                                                                   hr(),
                                                                   fluidRow(
                                                                     style = "margin-left:40px;padding:10px;",
                                                                     column(width=3, align = "center", h4('Learn Knowledge Network')),
                                                                     column(width=1, align = "center", img(src = "arrow.png",height = 40,width = 60)),
                                                                     column(width=3, align = "center", h4('Engineer and Assess Insights')),
                                                                     column(width=1, align = "center", img(src = "arrow.png",height = 40,width = 60)),
                                                                     column(width=3, align = "center", h4('Take Decisions'))
                                                                   ),
                                                                   hr(),
                                                                   div(style="text-align:center",
                                                                       actionButton("start", "Start Analyzing", style  = "background-color:#2E86C1;color:white;height:50px;font-size:20px", width = '300px', align = "center")
                                                                   )
                                                               )

                                                      )),
                              shinydashboard::tabItem(tabName = "Structure",
                                                      shiny::fluidRow(
                                                        shiny::column(
                                                          width = 3,
                                                          offset = -10,
                                                          style='padding:0px;margin:0px',
                                                          tabBox(width=12,id="control_tabs",
                                                                 tabPanel("App",
                                                                          shiny::fluidRow(column(3,materialSwitch(inputId = "parallel", label = "go parallel", status = "primary", right = FALSE)),column(6,selectInput("clusters",choices = c(1:20),label = "#clusters")))
                                                                          ),
                                                                 tabPanel("Data",
                                                                          div(id = "data",
                                                                              shinyWidgets::radioGroupButtons(inputId = "dataOptions",
                                                                                                              choices = c("Upload","Pre-process","Explore"),
                                                                                                              selected = "Upload",
                                                                                                              justified = FALSE
                                                                                                              ),

                                                                              shiny::conditionalPanel(
                                                                                condition = "input.dataOptions == 'Upload'",
                                                                                shiny::h4("Upload Data:"),
                                                                                shiny::helpText("Select prefered input format(RData suggested for data > 100mb)"),
                                                                                h5('Data Format:'),
                                                                                shiny::selectInput('format',label = NULL,c(".RData",".CSV")),
                                                                                h5('File Input:'),
                                                                                shiny::fileInput('dataFile',
                                                                                                 label = NULL,
                                                                                                 accept = c('.RData','.csv')
                                                                                )
                                                                              ),
                                                                              shiny::conditionalPanel(
                                                                                condition = "input.dataOptions=='Pre-process'",
                                                                                hr(),
                                                                                div(id="dataImpute",
                                                                                    shiny::h4("Impute Missing Data:"),
                                                                                    actionButton('impute','Impute')),
                                                                                hr(),
                                                                                div(id="dataDiscretize",
                                                                                    shiny::h4('Discretize Data'),
                                                                                    h5('Discretization Type:'),
                                                                                    shiny::fluidRow(column(6,shiny::selectInput('dtype',label = NULL,c("interval","quantile","frequency","cluster","hybrid"))),column(6,actionButton('discretize',"Discretize")))
                                                                                    #h5("subset columns in data using the tables")

                                                                              )),
                                                                              shiny::conditionalPanel(
                                                                                condition = "input.dataOptions=='Explore'",
                                                                                h5("Association Network"),
                                                                                shiny::fluidRow(column(6,shiny::selectInput('assocType',label = NULL,c("cramer's V","Cohen's D","Goodman Kruskal lambda","Tschuprow's T"))),column(6,actionButton('association',"Build")))
                                                                                #h5("subset association networks using the tables")
                                                                              )
                                                                              )
                                                                          ),
                                                                 tabPanel("Learning",
                                                                          status = "primary",
                                                                          shiny::h4("Structure"),
                                                                          shinyWidgets::radioGroupButtons(inputId = "net",
                                                                                                          choices = c("Learn" = 2,
                                                                                                                      "Upload" = 1,
                                                                                                                      "Validate" = 3),
                                                                                                          selected = 1,
                                                                                                          justified = FALSE
                                                                          ),

                                                                          # Conditional panel for uploading structure
                                                                          shiny::conditionalPanel(
                                                                            condition = "input.net==3",
                                                                            h5("Validation Method"),
                                                                            shiny::selectInput('crossFunc',label = NULL,choices = c("k-fold","hold-out")),
                                                                            h5("Parameter Fitting Method"),
                                                                            shiny::selectInput('paramMethod3',label = NULL,choices = c("Maximum Likelihood parameter estimation" = "mle","Bayesian parameter estimation" = "bayes")),
                                                                            h5("Loss function"),
                                                                            shiny::selectInput('lossFunc',label = NULL,choices = c("pred","pred-lw")),
                                                                            shiny::actionButton("calLoss","Cross Validate"),
                                                                            h5("Log-Likelihood Loss of the learned model"),
                                                                            shiny::verbatimTextOutput("valLoss")

                                                                          ),
                                                                          shiny::conditionalPanel(

                                                                            condition = "input.net == 1",
                                                                            h5("Paramter learning type"),
                                                                            selectizeInput('paramMethod',label = NULL,choices = c("Maximum Likelihood parameter estimation" = "mle","Bayesian parameter estimation" = "bayes")),
                                                                            hr(),
                                                                            div(style ='overflow-y:scroll',
                                                                            # File input
                                                                            shiny::p("Note: Upload .RData file"),
                                                                            shiny::fileInput(
                                                                              'structFile',
                                                                              strong('File Input:'),
                                                                              accept = c('.RData')
                                                                            )
                                                                          )),
                                                                          hr(),

                                                                          # Conditional panel for learning structure
                                                                          shiny::conditionalPanel(
                                                                            condition = "input.net == 2",
                                                                            div(style ='overflow-y:scroll;height:600px;padding-right:20px;',
                                                                            shiny::h4("Structural learning"),
                                                                            # Structural learning algorithm input select
                                                                            shiny::selectizeInput(
                                                                              inputId = "alg",
                                                                              shiny::h5("Learning Algorithm:"),
                                                                              choices = list(
                                                                                "Score-based Learning(recommended)" =
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
                                                                                    "2-phase Restricted Maximization" = 'rsmax2',
                                                                                    "PC" = "pc.stable"
                                                                                  ),
                                                                                "Local Discovery Learning" =
                                                                                  c("Max-Min Parents and Children" = 'mmpc',
                                                                                    "Semi-Interleaved HITON-PC" = "si.hiton.pc",
                                                                                    "ARACNE" = "aracne",
                                                                                    "Chow-Liu" = "chow.liu"
                                                                                  )
                                                                              )
                                                                            ),
                                                                            hr(),
                                                                            h5("Bootstrap replicates"),
                                                                            sliderInput("boot", label = NULL,
                                                                                        min = 1, max = 1000,
                                                                                        value = 10),
                                                                            hr(),
                                                                            h5("Proportion of sample for Bootstrap:"),
                                                                            sliderInput("SampleSize", label = NULL,
                                                                                        min = 0, max = 1,
                                                                                        value = 0.7),
                                                                            hr(),
                                                                            h5("Edge Strength"),
                                                                            sliderInput("edgeStrength", label = NULL,
                                                                                        min = 0, max = 1,
                                                                                        value = 0.5),
                                                                            hr(),
                                                                            h5("Direction Confidence:"),
                                                                            sliderInput("directionStrength", label = NULL,
                                                                                        min = 0, max = 1,
                                                                                        value = 0.5),
                                                                            h5("Parameter Learning Type"),
                                                                            selectizeInput("paramMethod2",label = NULL,choices = c("Maximum Likelihood parameter estimation" = "mle","Bayesian parameter estimation" = "bayes")),
                                                                            h5("Inject Expert Knowledge by Forcing/Prohibiting Edges"),
                                                                            shiny::fluidRow(shiny::column(6,selectInput("listType",label = NULL,choices = c("Blacklist","Whitelist"))),shiny::column(6,shiny::fileInput('listFile',label = NULL,accept = c('.csv')))),
                                                                            actionButton('learnBtn', 'Bootstrap'),
                                                                            actionButton('learnSBtn','Direct'),
                                                                            hr(),
                                                                            shiny::h5("Save learned structure"),
                                                                            downloadButton('saveBtn','Save')
                                                                          ))
                                                                 ),
                                                                 tabPanel("Graph",
                                                                          #status = "primary",
                                                                          div(id="graph",
                                                                              h5('group of variables:'),
                                                                              shiny::fluidRow(shiny::column(6,selectizeInput('varselect',label = "Variables","",multiple = T)),
                                                                                              shiny::column(6,selectInput('varshape',label = "Shape",""))
                                                                              ),
                                                                              actionButton('group','Group Variables'),
                                                                              hr(),
                                                                              h5('vector of index:'),
                                                                              shiny::fluidRow(shiny::column(6,textInput('varselectvector',label = "Variables")),
                                                                                              shiny::column(6,selectInput('varshape2',label = "Shape",""))
                                                                              ),
                                                                              actionButton('group2','Group Variables'),
                                                                              div(id = "graphChain",
                                                                                  h4("Chain of Neighbours"),
                                                                                  sliderInput("degree", label = NULL,
                                                                                              min = 1, max = 10,
                                                                                              value = 2
                                                                                  )),
                                                                              div(id = "NChain",
                                                                                  h4("Nth Neighbours"),
                                                                                  sliderInput("degreeN", label = NULL,
                                                                                              min = 1, max = 10,
                                                                                              value = 2
                                                                                  )),
                                                                              hr(),
                                                                              div(id="graphLayout",
                                                                                  h4("Select Graph Layout"),
                                                                                  shiny::selectInput('graph_layout',label = NULL,"layout_nicely"))
                                                                          )),
                                                                 tabPanel("Inference",
                                                                          status = "primary",
                                                                          shiny::h4("Display inference plot"),
                                                                          shiny::fluidRow(shiny::column(5,actionButton('plotBtn', 'Simple Plot')),shiny::column(4,actionButton('plotStrengthBtn', 'Confidence Plot'))),
                                                                          hr(),
                                                                          shiny::h4("No of iterations for confidence plot"),
                                                                          sliderInput("numInterval", label = NULL,
                                                                                      min = 1, max = 500,
                                                                                      value = 25
                                                                          ),
                                                                          hr(),
                                                                          h4("Select evidence to add to the model"),
                                                                          shiny::fluidRow(shiny::column(6,actionButton('insertBtn', 'Insert')),
                                                                                          shiny::column(6,actionButton('removeBtn', 'Remove'))
                                                                          ),
                                                                          shiny::fluidRow(shiny::column(6,tags$div(id = 'placeholder1')),
                                                                                          shiny::column(6,tags$div(id = 'placeholder2'))
                                                                          ),
                                                                          hr(),
                                                                          h4("Select an event of interest"),
                                                                          shiny::h5("Event Node:"),
                                                                          shiny::selectInput("event",
                                                                                             label = NULL,
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
                                                                 tabPanel("Association Graph",
                                                                          sliderInput("threshold", label = "Association Threshold",min = 0, max = 1,value = 0.25
                                                                          ),
                                                                          div(style = "position:absolute;right:1em;margin-right:10px;",
                                                                              bsButton('graphBtn2', '', icon = icon("refresh"),style = "default")
                                                                              #bsButton('secondSaveBtn', '', icon = icon("save"),style="default")

                                                                          ),

                                                                          bsPopover('graphBtn2', trigger = "hover", title = "Update", content = "Reloads the network graph", placement = "left", options = list(container = "body")),
                                                                          #bsPopover('secondSaveBtn', trigger = "hover", title = "Save",
                                                                          #content = "Saves graph to XYZ file. Go to Graph tab for more options",
                                                                          #placement = "bottom", options = list(container = "body")),
                                                                          br(),
                                                                          withSpinner(visNetworkOutput("assocPlot",height = "600px"), color= "#2E86C1")
                                                                 ),
                                                                 tabPanel("Network Graph",
                                                                          shiny::fluidRow(shiny::column(6,shiny::selectInput("moduleSelection",label = "Module","graph")),shiny::column(3,shiny::selectInput("neighbornodes",label = "Nth Neighbor List",choices = ""))),
                                                                          div(style = "position:absolute;right:1em;margin-right:10px;",
                                                                              bsButton('graphBtn', '', icon = icon("refresh"),style = "default")
                                                                              #bsButton('secondSaveBtn', '', icon = icon("save"),style="default")

                                                                          ),

                                                                          bsPopover('graphBtn', trigger = "hover", title = "Update", content = "Reloads the network graph", placement = "left", options = list(container = "body")),
                                                                          #bsPopover('secondSaveBtn', trigger = "hover", title = "Save",
                                                                                    #content = "Saves graph to XYZ file. Go to Graph tab for more options",
                                                                                    #placement = "bottom", options = list(container = "body")),
                                                                          br(),
                                                                          withSpinner(visNetworkOutput("netPlot",height = "600px"), color= "#2E86C1")
                                                                         ),
                                                                 tabPanel("Inference Plot",
                                                                          sliderInput("NumBar", label = "No. of bars",min = 0, max = 1,value = 1,step=1),
                                                                          actionButton("sortPlot","Sort X-axis"),
                                                                          withSpinner(plotOutput("distPlot",height = "600px")), color="#2E86C1"),
                                                                 tabPanel('Prevalence',
                                                                          selectInput("paramSelect",label = "Variable",""),
                                                                          withSpinner(plotOutput("parameterPlot",height = "600px")),color="#2E86C1"),
                                                                 tabPanel("Tables",
                                                                          shiny::fluidRow(shiny::column(4,selectInput("tableName",label = NULL,"")),shiny::column(1,downloadButton("downloadData", "Download"))),
                                                                          withSpinner(DT::dataTableOutput("tableOut")),color = "#2E86C1")
                                                                 )
                                                          )

                                                      )
                              ),
                              tabItem(tabName = "About",
                                      fluidRow(box(
                                        status = "primary",
                                        width = 12,
                                        div(style="text-align:center",
                                            h1('Creators')
                                        ),
                                        fluidRow(
                                          style = "margin-left:50px;padding:10px;",
                                          column(width=3, align = "center",
                                                 img(src = "shubham.jpg",style = "max-width: 50%; width: 50%; height: auto"),
                                                 h4('Shubham Maheshwari'),
                                                 h5('Data Scientist, Stockroom.io'),
                                                 h5('B.Tech Computer Science, IIIT-Delhi'),


                                                 fluidRow(width = 12,
                                                   column(width=3, a(img(src = "github.png", width = '30px', height = '30px'), href = "https://github.com/shubham14101"),target = "_blank"),
                                                   column(width=3, a(img(src = "facebook.png", style = "margin:5px; width: 20px; height: 20px"), href = "https://www.facebook.com/shubham.maheshwari3"),target = "_blank"),
                                                   column(width=3, a(img(src = "linkedin.png", style = "margin:5px; width: 20px; height: 20px"), href = "https://www.linkedin.com/in/shubham-maheshwari-93a35b108/"),target = "_blank"),
                                                   column(width=3, a(img(src = "twitter.png", style = "margin:6px; width: 18px; height: 18px"), href = "https://twitter.com/real_SM96"),target = "_blank")
                                                 )),
                                          column(width=1, align = "center", img(src = "vertical-line.png",style = "max-width: 100%; width: 100%; height: 100%;")),
                                          column(width=3, align = "center",
                                                 img(src = "anant.jpg", style = "max-width: 50%; width: 50%; height: auto"),
                                                 h4('Anant Mittal'),
                                                 h5('Data Scientist, Egregore Labs'),
                                                 h5('B.Tech Computer Science, IIIT-Delhi'),


                                                 fluidRow(width = 12,
                                                          column(width=3, a(img(src = "github.png", width = '30px', height = '30px'), href = "https://github.com/anant15"), target = "_blank"),
                                                          column(width=3, a(img(src = "facebook.png", style = "margin:5px; width: 20px; height: 20px"), href = "https://www.facebook.com/shubham.maheshwari3"), target = "_blank"),
                                                          column(width=3, a(img(src = "linkedin.png", style = "margin:5px; width: 20px; height: 20px"), href = "https://www.linkedin.com/in/shubham-maheshwari-93a35b108/"), target = "_blank"),
                                                          column(width=3, a(img(src = "twitter.png", style = "margin:6px; width: 18px; height: 18px"), href = "https://twitter.com/real_SM96"),target = "_blank")
                                                 )),
                                          column(width=1, align = "center", img(src = "vertical-line.png",style = "max-width: 100%; width: 100%; height: auto;")),
                                          column(width=3, align = "center",
                                                 img(src = "tps.jpg", style = "max-width: 50%; width: 50%; height: auto;"),
                                                 h4('Tavpritesh Sethi'),
                                                 h5('Assistant Professor, IIIT-Delhi'),
                                                 h5('Visiting Assistant Professor, Stanford Medicine'),

                                                 fluidRow(width = 12,
                                                          column(width=3, a(img(src = "github.png", width = '30px', height = '30px'), href = "https://github.com/SAFE-ICU?tab=repositories"), target = "_blank"),
                                                          column(width=3, a(img(src = "facebook.png", style = "margin:5px; width: 20px; height: 20px"), href = "https://www.facebook.com/tavpritesh.sethi"), target = "_blank"),
                                                          column(width=3, a(img(src = "linkedin.png", style = "margin:5px; width: 20px; height: 20px"), href = "https://in.linkedin.com/in/tavpritesh"), target = "_blank"),
                                                          column(width=3, a(img(src = "twitter.png", style = "margin:6px; width: 18px; height: 18px"), href = "https://twitter.com/tavpritesh"), target = "_blank")
                                                 ))
                                        ),
                                        hr(),
                                        div(style="text-align:center",
                                            h4("ShinyBN: Democratizing Bayesian Network Analysis in Complex Multivariate Data (submitted)"),
                                            hr(),
                                            h4("Correspondence: tavpriteshsethi@iiitd.ac.in"),
                                            hr(),
                                            h4("Acknowledgements - We acknowledge the useful inputs provided
                                               by Prof. Rakesh Lodha, Professor, All India Institute of Medical Sciences, New Delhi, India")
                                        )


                                      )

                                      )
                                    )
),
tags$footer("Funding Support: The Wellcome Trust/DBT India Alliance grant IA/CPHE/14/1/501504 to Tavpritesh Sethi", align = "center", style = "
position:absolute;
            bottom:0;
            width:100%;
            height:30px;
            padding:5px;
            background-color: white;z-index:1200;")
)


)
