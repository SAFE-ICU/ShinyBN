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


dashboardPage(skin = "blue",
              dashboardHeader(title = "Bayesian Networks",
                              titleWidth = 240
              ),
              dashboardSidebar(width = 240,
                               sidebarMenu(id = "sidebarMenu",
                                           collapsed = T,
                                           menuItem("Home",
                                                    tabName = "home",
                                                    icon = icon("home")
                                           ),
                                           menuItem("Structure & Inference",
                                                    icon = shiny::icon("globe"),
                                                    tabName = "structure"
                                           )
                               )
              ),
              dashboardBody(id ="dashboardBody",
                            useShinyalert(),
                            shinydashboard::tabItems(
                              shinydashboard::tabItem(tabName = "home",
                                                      fluidRow(box(title = "About the App",
                                                                   status = "primary",
                                                                   width = 12,
                                                                   shiny::h4("Note*: The Demo version of the app is running for the Iris data set freely available in R"),
                                                                   shiny::h1("Features"),
                                                                   shiny::h3("Structure Part"),
                                                                   shiny::h5("This is used for bayesian structure learning."),
                                                                   shiny::h5("The user is provided with 2 options either to upload discrete data and the learned bayesian object which can then be used to
                                                                             visualize the directed network graph and perform multiple chained inferences or to upload discrete data and simply choose from the paramters for structure learning
                                                                             ,the app will then run the stucture learning algorithm with the chooses paramters after which graph visualization and infrences can be performed."),
                                                                   shiny::h5("Structure learning part provides a variety of features like:-"),
                                                                   shiny::h5("1) Prefered structure learning algorithm from the list of following algorithms:-Hill Climbing,Tabu,Grow-Shrink,Incremental Association,Fast IAMB,Inter IAMB,Max-Min Hill Climbing,2-phase Restricted Maximization,Max-Min Parents and Children,Semi-Interleaved HITON-PC,ARACNE,Chow-Liu"),
                                                                   shiny::h5("2) User can select the number of bootstrap iteration to run between the range of 1-1000"),
                                                                   shiny::h5("3) User can select the sample size from the data to be used for bootstrap"),
                                                                   shiny::h5("4) User can choose to prune the network based on edge strength and direction confidence in the results of bootstap structure learning."),
                                                                   shiny::h5("5) User can visualize the learned directed graph on the right side of the screen"),
                                                                   shiny::h4("Note*: Due to size restrictions on data upload and the nature of the input it is required of the user to upload the directed data and/or the bnlearn object both as independent individual .Rdata files. This enables the user to be able to utilize the app even for large data with multiple thousands variables/samples."),
                                                                   shiny::h3("Inference Part"),
                                                                   shiny::h5("The app is capable of performing multiple chained inference.Platform is provided for user to not only
                                                                             visualize the inference plot but also highlight the evidence and event nodes in the graph. The following are the use cases oh the inference tab:-"),
                                                                   shiny::h5("1) Insert and Remove buttons are used to add/removes nodes and their values as evidencde for inference"),
                                                                   shiny::h5("2) The event box can be used to set the appropriate node as event"),
                                                                   shiny::h5("3) The update plot button is used to get the updated plot of inference based on currently set evidence and event."),
                                                                   shiny::h5("4) The update graph button is used to update the network graph to visualizd the currently set evidence and event nodes where event is highlighted in green and evidence in red."),
                                                                   shiny::h4("Note*: a single point of change is selected for updating the plot and the graph is because with large data with several hundred nodes dynamic changes is laggy and hence runs into invalid operating while fetching inferences/updating graphs."),
                                                                   shiny::h3("Additional Features"),
                                                                   shiny::h5("The user can user the option simple learn on the prefered algorithm to do a simple bayesian learning instead of a bootstraped one"),
                                                                   shiny::h5("User has the option to save the learned structure object by adding the exact path with file name and .RData extension in the structure saving field provided under the save structure button."),
                                                                   shiny::h5("User has the option to save the learned directed graph by adding the exact path with file name and .csv extension in the graph saving field provided under the save graph button."),
                                                                   shiny::h5("User has the option to bulid a confidence plot for the aprroximate inference using the confidence plot button, which enables the user to better understand the approxiamte inference which generally varies due to randomness in sample selection."),
                                                                   shiny::h4("Note*: The app is an open source project created to help people perfom and visualize structure learning on their own dataset for research and study. In no reason the app is meant for or to be used for commercial purposes. By agreeing to install the package or using the App you agree to these terms.If you found the app usefull please suggest it to other people who may find it of use. The app is still in Beta-phase and any and all suggestion for improvements are welcome."),
                                                                   shiny::h3("Contributers"),
                                                                   shiny::h5("Shubham Maheshwari (IIITD)"),
                                                                   shiny::h5("Anant Mittal (IIITD)"),
                                                                   shiny::h5("Dr.Tavpritesh Sethi (Stanford/AIIMS/IIITD)")
                                                                   )
                                                               )
                                                      ),
                              shinydashboard::tabItem(tabName = "structure",
                                                      shiny::fluidRow(
                                                        shiny::column(
                                                          width = 3,
                                                          tabBox(width=12,id="control_tabs",
                                                                 tabPanel("Data",
                                                                          shiny::helpText("Select prefered input format(RData suggested for data > 100mb)"),
                                                                          shiny::selectInput('format','Data Format',c(".RData",".CSV")),
                                                                          shiny::helpText("Upload your data:"),
                                                                          shiny::fileInput('dataFile',
                                                                                           strong('File Input:'),
                                                                                           accept = c('.RData','.csv')
                                                                                           )
                                                                          ),
                                                                 tabPanel("EDA",
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
                                                          tabBox(id = "visula_tabs",
                                                                 width = 12,
                                                                 tabPanel("Network Graph",
                                                                          withSpinner(visNetworkOutput("netPlot",height = "600px")), color= "#f4b943"),
                                                                 tabPanel("Inference Plot",
                                                                          withSpinner(plotOutput("distPlot",height = "600px")), color="#ff69b4")
                                                                 )
                                                          )

                                                      )
                              )
)
)
)
