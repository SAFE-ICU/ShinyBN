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

shinyServer(function(input, output,session) {
  withProgress(message = "Initializing Dashboard", value = 0, {
  #Data upload limit
  options(shiny.maxRequestSize=1500*1024^2)
  #Structure Initialization
  D <- get(load('a.RData'))
  DiscreteData <- D
  trueData<<-DiscreteData
  bn.hc.boot <- boot.strength(data = DiscreteData,R = 5,m =ceiling(nrow(DiscreteData)*0.7) ,algorithm = "hc")
  bn.hc.boot.pruned <- bn.hc.boot[bn.hc.boot$strength > 0.5 & bn.hc.boot$direction >0.5,]
  bn.hc.boot.average <- cextend(averaged.network(bn.hc.boot.pruned))
  bn.hc.boot.fit <- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = 'bayes')
  #Graph Initialization
  NetworkGraph <- data.frame(directed.arcs(bn.hc.boot.average))
  nodeNames <- names(bn.hc.boot.average$nodes)
  shapeVector<- rep('dot',length(nodeNames))
  EventNode <- nodeNames[1]
  EvidenceNode <- c()
  output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,2,'layout_nicely')})
  #Module Initilization
  communities<-custom.Modules(NetworkGraph,bn.hc.boot.pruned[,3])
  names(communities)<-paste("Module",c(1:length(communities)),sep=" ")
  updateSelectInput(session,"moduleSelection",choices = c("graph",names(communities)))
  #App Initialization
  inserted  <- c()
  insertedV <- c()
  rvs <<- reactiveValues(evidence = list(),values = list(),evidenceObserve = list(),valueObserve = list())
  updateSelectInput(session,'event',choices = nodeNames)
  updateSelectizeInput(session,'varselect',choices = nodeNames)
  updateSelectInput(session,'paramSelect',choices = nodeNames)
  updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star","ellipse", "database", "text", "diamond"))
  updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star","ellipse", "database", "text", "diamond"))
  updateSelectInput(session,'graph_layout',choices = c("layout_nicely","layout_as_star","layout_as_tree","layout_in_circle","layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
  output$distPlot<- renderPlot(validate("Built infrence plot will be displayed"))
  #Sanity check
  sanity<-1
  confidence<-1
  check<-1
  #Association Network
  assocNetwork<-custom.association(DiscreteData,"cramer's V")
  assocNetworkprune<- assocNetwork[which(assocNetwork[,3]>0.1),]
  shapeVectorAssoc<- rep('dot',length(unique(c(assocNetworkprune[,1],assocNetworkprune[,2]))))
  output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),EvidenceNode,EventNode,2,'layout_nicely',shapeVectorAssoc)})
  #Tables
  updateSelectInput(session,"tableName",choices = c("Data","Association Graph","Bayesian Graph","Cross Validation Results","blacklist edges","whitelist edges"))
  output$tableOut<- DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10))
  #Validation
  bn.validate<-bn.cv(DiscreteData[,nodeNames],bn=bn.hc.boot.average)
  predError<-c()
  for(n in nodeNames)
  {
    targetLoss<-bn.cv(DiscreteData[,nodeNames],bn=bn.hc.boot.average,loss = "pred",loss.args = list(target = n))
    predError<-rbind(predError,targetLoss[[1]]$loss)
  }
  rownames(predError)<-nodeNames
  colnames(predError)<-"Classification Error"
  output$valLoss<-renderText({bn.validate[[1]]$loss})
  #blacklist/whitelist
  blacklistEdges<-c()
  whitelistEdges<-c()
  #Nth degree neighbors
  graph<-graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
  #Inference tab
  updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNames[1]]),value = nlevels(DiscreteData[,nodeNames[1]]))
  })
  #observe events
  observeEvent(input$tableName,{
    tryCatch({
      if(input$tableName=="Data")
      {
        output$tableOut<- DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10),selection = list(target = 'column'))
      }
      else if(input$tableName == "Association Graph")
      {
        output$tableOut<- DT::renderDataTable({assocNetwork},options = list(scrollX = TRUE,pageLength = 10))
      }
      else if(input$tableName=="Bayesian Graph")
      {
        output$tableOut<- DT::renderDataTable({NetworkGraph},options = list(scrollX = TRUE,pageLength = 10))
      }
      else if(input$tableName=="Cross Validation Results")
      {
        output$tableOut<- DT::renderDataTable({predError},options = list(scrollX = TRUE,pageLength = 10))
      }
      else if(input$tableName=="blacklist edges")
      {
        output$tableOut<- DT::renderDataTable({blacklistEdges},options = list(scrollX = TRUE,pageLength = 10))
      }
      else if(input$tableName=="whitelist edges")
      {
        output$tableOut<- DT::renderDataTable({whitelistEdges},options = list(scrollX = TRUE,pageLength = 10))
      }
    },error=function(e){
      shinyalert(toString(e), type = "error")
    })

  })
  observeEvent(input$listFile,{
    tryCatch({
      file=input$listFile
      if(input$listType=="Blacklist")
      {
        blacklistEdges=read.csv(file$datapath,stringsAsFactors = T,na.strings = c("NA","na","Na","nA","","?","-"))
        if(dim(blacklistEdges)[2]!=2)
        {
          blacklistEdges<<-c()
          shinyalert("Please upload a .csv file containg edges in format 'from' and 'to'",type="error")
        }
        else if(!(unique(blacklistEdges[,1],blacklistEdges[,2]) %in% colnames(DiscreteData)))
        {
          blacklistEdges<<-c()
          shinyalert("please upload a correct file containg only nodes as observed in the data",type="error")
        }
      }
      else
      {
        whitelistEdges=read.csv(file$datapath,stringsAsFactors = T,na.strings = c("NA","na","Na","nA","","?","-"))
        if(dim(whitelistEdges)[2]!=2)
        {
          whitelistEdges<<-c()
          shinyalert("Please upload a .csv file containg edges in format 'from' and 'to'",type="error")
        }
        else if(!(unique(blacklistEdges[,1],blacklistEdges[,2]) %in% colnames(DiscreteData)))
        {
          whitelistEdges<<-c()
          shinyalert("please upload a correct file containg only nodes as observed in the data",type="error")
        }
      }
    },error=function(e){
      shinyalert(toString(e), type = "error")
    })
  })
  observeEvent(input$start,{
      updateTabItems(session, "sidebarMenu", "Structure")
    })
  observeEvent(input$threshold,{
    tryCatch({
      assocNetworkprune<<- assocNetwork[which(assocNetwork[,3]>input$threshold),]
      shapeVectorAssoc<<- rep('dot',length(unique(c(assocNetworkprune[,1],assocNetworkprune[,2]))))
      output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),EvidenceNode,EventNode,input$degree,input$graph_layout,shapeVectorAssoc)})
    },error=function(e){
      shinyalert(toString(e), type = "error")
    })
  })
  observeEvent(input$association,{
    tryCatch({
      assocNetwork<<-custom.association(DiscreteData,input$assocType)
      assocNetworkprune<<- assocNetwork[which(assocNetwork[,3]>input$threshold),]
      shapeVectorAssoc<<- rep('dot',length(unique(c(assocNetworkprune[,1],assocNetworkprune[,2]))))
      output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),EvidenceNode,EventNode,input$degree,input$graph_layout,shapeVectorAssoc)})
    },error=function(e){
      shinyalert(toString(e), type = "error")
    })

  })
  observeEvent(input$calLoss,{
    tryCatch({
      withProgress(message = "Validating Model", value = 0, {
        if(input$parallel==T)
        {
          bn.validate<-bn.cv(DiscreteData[,nodeNames],bn=bn.hc.boot.average,fit = input$paramMethod3,method = input$crossFunc,cluster = cl)
          predError<<-c()
          for(n in nodeNames)
          {
            targetLoss<<-bn.cv(DiscreteData[,nodeNames],bn=bn.hc.boot.average,fit = input$paramMethod3,loss = input$lossFunc,method = input$crossFunc,loss.args = list(target = n),cluster = cl)
            predError<<-rbind(predError,targetLoss[[1]]$loss)
          }
          rownames(predError)<<-nodeNames
          colnames(predError)<<-"Classification Error"
          output$valLoss<<-renderText({bn.validate[[1]]$loss})
        }
        else
        {
          bn.validate<-bn.cv(DiscreteData[,nodeNames],bn=bn.hc.boot.average,fit = input$paramMethod3,method = input$crossFunc)
          predError<<-c()
          for(n in nodeNames)
          {
            targetLoss<<-bn.cv(DiscreteData[,nodeNames],bn=bn.hc.boot.average,fit = input$paramMethod3,loss = input$lossFunc,method = input$crossFunc,loss.args = list(target = n))
            predError<<-rbind(predError,targetLoss[[1]]$loss)
          }
          rownames(predError)<<-nodeNames
          colnames(predError)<<-"Classification Error"
          output$valLoss<<-renderText({bn.validate[[1]]$loss})
        }
        })
    },error=function(e){
      shinyalert(toString(e), type = "error")
    })

  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$tableName, ".csv", sep = "")
    },
    content = function(file) {
      if(input$tableName=="Data")
      {
        write.csv(DiscreteData,file,row.names = F)
      }
      else if(input$tableName == "Association Graph")
      {
        write.csv(assocNetwork, file,row.names = F)
      }
      else if(input$tableName=="Bayesian Graph")
      {
        write.csv(NetworkGraph, file,row.names = F)
      }
      else if(input$tableName=="Cross Validation Results")
      {
        write.csv(predError, file,row.names=F)
      }
      else if(input$tableName=="blacklist edges")
      {
        write.csv(blacklistC, file,row.names = F)
      }
      else if(input$tableName=="whitelist edges")
      {
        write.csv(whitelistC, file,row.names=F)
      }
    }
  )
  output$saveBtn<-downloadHandler(
    filename = function() {
      paste('structure', ".RData", sep = "")
    },
    content = function(file) {
      save(bn.hc.boot.average,file)
    }
  )
  #Data Frame From User
  observeEvent(input$dataFile,{
    inFile <- input$dataFile
    if (is.null(inFile))
    {
      shinyalert("Data file is empty, pls upload a valid datafile",type = "error")
    }
    else
    {
      tryCatch({
        if(input$format==".RData")
        {
          if(file_ext(inFile$datapath) == "RData")
          {
            tryCatch({
              DiscreteData <<- get(load(inFile$datapath))
              trueData<<-DiscreteData
              },error = function(e){
                DiscreteData<<- readRDS(inFile$datapath)
                trueData<<-DiscreteData
              })
          }
          else
          {
            shinyalert("Added file is not a .RData file.Please upload a RData file.", type = "error")
          }

        }
        else
        {
          if(file_ext(inFile$datapath) == "csv")
          {
            DiscreteData <<- read.csv(inFile$datapath,stringsAsFactors = T,na.strings = c("NA","na","Na","nA","","?","-"))
            trueData<<-DiscreteData
          }
          else
          {
            shinyalert("Added file is not a .csv file.Please upload a CSV file.", type = "error")
          }
        }
        check.discrete(DiscreteData)
        check.NA(DiscreteData)
        DiscreteData<<-as.data.frame(DiscreteData)
        #Reset APP
        blacklistEdges<<-c()
        whitelistEdges<<-c()
        },error = function(e){
             shinyalert(c("Error in loading data: ",toString(e)), type = "error")
           })
      }
    })
  observeEvent(input$discretize,{
    tryCatch({
      check.NA(DiscreteData)
      withProgress(message = "Discretizing data", value = 0, {
        tempDiscreteData <- DiscreteData
        for(n in colnames(tempDiscreteData))
        {
          if(is.numeric(tempDiscreteData[,n])|| is.integer(tempDiscreteData[,n]))
          {
            temp = custom.discretize(as.numeric(tempDiscreteData[,n]),input$dtype)
            tempDiscreteData[,n]<-temp
          }
        }
        tempDiscreteData[,which(lapply(tempDiscreteData,nlevels)<2)] = NULL
        tempDiscreteData <- droplevels(tempDiscreteData)
        DiscreteData <<-tempDiscreteData
        trueData<<-DiscreteData
      })},error = function(e){
        type <- toString(input$dtype)
        messageString <- paste(c("Error is discretising using method ", type, ". Try using other method or upload pre-discretised data."), collapse = '')
        shinyalert(messageString, type = "error")
      })

  })

  observeEvent(input$impute,{
    tryCatch({
      withProgress(message = "Imputing missing data", value = 0, {
      for(n in colnames(DiscreteData))
      {
        if(is.character(DiscreteData[,n]))
        {
          DiscreteData[,n]<<-as.factor(DiscreteData[,n])
        }
      }
      DiscreteData <<- missRanger(DiscreteData,maxiter = 2,num.tree = 100)
      trueData<<-DiscreteData
      check.discrete(DiscreteData)
      check.NA(DiscreteData)
    })}, error = function(e){
      type <- toString(input$dtype)
      messageString <- "Error imputing missingness using missRanger method. Try uploading pre-imputed data."
      shinyalert(messageString, type = "error")
    })

  })


  # Get the data selection from user
  observeEvent(input$structFile,{# Get the uploaded file from user
       if(sum(is.na(DiscreteData))>0)
       {
          shinyalert("Please impute missingness in the data first",type="info")
       }
       else if(sum(lapply(DiscreteData,is.numeric))>0)
       {
         shinyalert("Please discritize the data first",type="info")
       }
       else
       {
         inFile <- input$structFile
         if (is.null(inFile))
         {
           shinyalert("Structure File is empty",type='error')
         }
         else
         {
           if(is.null(DiscreteData))
           {
             shinyalert("Please Upload Data File First",type = 'error')
           }
           else
           {
             tryCatch({
               tryCatch({
                 bn.hc.boot.average <<- get(load(inFile$datapath))
               },error = function(e){
                 bn.hc.boot.average <<- readRDS(inFile$datapath)
               })
               if(input$parallel==T)
               {
                 bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod,cluster = cl)
               }
               else
               {
                 bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod)
               }
               shinyalert("Learned Structure loaded",type = "success")
               for(elem in 1:length(inserted))
               {
                 removeUI(
                   ## pass in appropriate div id
                   selector = paste0('#', inserted[elem])
                 )

               }
               inserted <<- c()
               for(elem2 in 1:length(insertedV))
               {
                 removeUI(
                   ## pass in appropriate div id
                   selector = paste0('#', insertedV[elem2])
                 )

               }
               insertedV <<- c()
               rvs$evidence <<- c()
               rvs$value <<- c()
               rvs$evidenceObserve <<- c()
               rvs$valueObserve <<- c()
               output$distPlot <<- renderPlot(NULL)
               NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
               nodeNames <<- names(bn.hc.boot.average$nodes)
               EventNode <<- nodeNames[1]
               EvidenceNode <<- c()
               shapeVector<<- rep('dot',length(nodeNames))
               updateSelectInput(session,'event',choices = nodeNames)
               output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout)})
               updateSelectizeInput(session,'varselect',choices = nodeNames)
               updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                 "ellipse", "database", "text", "diamond"))
               updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                                  "ellipse", "database", "text", "diamond"))
               updateSelectInput(session,'graph_layout',choices = c("layout_nicely","layout_as_star","layout_as_tree","layout_in_circle","layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
               updateSelectInput(session,'paramSelect',choices = nodeNames)
               communities<<-custom.Modules(NetworkGraph,rep(1,length(NetworkGraph[,1])))
               names(communities)<<-paste("Module",c(1:length(communities)),sep=" ")
               updateSelectInput(session,"moduleSelection",choices = c("graph",names(communities)))
               graph<<-graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
               updateSelectInput(session,"neighbornodes",choices = "")
               updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNames[1]]),value = nlevels(DiscreteData[,nodeNames[1]]))
             },error = function(e){
               shinyalert(toString(e), type = "error")
             })
           }
         }
       }
    })



  # Learn the structure of the network
  observeEvent(input$learnBtn, {
    if(sum(is.na(DiscreteData))>0)
    {
      shinyalert("Please impute missingness in the data first",type="info")
    }
    else if(sum(lapply(DiscreteData,is.numeric))>0)
    {
      shinyalert("Please discritize the data first",type="info")
    }
    else
    {
      tryCatch({
        shinyalert("Structure Learning started",type="info")
        if (is.null(DiscreteData))
          return(NULL)

        # Create a Progress object
        progress <- shiny::Progress$new()

        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        progress$set(message = "Learning network structure", value = 0)

        # Get the selected learning algorithm from the user and learn the network
        if(input$parallel==T)
        {
          bn.hc.boot <<- boot.strength(data = DiscreteData, R = input$boot, m = ceiling(nrow(DiscreteData)*input$SampleSize), algorithm = input$alg,algorithm.args=list(blacklist=blacklistEdges,whitelist=whitelistEdges),cluster = cl)
          bn.hc.boot.pruned <<- bn.hc.boot[bn.hc.boot$strength > input$edgeStrength & bn.hc.boot$direction > input$directionStrength,]
          bn.hc.boot.average <<- cextend(averaged.network(bn.hc.boot.pruned))
          bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2,cluster = cl)
        }
        else
        {
          bn.hc.boot <<- boot.strength(data = DiscreteData, R = input$boot, m = ceiling(nrow(DiscreteData)*input$SampleSize), algorithm = input$alg,algorithm.args=list(blacklist=blacklistEdges,whitelist=whitelistEdges))
          bn.hc.boot.pruned <<- bn.hc.boot[bn.hc.boot$strength > input$edgeStrength & bn.hc.boot$direction > input$directionStrength,]
          bn.hc.boot.average <<- cextend(averaged.network(bn.hc.boot.pruned))
          bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2)
        }
        shinyalert("Structure learning done",type="success")
        for(elem in 1:length(inserted))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', inserted[elem])
          )

        }
        inserted <<- c()
        for(elem2 in 1:length(insertedV))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', insertedV[elem2])
          )

        }
        insertedV <<- c()
        rvs$evidence <<- c()
        rvs$value <<- c()
        rvs$evidenceObserve <<- c()
        rvs$valueObserve <<- c()
        output$distPlot <<- renderPlot(NULL)
        NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
        nodeNames <<- names(bn.hc.boot.average$nodes)
        EventNode <<- nodeNames[1]
        EvidenceNode <<- c()
        shapeVector<<- rep('dot',length(nodeNames))
        updateSelectInput(session,'event',choices = nodeNames)
        output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout)})
        updateSelectInput(session,'event',choices = nodeNames)
        updateSelectizeInput(session,'varselect',choices = nodeNames)
        updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                          "ellipse", "database", "text", "diamond"))
        updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                           "ellipse", "database", "text", "diamond"))
        updateSelectInput(session,'graph_layout',choices = c("layout_nicely","layout_as_star","layout_as_tree","layout_in_circle","layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
        updateSelectInput(session,'paramSelect',choices = nodeNames)
        communities<<-custom.Modules(NetworkGraph,bn.hc.boot.pruned[,3])
        names(communities)<<-paste("Module",c(1:length(communities)),sep=" ")
        updateSelectInput(session,"moduleSelection",choices = c("graph",names(communities)))
        graph<<-graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
        updateSelectInput(session,"neighbornodes",choices = "")
        updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNames[1]]),value = nlevels(DiscreteData[,nodeNames[1]]))
      },error = function(e){
        shinyalert(toString(e), type = "error")
      })
    }
  })
  observeEvent(input$learnSBtn, {
    if(sum(is.na(DiscreteData))>0)
    {
      shinyalert("Please impute missingness in the data first",type="info")
    }
    else if(sum(lapply(DiscreteData,is.numeric))>0)
    {
      shinyalert("Please discritize the data first",type="info")
    }
    else
    {
      tryCatch({
        shinyalert("Structure Learning started",type="info")
        if (is.null(DiscreteData))
          return(NULL)

        # Create a Progress object
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        progress$set(message = "Learning network structure", value = 0)

        # Get the selected learning algorithm from the user and learn the network
        if(input$parallel==T)
        {
          if(input$alg == 'hc')
          {
            bn.hc.boot.average <<- cextend(bnlearn::hc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster = cl))
          }
          else if(input$alg =="pc.stable")
          {
            bn.hc.boot.average <<- cextend(bnlearn::pc.stable(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster = cl))
          }
          else if(input$alg == 'tabu')
          {
            bn.hc.boot.average <<- cextend(bnlearn::tabu(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster = cl))
          }
          else if(input$alg == 'gs')
          {
            bn.hc.boot.average <<- bnlearn::gs(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster=cl)
          }
          else if(input$alg == 'iamb')
          {
            bn.hc.boot.average <<- cextend(bnlearn::iamb(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster=cl))
          }
          else if(input$alg == 'fast.iamb')
          {
            bn.hc.boot.average <<- cextend(bnlearn::fast.iamb(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster=cl))
          }
          else if(input$alg=='inter.iamb')
          {
            bn.hc.boot.average <<- cextend(bnlearn::inter.iamb(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster=cl))
          }
          else if(input$alg == 'mmhc')
          {
            bn.hc.boot.average <<- cextend(bnlearn::mmhc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster=cl))
          }
          else if(input$alg == 'rsmax2')
          {
            bn.hc.boot.average <<- cextend(bnlearn::rsmax2(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster = cl))
          }
          else if(input$alg == 'mmpc')
          {
            bn.hc.boot.average <<- cextend(bnlearn::mmpc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster = cl))
          }
          else if(input$alg == 'si.hiton.pc')
          {
            bn.hc.boot.average <<- cextend(bnlearn::si.hiton.pc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster = cl))
          }
          else if(input$alg == 'aracne')
          {
            bn.hc.boot.average <<- cextend(bnlearn::aracne(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster = cl))
          }
          else
          {
            bn.hc.boot.average <<- cextend(bnlearn::chow.liu(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges,cluster = cl))
          }
          #bn.hc.boot.average <<- bnlearn::hc(DiscreteData)
          bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2,cluster=cl)
        }
        else
        {
          if(input$alg == 'hc')
          {
            bn.hc.boot.average <<- cextend(bnlearn::hc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
          }
          else if(input$alg =="pc.stable")
          {
            bn.hc.boot.average <<- cextend(bnlearn::pc.stable(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
          }
          else if(input$alg == 'tabu')
          {
            bn.hc.boot.average <<- cextend(bnlearn::tabu(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
          }
          else if(input$alg == 'gs')
          {
            bn.hc.boot.average <<- bnlearn::gs(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges)
          }
          else if(input$alg == 'iamb')
          {
            bn.hc.boot.average <<- cextend(bnlearn::iamb(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
          }
          else if(input$alg == 'fast.iamb')
          {
            bn.hc.boot.average <<- cextend(bnlearn::fast.iamb(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
          }
          else if(input$alg=='inter.iamb')
          {
            bn.hc.boot.average <<- cextend(bnlearn::inter.iamb(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
          }
          else if(input$alg == 'mmhc')
          {
            bn.hc.boot.average <<- cextend(bnlearn::mmhc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
          }
          else if(input$alg == 'rsmax2')
          {
            bn.hc.boot.average <<- cextend(bnlearn::rsmax2(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
          }
          else if(input$alg == 'mmpc')
          {
            bn.hc.boot.average <<- cextend(bnlearn::mmpc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
          }
          else if(input$alg == 'si.hiton.pc')
          {
            bn.hc.boot.average <<- cextend(bnlearn::si.hiton.pc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
          }
          else if(input$alg == 'aracne')
          {
            bn.hc.boot.average <<- cextend(bnlearn::aracne(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
          }
          else
          {
            bn.hc.boot.average <<- cextend(bnlearn::chow.liu(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
          }
          #bn.hc.boot.average <<- bnlearn::hc(DiscreteData)
          bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2)
        }
        if(input$alg == 'hc')
        {
          bn.hc.boot.average <<- cextend(bnlearn::hc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
        }
        else if(input$alg =="pc.stable")
        {
          bn.hc.boot.average <<- cextend(bnlearn::pc.stable(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
        }
        else if(input$alg == 'tabu')
        {
          bn.hc.boot.average <<- cextend(bnlearn::tabu(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
        }
        else if(input$alg == 'gs')
        {
          bn.hc.boot.average <<- bnlearn::gs(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges)
        }
        else if(input$alg == 'iamb')
        {
          bn.hc.boot.average <<- cextend(bnlearn::iamb(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
        }
        else if(input$alg == 'fast.iamb')
        {
          bn.hc.boot.average <<- cextend(bnlearn::fast.iamb(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
        }
        else if(input$alg=='inter.iamb')
        {
          bn.hc.boot.average <<- cextend(bnlearn::inter.iamb(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
        }
        else if(input$alg == 'mmhc')
        {
          bn.hc.boot.average <<- cextend(bnlearn::mmhc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
        }
        else if(input$alg == 'rsmax2')
        {
          bn.hc.boot.average <<- cextend(bnlearn::rsmax2(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
        }
        else if(input$alg == 'mmpc')
        {
          bn.hc.boot.average <<- cextend(bnlearn::mmpc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
        }
        else if(input$alg == 'si.hiton.pc')
        {
          bn.hc.boot.average <<- cextend(bnlearn::si.hiton.pc(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
        }
        else if(input$alg == 'aracne')
        {
          bn.hc.boot.average <<- cextend(bnlearn::aracne(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
        }
        else
        {
          bn.hc.boot.average <<- cextend(bnlearn::chow.liu(DiscreteData,blacklist=blacklistEdges,whitelist=whitelistEdges))
        }
        #bn.hc.boot.average <<- bnlearn::hc(DiscreteData)
        bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2)
        shinyalert("Structure learning done",type="success")
        for(elem in 1:length(inserted))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', inserted[elem])
          )

        }
        inserted <<- c()
        for(elem2 in 1:length(insertedV))
        {
          removeUI(
            ## pass in appropriate div id
            selector = paste0('#', insertedV[elem2])
          )

        }
        insertedV <<- c()
        rvs$evidence <<- c()
        rvs$value <<- c()
        rvs$evidenceObserve <<- c()
        rvs$valueObserve <<- c()
        output$distPlot <<- renderPlot(NULL)
        NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
        nodeNames <<- names(bn.hc.boot.average$nodes)
        EventNode <<- nodeNames[1]
        EvidenceNode <<- c()
        shapeVector<<- rep('dot',length(nodeNames))
        updateSelectInput(session,'event',choices = nodeNames)
        output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout)})
        updateSelectInput(session,'event',choices = nodeNames)
        updateSelectizeInput(session,'varselect',choices = nodeNames)
        updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                          "ellipse", "database", "text", "diamond"))
        updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                           "ellipse", "database", "text", "diamond"))
        updateSelectInput(session,'graph_layout',choices = c("layout_nicely","layout_as_star","layout_as_tree","layout_in_circle","layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
        updateSelectInput(session,'paramSelect',choices = nodeNames)
        communities<<-custom.Modules(NetworkGraph,rep(1,length(NetworkGraph[,1])))
        names(communities)<<-paste("Module",c(1:length(communities)),sep=" ")
        updateSelectInput(session,"moduleSelection",choices = c("graph",names(communities)))
        graph<<-graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
        updateSelectInput(session,"neighbornodes",choices = "")
        updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNames[1]]),value = nlevels(DiscreteData[,nodeNames[1]]))
      },error = function(e){
        shinyalert(toString(e), type = "error")
      })
    }
  })
  observeEvent(input$paramSelect,{
    tryCatch({
      output$parameterPlot<-renderPlot({bn.fit.barchart(bn.hc.boot.fit[[input$paramSelect]])})
    },error = function(e){
      shinyalert(toString(e), type = "error")
    })

  })
  observeEvent(input$parallel,{
    tryCatch({
      if(input$parallel==TRUE)
      {
        check<<-2
        cl <<- makeCluster(strtoi(input$clusters), type = "SOCK")
        shinyalert("Parallel clusters successfully created",type="success")
      }
      else
      {
        if(check==2)
        {
          stopCluster(cl)
          ckeck<<-1
        }
      }
    },error=function(e){
      shinyalert(toString(e), type = "error")
    })
  })
  observeEvent(input$insertBtn, {
    tryCatch({
      btn <<- input$insertBtn
      id <- paste0('Evidence', btn)
      idL <- paste("Evidence", btn)
      idV <- paste0('Value', btn)
      idVL <- paste("Value", btn)
      insertUI(selector = '#placeholder1',
               ui = tags$div(selectInput(id,'Evidence',nodeNames),
                             id = id
               )
      )
      insertUI(selector = '#placeholder2',
               ui = tags$div(selectInput(idV,'Value',levels(DiscreteData[,nodeNames[1]])),
                             id = idV
               )
      )
      inserted <<- c(id, inserted)
      insertedV <<- c(idV,insertedV)
      rvs$evidence <<- c(rvs$evidence,id)
      rvs$value <<- c(rvs$value,id)
      rvs$evidenceObserve <<- c(rvs$evidenceObserve,observeEvent(input[[id]],{
        tryCatch({
          valID = insertedV[which(inserted == id)]
          updateSelectInput(session,valID, choices = levels(DiscreteData[,input[[id]]]))
        },error = function(e){
          shinyalert(toString("Please learn structure or upload structure on new data uploaded to make infrences"), type = "error")
        })
      }))

    },error = function(e){
      shinyalert(toString(e), type = "error")
    })
  })

  observeEvent(input$removeBtn, {
    tryCatch({
      removeUI(
        ## pass in appropriate div id
        selector = paste0('#', inserted[length(inserted)])
      )
      inserted <<- inserted[-length(inserted)]
      removeUI(
        ## pass in appropriate div id
        selector = paste0('#', insertedV[length(insertedV)])
      )
      insertedV <<- insertedV[-length(insertedV)]
      rvs$evidence <<- rvs$evidence[-length(inserted)]
      rvs$value <<- rvs$value[-length(insertedV)]
      rvs$evidenceObserve <<- rvs$evidenceObserve[-length(inserted)]
      rvs$valueObserve <<- rvs$valueObserve[-length(insertedV)]
    },error=function(e){
      shinyalert(toString(e), type = "error")
    })
  })
  observeEvent(input$event,{
    tryCatch({
      if(input$event=="")
      {
        updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNames[1]]),value = nlevels(DiscreteData[,nodeNames[1]]))
      }
      else
      {
        updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,input$event]),value = nlevels(DiscreteData[,input$event]))
      }
    },error=function(e){
      shinyalert(toString(e), type = "error")
    })
  })
  observeEvent(input$plotBtn,{
    tryCatch({
      confidence<<-1
      str1 <<- ""
      count =1
      for(elem in inserted)
      {
        vid = insertedV[which(inserted == elem)]
        str1 <<- paste0(str1,"(", input[[elem]], "=='", input[[vid]], "')")
        if(count!=length(inserted))
        {
          str1 <<- paste0(str1," & ")
        }
        count = count + 1
      }
      probs = prop.table(table(cpdist(bn.hc.boot.fit,input$event,evidence = eval(parse(text = str1)))))
      output$distPlot = renderPlot({par(mar=c(5,3,3,3))
        par(oma=c(5,3,3,3))
        barx<<-barplot(probs,
                col = "lightblue",
                main = paste("Conditional Probabilities on ",input$event),
                border = NA,
                xlab = "",
                ylab = "Probabilities",
                ylim = c(0,1),
                las=2)
        text(x = barx,y = round(probs,digits = 4),label = round(probs,digits = 4), pos = 3, cex = 0.8, col = "black")
        })
    },error = function(e){
      shinyalert(toString(e), type = "error")
    })

  })
  observeEvent(input$plotStrengthBtn,{
    tryCatch({
      confidence<<-2
      probT = c()
      for(i in 1:input$plotStrengthBtn)
      {
        str1 <<- ""
        count =1
        for(elem in inserted)
        {
          vid = insertedV[which(inserted == elem)]
          str1 <<- paste0(str1,"(", input[[elem]], "=='", input[[vid]], "')")
          if(count!=length(inserted))
          {
            str1 <<- paste0(str1," & ")
          }
          count = count + 1
        }
        probs = prop.table(table(cpdist(bn.hc.boot.fit,input$event,evidence = eval(parse(text = str1)))))
        probT = rbind(probT,probs)
      }
      ee = 1
      ee$mean = colMeans(probT)
      ee$sd = apply(probT, 2, sd)
      output$distPlot = renderPlot({par(mar=c(5,3,3,3))
        par(oma=c(5,3,3,3))
        barx <<-barplot(ee$mean,
                       col = "lightblue",
                       main = paste("Conditional Probabilities on ",input$event),
                       border = NA,
                       xlab = "",
                       ylab = "Probabilities",
                       ylim = c(0,1),
                       las=2)
        text(x = barx,y = round(ee$mean,digits = 4),label = round(ee$mean,digits = 4), pos = 3, cex = 0.8, col = "black")
        error.bar(barx,ee$mean, 1.96*ee$sd/sqrt(input$plotStrengthBtn))})

    },error = function(e){
      shinyalert(toString(e), type = "error")
    })

  })
  observeEvent(input$sortPlot,{
    if(confidence==1)
    {
      tryCatch({
        confidence<<-1
        str1 <<- ""
        count =1
        for(elem in inserted)
        {
          vid = insertedV[which(inserted == elem)]
          str1 <<- paste0(str1,"(", input[[elem]], "=='", input[[vid]], "')")
          if(count!=length(inserted))
          {
            str1 <<- paste0(str1," & ")
          }
          count = count + 1
        }
        probs = sort(prop.table(table(cpdist(bn.hc.boot.fit,input$event,evidence = eval(parse(text = str1))))),decreasing = T)[1:input$NumBar]
        output$distPlot = renderPlot({par(mar=c(5,3,3,3))
          par(oma=c(5,3,3,3))
          barx<<-barplot(probs,
                        col = "lightblue",
                        main = paste("Conditional Probabilities on ",input$event),
                        border = NA,
                        xlab = "",
                        ylab = "Probabilities",
                        ylim = c(0,1),
                        las=2)
          text(x = barx,y = round(probs,digits = 4),label = round(probs,digits = 4), pos = 3, cex = 0.8, col = "black")
        })


      },error = function(e){
        shinyalert(toString(e), type = "error")
      })
    }
    else
    {
      tryCatch({
        confidence<<-2
        probT = c()
        for(i in 1:input$plotStrengthBtn)
        {
          str1 <<- ""
          count =1
          for(elem in inserted)
          {
            vid = insertedV[which(inserted == elem)]
            str1 <<- paste0(str1,"(", input[[elem]], "=='", input[[vid]], "')")
            if(count!=length(inserted))
            {
              str1 <<- paste0(str1," & ")
            }
            count = count + 1
          }
          probs = prop.table(table(cpdist(bn.hc.boot.fit,input$event,evidence = eval(parse(text = str1)))))
          probT = rbind(probT,probs)
        }
        ee = 1
        ee$mean = colMeans(probT)
        ee$sd = apply(probT, 2, sd)
        nm = names(sort(ee$mean,decreasing = T))[1:input$NumBar]
        output$distPlot = renderPlot({par(mar=c(5,3,3,3))
          par(oma=c(5,3,3,3))
          barx <<-barplot(ee$mean[nm],
                         col = "lightblue",
                         main = paste("Conditional Probabilities on ",input$event),
                         border = NA,
                         xlab = "",
                         ylab = "Probabilities",
                         ylim = c(0,1),
                         las=2)
          text(x = barx,y = round(ee$mean[nm],digits = 4),label = round(ee$mean[nm],digits = 4), pos = 3, cex = 0.8, col = "black")
          error.bar(barx,ee$mean[nm], 1.96*ee$sd[nm]/sqrt(input$plotStrengthBtn))})

      },error = function(e){
        shinyalert(toString(e), type = "error")
      })
    }
  })
  observeEvent(input$moduleSelection,{
    tryCatch({
      if(input$moduleSelection!='graph')
      {
        selectedNodes<<-communities[[input$moduleSelection]]
        from<-c()
        to<-c()
        for(i in 1:length(NetworkGraph[,1]))
        {
          if(is.element(NetworkGraph[i,1],selectedNodes))
          {
            from<-c(from,i)
          }
          if(is.element(NetworkGraph[i,2],selectedNodes))
          {
            to<-c(to,i)
          }
        }
        pruneGraph<<-NetworkGraph[intersect(from,to),]
        shapeVector<<-rep('dot',length(communities[[input$moduleSelection]]))
        for(elem in 1:length(inserted))
        {
          removeUI(
            selector = paste0('#', inserted[elem])
          )

        }
        inserted <<- c()
        for(elem2 in 1:length(insertedV))
        {
          removeUI(
            selector = paste0('#', insertedV[elem2])
          )

        }
        insertedV <<- c()
        rvs$evidence <<- c()
        rvs$value <<- c()
        rvs$evidenceObserve <<- c()
        rvs$valueObserve <<- c()
        output$distPlot <<- renderPlot(NULL)
        nodeNames <<- selectedNodes
        EventNode <<- nodeNames[1]
        EvidenceNode <<- c()
        shapeVector<<- rep('dot',length(nodeNames))
        updateSelectInput(session,'event',choices = nodeNames)
        output$netPlot<-renderVisNetwork({graph.custom(pruneGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout)})
        updateSelectInput(session,'event',choices = nodeNames)
        updateSelectizeInput(session,'varselect',choices = nodeNames)
        updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                          "ellipse", "database", "text", "diamond"))
        updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                           "ellipse", "database", "text", "diamond"))
        updateSelectInput(session,'graph_layout',choices = c("layout_nicely","layout_as_star","layout_as_tree","layout_in_circle","layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
        updateSelectInput(session,'paramSelect',choices = nodeNames)
        graph<<-graph_from_edgelist(as.matrix(pruneGraph),directed = TRUE)
        updateSelectInput(session,"neighbornodes",choices = "")
        updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNames[1]]),value = nlevels(DiscreteData[,nodeNames[1]]))
      }
      else
      {
        insertedV <<- c()
        rvs$evidence <<- c()
        rvs$value <<- c()
        rvs$evidenceObserve <<- c()
        rvs$valueObserve <<- c()
        output$distPlot <<- renderPlot(NULL)
        NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
        nodeNames <<- names(bn.hc.boot.average$nodes)
        EventNode <<- nodeNames[1]
        EvidenceNode <<- c()
        shapeVector<<- rep('dot',length(nodeNames))
        updateSelectInput(session,'event',choices = nodeNames)
        output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout)})
        updateSelectInput(session,'event',choices = nodeNames)
        updateSelectizeInput(session,'varselect',choices = nodeNames)
        updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                          "ellipse", "database", "text", "diamond"))
        updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                           "ellipse", "database", "text", "diamond"))
        updateSelectInput(session,'graph_layout',choices = c("layout_nicely","layout_as_star","layout_as_tree","layout_in_circle","layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
        updateSelectInput(session,'paramSelect',choices = nodeNames)
        graph<<-graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
        updateSelectInput(session,"neighbornodes",choices = "")
        updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNames[1]]),value = nlevels(DiscreteData[,nodeNames[1]]))
      }
    },error=function(e){
      shinyalert(toString(e), type = "error")
    })
  })
  observeEvent(input$current_node_id,{
    tryCatch({
      if(!is.null(input$current_node_id))
      {
        if(input$degreeN>1)
        {
          nlist<<-ego(graph,input$degreeN,nodes = input$current_node_id, mode = c("all", "out", "in"),mindist = 0)
          nlistP<<-ego(graph,input$degreeN-1,nodes = input$current_node_id, mode = c("all", "out", "in"),mindist = 0)
          diffList<<-setdiff(nlist[[1]]$name,nlistP[[1]]$name)
          updateSelectInput(session,"neighbornodes",choices = diffList)
        }
        else
        {
          nlist<<-ego(graph,input$degreeN,nodes = input$current_node_id, mode = c("all", "out", "in"),mindist = 0)
          updateSelectInput(session,"neighbornodes",choices = nlist[[1]]$name)
        }

      }
    },error=function(e){
      shinyalert(toString(e), type = "error")
    })
  })
  observeEvent(input$degree,{
    tryCatch({
      for(elem in inserted)
      {
        EvidenceNode = c(EvidenceNode,input[[elem]])
      }
      if(sanity==1)
      {
        EventNode = nodeNames[1]
        sanity=sanity + 1
      }
      else
      {
        EventNode = input$event
      }
      output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout)})
      assocNetworkprune<<- assocNetwork[which(assocNetwork[,3]>input$threshold),]
      output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),EvidenceNode,EventNode,input$degree,input$graph_layout,shapeVectorAssoc)})
      updateSelectInput(session,"neighbornodes",choices = "")
    },error = function(e){
      shinyalert(toString(e), type = "error")

    })


  })
  observeEvent(input$graph_layout,{
    tryCatch({
      for(elem in inserted)
      {
        EvidenceNode = c(EvidenceNode,input[[elem]])
      }
      if(sanity==1)
      {
        EventNode = nodeNames[1]
        sanity=sanity + 1
      }
      else
      {
        EventNode = input$event
      }
      output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout)})
      assocNetworkprune<<- assocNetwork[which(assocNetwork[,3]>input$threshold),]
      output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),EvidenceNode,EventNode,input$degree,input$graph_layout,shapeVectorAssoc)})
      updateSelectInput(session,"neighbornodes",choices = "")
    },error = function(e){
      shinyalert(toString(e), type = "error")

    })

  })
  observeEvent(input$graphBtn,{
    tryCatch({
      for(elem in inserted)
      {
        EvidenceNode = c(EvidenceNode,input[[elem]])
      }
      if(sanity==1)
      {
        EventNode = nodeNames[1]
        sanity=sanity + 1
      }
      else
      {
        EventNode = input$event
      }
      output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout)})
      updateSelectInput(session,"neighbornodes",choices = "")
      },error = function(e){
      shinyalert(toString(e), type = "error")

    })

  })
  observeEvent(input$graphBtn2,{
    tryCatch({
      for(elem in inserted)
      {
        EvidenceNode = c(EvidenceNode,input[[elem]])
      }
      if(sanity==1)
      {
        EventNode = nodeNames[1]
        sanity=sanity + 1
      }
      else
      {
        EventNode = input$event
      }
      assocNetworkprune<<- assocNetwork[which(assocNetwork[,3]>input$threshold),]
      output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),EvidenceNode,EventNode,input$degree,input$graph_layout,shapeVectorAssoc)})
    },error = function(e){
      shinyalert(toString(e), type = "error")

    })
  })
  observeEvent(input$group,{
    tryCatch({
      shapeVector[which(nodeNames %in% input$varselect)] <<- input$varshape
      shapeVectorAssoc[which(nodeNames %in% input$varselect)] <<- input$varshape
      for(elem in inserted)
      {
        EvidenceNode = c(EvidenceNode,input[[elem]])
      }
      if(sanity==1)
      {
        EventNode = nodeNames[1]
        sanity=sanity + 1
      }
      else
      {
        EventNode = input$event
      }
      output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout)})
      output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),EvidenceNode,EventNode,input$degree,input$graph_layout,shapeVectorAssoc)})
      updateSelectInput(session,"neighbornodes",choices = "")
    },error = function(e){
      shinyalert(toString(e), type = "error")

    })


  })

  observeEvent(input$group2,{
    tryCatch({
      shapeVector<<-shapeVector[1:length(nodeNames)]
      shapeVector[eval(parse(text = input$varselectvector))] <<- input$varshape2
      shapeVectorAssoc<<-shapeVectorAssoc[1:length(unique(c(assocNetworkprune[,1],assocNetworkprune[,2])))]
      shapeVectorAssoc[eval(parse(text = input$varselectvector))] <<- input$varshape2
      for(elem in inserted)
      {
        EvidenceNode = c(EvidenceNode,input[[elem]])
      }
      if(sanity==1)
      {
        EventNode = nodeNames[1]
        sanity=sanity + 1
      }
      else
      {
        EventNode = input$event
      }
      output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout)})
      output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),EvidenceNode,EventNode,input$degree,input$graph_layout,shapeVectorAssoc)})
      updateSelectInput(session,"neighbornodes",choices = "")
    },error = function(e){
      shinyalert(toString(e), type = "error")

    })

  })


  #homeIntroduction Event
  observeEvent(input$homeIntro,{
      print(input$sidebarMenu)
      if(input$sidebarMenu == "Home")
      {introjs(session, options = list(steps = homeHelp))}
      else if(input$sidebarMenu == "Structure")
      {
        print(input$control_tabs)
        if(input$control_tabs == "Data")
        {
          introjs(session, options = list(steps = dataHelp))
        }
        else if(input$control_tabs == "Graph")
        introjs(session, options = list(steps = graphHelp))

      }
    })
})
