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
source('error.bar.R')
source('graph.custom.R')
source('graph.custom.assoc.R')
source('custom.discretize.R')
source('check.NA.R')
source('check.discrete.R')
source('custom.association.R')

shinyServer(function(input, output,session) {
  #Data upload limit
  options(shiny.maxRequestSize=1500*1024^2)
  #Structure Initialization
  D <- get(load('a.RData'))
  DiscreteData <- D
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
  #Association Network
  assocNetwork<-custom.association(DiscreteData,"cramer's V")
  assocNetworkprune<- assocNetwork[which(assocNetwork[,3]>0.3),]
  shapeVectorAssoc<- rep('dot',length(unique(c(assocNetworkprune[,1],assocNetworkprune[,2]))))
  output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),EvidenceNode,EventNode,2,'layout_nicely',shapeVectorAssoc)})
  #Tables
  updateSelectInput(session,"tableName",choices = c("Data","Association Graph","Bayesian Graph","Cross Validation Results"))
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
  observeEvent(input$tableName,{
    if(input$tableName=="Data")
    {
      output$tableOut<- DT::renderDataTable({DiscreteData},options = list(scrollX = TRUE,pageLength = 10))
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
  })
  observeEvent(input$start,{
    updateTabItems(session, "sidebarMenu", "Structure")
    })
  observeEvent(input$threshold,{
    assocNetworkprune<<- assocNetwork[which(assocNetwork[,3]>input$threshold),]
    shapeVectorAssoc<<- rep('dot',length(unique(c(assocNetworkprune[,1],assocNetworkprune[,2]))))
    output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),EvidenceNode,EventNode,input$degree,input$graph_layout,shapeVectorAssoc)})
  })
  observeEvent(input$association,{
    assocNetwork<<-custom.association(DiscreteData,input$assocType)
    assocNetworkprune<<- assocNetwork[which(assocNetwork[,3]>input$threshold),]
    shapeVectorAssoc<<- rep('dot',length(unique(c(assocNetworkprune[,1],assocNetworkprune[,2]))))
    output$assocPlot<-renderVisNetwork({graph.custom.assoc(assocNetworkprune,unique(c(assocNetworkprune[,1],assocNetworkprune[,2])),EvidenceNode,EventNode,input$degree,input$graph_layout,shapeVectorAssoc)})
  })
  observeEvent(input$calLoss,{
    withProgress(message = "Validating Model", value = 0, {
    bn.validate<-bn.cv(DiscreteData[,nodeNames],bn=bn.hc.boot.average,fit = input$paramMethod3,method = input$crossFunc)
    predError<<-c()
    for(n in nodeNames)
    {
      targetLoss<<-bn.cv(DiscreteData[,nodeNames],bn=bn.hc.boot.average,fit = input$paramMethod3,loss = input$lossFunc,method = input$crossFunc,loss.args = list(target = n))
      predError<<-rbind(predError,targetLoss[[1]]$loss)
    }
    rownames(predError)<<-nodeNames
    colnames(predError)<<-"Classification Error"
    output$valLoss<<-renderText({bn.validate[[1]]$loss})})
  })
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
              },error = function(e){
                DiscreteData<<- readRDS(inFile$datapath)
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
            DiscreteData <<- read.csv(inFile$datapath,stringsAsFactors = T)
          }
          else
          {
            shinyalert("Added file is not a .csv file.Please upload a CSV file.", type = "error")
          }
        }
        check.discrete(DiscreteData)
        check.NA(DiscreteData)
        DiscreteData<<-as.data.frame(DiscreteData)
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
      })},error = function(e){
        print("error0")
        print(e)
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
      check.discrete(DiscreteData)
      check.NA(DiscreteData)
    })}, error = function(e){
      print("error0")
      print(toString(e))
      type <- toString(input$dtype)
      messageString <- "Error imputing missingness using missRanger method. Try uploading pre-imputed data."
      shinyalert(messageString, type = "error")
    })

  })


  # Get the data selection from user
  observeEvent(input$structFile,

               {# Get the uploaded file from user
                 inFile <- input$structFile
                 if (is.null(inFile))
                 {
                   print("Structure File is empty")
                 }
                 else
                 {
                   if(is.null(DiscreteData))
                   {
                     print("Please Upload Data File First")
                   }
                   else
                   {
                     tryCatch({
                       tryCatch({
                         bn.hc.boot.average <<- get(load(inFile$datapath))
                       },error = function(e){
                         bn.hc.boot.average <<- readRDS(inFile$datapath)
                       })

                       #print("2")
                       bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod)
                       print("Learned Structure loaded")
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
                     },error = function(e){
                       print("error 1")
                       shinyalert(toString(e), type = "error")
                       #output$netPlot<- renderForceNetwork({validate(e)})
                     })
                   }
                 }
               }
  )



  # Learn the structure of the network
  observeEvent(input$learnBtn, {
    tryCatch({
      print("Structure Learning started")
      if (is.null(DiscreteData))
        return(NULL)

      # Create a Progress object
      progress <- shiny::Progress$new()

      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = "Learning network structure", value = 0)

      # Get the selected learning algorithm from the user and learn the network
      bn.hc.boot <<- boot.strength(data = DiscreteData, R = input$boot, m = ceiling(nrow(DiscreteData)*input$SampleSize), algorithm = input$alg)
      bn.hc.boot.pruned <<- bn.hc.boot[bn.hc.boot$strength > input$edgeStrength & bn.hc.boot$direction > input$directionStrength,]
      bn.hc.boot.average <<- cextend(averaged.network(bn.hc.boot.pruned))
      bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2)
      print("Structure learning done")
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
    },error = function(e){
      print("error 2")
      shinyalert(toString(e), type = "error")
      #output$netPlot<- renderForceNetwork({validate(e)})#"Error: in processing your request of structure learning. Possible reasins of error can be unsuited learning algorithm, .Rdata format not used for data or structure upload, inappropriate bnlearn file, thresholds set for pruning returns no results"

    })


  })
  observeEvent(input$learnSBtn, {
    tryCatch({
      print("Structure Learning started")
      if (is.null(DiscreteData))
        return(NULL)

      # Create a Progress object
      progress <- shiny::Progress$new()
      print(DiscreteData)
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = "Learning network structure", value = 0)

      # Get the selected learning algorithm from the user and learn the network
      if(input$alg == 'hc')
      {
        bn.hc.boot.average <<- cextend(bnlearn::hc(DiscreteData))
      }
      else if(input$alg =="pc.stable")
      {
        bn.hc.boot.average <<- cextend(bnlearn::pc.stable(DiscreteData))
      }
      else if(input$alg == 'tabu')
      {
        bn.hc.boot.average <<- cextend(bnlearn::tabu(DiscreteData))
      }
      else if(input$alg == 'gs')
      {
        bn.hc.boot.average <<- bnlearn::gs(DiscreteData)
      }
      else if(input$alg == 'iamb')
      {
        bn.hc.boot.average <<- cextend(bnlearn::iamb(DiscreteData))
      }
      else if(input$alg == 'fast.iamb')
      {
        bn.hc.boot.average <<- cextend(bnlearn::fast.iamb(DiscreteData))
      }
      else if(input$alg=='inter.iamb')
      {
        bn.hc.boot.average <<- cextend(bnlearn::inter.iamb(DiscreteData))
      }
      else if(input$alg == 'mmhc')
      {
        bn.hc.boot.average <<- cextend(bnlearn::mmhc(DiscreteData))
      }
      else if(input$alg == 'rsmax2')
      {
        bn.hc.boot.average <<- cextend(bnlearn::rsmax2(DiscreteData))
      }
      else if(input$alg == 'mmpc')
      {
        bn.hc.boot.average <<- cextend(bnlearn::mmpc(DiscreteData))
      }
      else if(input$alg == 'si.hiton.pc')
      {
        bn.hc.boot.average <<- cextend(bnlearn::si.hiton.pc(DiscreteData))
      }
      else if(input$alg == 'aracne')
      {
        bn.hc.boot.average <<- cextend(bnlearn::aracne(DiscreteData))
      }
      else
      {
        bn.hc.boot.average <<- cextend(bnlearn::chow.liu(DiscreteData))
      }
      #bn.hc.boot.average <<- bnlearn::hc(DiscreteData)
      bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = input$paramMethod2)
      print("Structure learning done")
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
    },error = function(e){
      print("error 3")
      shinyalert(toString(e), type = "error")
      #output$netPlot<- renderForceNetwork({validate(e)})

    })


  })
  observeEvent(input$paramSelect,{
    #print(bn.hc.boot.fit[input$paramSelect])
    tryCatch({
      output$parameterPlot<-renderPlot({bn.fit.barchart(bn.hc.boot.fit[[input$paramSelect]])})
    },error = function(e){
      shinyalert(toString(e), type = "error")
    })

  })

  observeEvent(input$insertBtn, {
    tryCatch({
      nodeNames = names(bn.hc.boot.average$nodes)
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
          #print(valID)
          updateSelectInput(session,valID, choices = levels(DiscreteData[,input[[id]]]))
        },error = function(e){
          shinyalert(toString("Please learn structure or upload structure on new data uploaded to make infrences"), type = "error")
          #output$distPlot<-renderPlot({validate("error: Please learn structure or upload structure on new data uploaded to make infrences")})
        })
      }))

    },error = function(e){
      print("error 4")
      shinyalert(toString(e), type = "error")
      #output$netPlot<- renderForceNetwork({validate(e)})

    })
  })

  observeEvent(input$removeBtn, {
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
  })
  observeEvent(input$plotBtn,{
    tryCatch({
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
        barx<-barplot(probs,
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
      print("error 5")
      shinyalert(toString(e), type = "error")
      #output$distPlot<- renderPlot({validate(e)})
    })

  })
  observeEvent(input$plotStrengthBtn,{
    tryCatch({
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
        barx <-barplot(ee$mean,
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
      print("error 6")
      shinyalert(toString(e), type = "error")
      output$distPlot<- renderPlot({validate(e)})
    })

  })
  observeEvent(input$saveBtn,{
    tryCatch({
      save(bn.hc.boot.average,file = input$path)

    },error = function(e){
      shinyalert(toString(e), type = "error")

    })

  })
  observeEvent(input$secondSaveBtn,{
    tryCatch({
      print("save")
      save(bn.hc.boot.average,file = input$path)

    },error = function(e){
      shinyalert(toString(e), type = "error")

    })

  })
  observeEvent(input$saveBtn2,{
    tryCatch({
      write.csv(NetworkGraph,file = input$path2,row.names = FALSE)


    },error = function(e){
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
    },error = function(e){
      shinyalert(toString(e), type = "error")

    })

  })


  #homeIntroduction Event
  observeEvent(input$homeIntro,
               {
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

               }
               )

})
