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

shinyServer(function(input, output,session) {
  options(shiny.maxRequestSize=1500*1024^2)
  temp<- 1
  output$distPlot<- renderPlot(validate("Built infrence plot will be displayed here"))
  D = get(load('a.RData'))
  DiscreteData <<- D
  bn.hc.boot <<- boot.strength(data = DiscreteData,R = 5,m =ceiling(nrow(DiscreteData)*0.7) ,algorithm = "hc")
  bn.hc.boot.pruned <<- bn.hc.boot[bn.hc.boot$strength > 0.5 & bn.hc.boot$direction >0.5,]
  bn.hc.boot.average <<- cextend(averaged.network(bn.hc.boot.pruned))
  bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = 'bayes')
  NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
  nodeNames <<- names(bn.hc.boot.average$nodes)
  inserted  <<- c()
  insertedV <<- c()
  EventNode <<- nodeNames[1]
  EvidenceNode <<- c()
  updateSelectInput(session,'event',choices = nodeNames)
  updateSelectInput(session,'graph_layout',choices = c("layout_nicely","layout_as_star","layout_as_tree","layout_in_circle","layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
  rvs <<- reactiveValues(evidence = list(),values = list(),evidenceObserve = list(),valueObserve = list())
  networkData <<- NetworkGraph[,1:2]
  selectedNodes <<- nodeNames
  src <- NetworkGraph$from
  target <- NetworkGraph$to
  nodes <- data.frame(name = selectedNodes)
  nodes$id <- 0:(nrow(nodes) - 1)
  colnames(networkData) = c("src","target")
  edges <- networkData %>%
    left_join(nodes, by = c("src" = "name")) %>%
    select(-src) %>%
    rename(source = id) %>%
    left_join(nodes, by = c("target" = "name")) %>%
    select(-target) %>%
    rename(target = id)

  edges$width <- 1

  nodes$group <- "not in use"
  nodes[which(nodes$name %in% EvidenceNode),3] = "Evidence"
  nodes[which(nodes$name == EventNode),3] = "Event"
  ColourScale <- 'd3.scaleOrdinal().domain(["not in use","Event","Evidence"]).range(["#0E5AE8", "#50E80E","#FF0000"]);'
  visNodes<- data.frame(id = selectedNodes,
                        label = selectedNodes,
                        group = nodes$group)
  visEdges<- data.frame(from = NetworkGraph$from,
                        to = NetworkGraph$to)
  output$netPlot<-renderVisNetwork({
    visNetwork(visNodes, visEdges, width = "200%") %>%
      visEdges(arrows ="to",smooth = T,color = list(color = "grey",highlight = "black",hover = "black"))%>%
      visGroups(groupname = "not in use", color = list(background = "lightblue",highlight = 'blue', hover = "blue")) %>%
      visGroups(groupname = "Event", color = list(background = "lightgreen",highlight = "green", hover = "green"))%>%
      visGroups(groupname = "Evidence", color = list(background = "pink",highlight = "red", hover = "red")) %>%
      visLegend(width = 0.1, position = "left")%>%
      visNodes(shape = "dot") %>%
      visOptions(highlightNearest = list(enabled =TRUE, degree = input$degree,hover = T, hideColor = 'rgba(200,200,200,0)'), nodesIdSelection = TRUE)%>%
      visInteraction(navigationButtons = TRUE)%>%
      visIgraphLayout(layout = input$graph_layout)
  })

  # Get the data selection from user
  observeEvent(input$dataFile,

               {# Get the uploaded file from user
                 inFile <- input$dataFile
                 if (is.null(inFile))
                 {
                   #DiscreteData <<- NULL
                   print("Data File is empty")
                 }
                 else
                 {
                   tryCatch({
                     if(input$format==".RData")
                     {
                       if(file_ext(inFile$datapath) == "RData")
                       {
                         DiscreteData <<- get(load(inFile$datapath))
                       }
                       else
                       {
                         shinyalert("Added file is not a .RData file. Try uploading again.", type = "error")
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
                         shinyalert("Added file is not a .csv file. Try uploading again.", type = "error")
                       }
                     }

                   },error = function(e){
                     print("error0")
                     shinyalert(c("Error in loading data: ",toString(e)), type = "error")
                   })
                   print("Data loaded")
                 }


               }
  )
  observeEvent(input$discretize,{
    tryCatch(
      {withProgress(message = "Discretizing data", value = 0, {
        tempDiscreteData <<- DiscreteData
        int <- sapply(tempDiscreteData,is.integer)
        tempDiscreteData[,int] <<- lapply(tempDiscreteData[,int], as.numeric)
        tempDiscreteData <<- as.data.frame(bnlearn::discretize(data.frame(tempDiscreteData),method=input$dtype))
        tempDiscreteData[,which(mapply(nlevels,tempDiscreteData[,sapply(DiscreteData,is.factor)])<2)] = NULL
        tempDiscreteData <<- droplevels(tempDiscreteData)
        print(tempDiscreteData)
        DiscreteData <<-tempDiscreteData
      })},error = function(e){
        print("error0")
        print(toString(e))
        type <- toString(input$dtype)
        messageString <- paste(c("Error is discretising using method ", type, ". Try using other method or upload pre-discretised data."), collapse = '')
        shinyalert(messageString, type = "error")
      })

  })

  observeEvent(input$impute,{
    tryCatch(
    {withProgress(message = "Imputing missing data", value = 0, {
      DiscreteData <<- missRanger(DiscreteData,maxiter = 2)
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
                       bn.hc.boot.average <<- get(load(inFile$datapath))
                       #print("2")
                       bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = 'bayes')
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
                       updateSelectInput(session,'event',choices = nodeNames)
                       networkData = NetworkGraph[,1:2]
                       selectedNodes <<- nodeNames
                       src <<- NetworkGraph$from
                       target <<- NetworkGraph$to
                       nodes <<- data.frame(name = nodeNames)
                       nodes$id <- 0:(nrow(nodes) - 1)
                       colnames(networkData) = c("src","target")
                       edges <- networkData %>%
                         left_join(nodes, by = c("src" = "name")) %>%
                         select(-src) %>%
                         rename(source = id) %>%
                         left_join(nodes, by = c("target" = "name")) %>%
                         select(-target) %>%
                         rename(target = id)

                       edges$width <- 1

                       nodes$group <- "not in use"
                       nodes[which(nodes$name %in% EvidenceNode),3] = "Evidence"
                       nodes[which(nodes$name == EventNode),3] = "Event"
                       visNodes<- data.frame(id = selectedNodes,
                                             label = selectedNodes,
                                             group = nodes$group)
                       visEdges<- data.frame(from = NetworkGraph$from,
                                             to = NetworkGraph$to)
                       output$netPlot<-renderVisNetwork({
                         visNetwork(visNodes, visEdges, width = "200%") %>%
                           visEdges(arrows ="to",smooth = T,color = list(color = "grey",highlight = "black",hover = "black"))%>%
                           visGroups(groupname = "not in use", color = list(background = "lightblue",highlight = 'blue', hover = "blue")) %>%
                           visGroups(groupname = "Event", color = list(background = "lightgreen",highlight = "green", hover = "green"))%>%
                           visGroups(groupname = "Evidence", color = list(background = "pink",highlight = "red", hover = "red")) %>%
                           visLegend(width = 0.1, position = "left")%>%
                           visNodes(shape = "dot") %>%
                           visOptions(highlightNearest = list(enabled =TRUE, degree = input$degree,hover = T, hideColor = 'rgba(200,200,200,0)'), nodesIdSelection = TRUE)%>%
                           visInteraction(navigationButtons = TRUE)%>%
                           visIgraphLayout(layout = input$graph_layout)
                       })
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
      bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = 'bayes')
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
      updateSelectInput(session,'event',choices = nodeNames)
      networkData = NetworkGraph[,1:2]
      selectedNodes <<- nodeNames
      src <- NetworkGraph$from
      target <- NetworkGraph$to
      nodes <- data.frame(name = nodeNames)
      nodes$id <- 0:(nrow(nodes) - 1)
      colnames(networkData) = c("src","target")
      edges <- networkData %>%
        left_join(nodes, by = c("src" = "name")) %>%
        select(-src) %>%
        rename(source = id) %>%
        left_join(nodes, by = c("target" = "name")) %>%
        select(-target) %>%
        rename(target = id)

      edges$width <- 1

      nodes$group <- "not in use"
      nodes[which(nodes$name %in% EvidenceNode),3] = "Evidence"
      nodes[which(nodes$name == EventNode),3] = "Event"
      visNodes<- data.frame(id = selectedNodes,
                            label = selectedNodes,
                            group = nodes$group)
      visEdges<- data.frame(from = NetworkGraph$from,
                            to = NetworkGraph$to)
      output$netPlot<-renderVisNetwork({
        visNetwork(visNodes, visEdges, width = "200%") %>%
          visEdges(arrows ="to",smooth = T,color = list(color = "grey",highlight = "black",hover = "black"))%>%
          visGroups(groupname = "not in use", color = list(background = "lightblue",highlight = 'blue', hover = "blue")) %>%
          visGroups(groupname = "Event", color = list(background = "lightgreen",highlight = "green", hover = "green"))%>%
          visGroups(groupname = "Evidence", color = list(background = "pink",highlight = "red", hover = "red")) %>%
          visLegend(width = 0.1, position = "left")%>%
          visNodes(shape = "dot") %>%
          visOptions(highlightNearest = list(enabled =TRUE, degree = input$degree,hover = T, hideColor = 'rgba(200,200,200,0)'), nodesIdSelection = TRUE)%>%
          visInteraction(navigationButtons = TRUE)%>%
          visIgraphLayout(layout = input$graph_layout)
      })
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
        bn.hc.boot.average <<- bnlearn::hc(DiscreteData)
      }
      else if(input$alg == 'tabu')
      {
        bn.hc.boot.average <<- bnlearn::tabu(DiscreteData)
      }
      else if(input$alg == 'gs')
      {
        bn.hc.boot.average <<- bnlearn::gs(DiscreteData)
      }
      else if(input$alg == 'iamb')
      {
        bn.hc.boot.average <<- bnlearn::iamb(DiscreteData)
      }
      else if(input$alg == 'fast.iamb')
      {
        bn.hc.boot.average <<- bnlearn::fast.iamb(DiscreteData)
      }
      else if(input$alg=='inter.iamb')
      {
        bn.hc.boot.average <<- bnlearn::inter.iamb(DiscreteData)
      }
      else if(input$alg == 'mmhc')
      {
        bn.hc.boot.average <<- bnlearn::mmhc(DiscreteData)
      }
      else if(input$alg == 'rsmax2')
      {
        bn.hc.boot.average <<- bnlearn::rsmax2(DiscreteData)
      }
      else if(input$alg == 'mmpc')
      {
        bn.hc.boot.average <<- bnlearn::mmpc(DiscreteData)
      }
      else if(input$alg == 'si.hiton.pc')
      {
        bn.hc.boot.average <<- bnlearn::si.hiton.pc(DiscreteData)
      }
      else if(input$alg == 'aracne')
      {
        bn.hc.boot.average <<- bnlearn::aracne(DiscreteData)
      }
      else
      {
        bn.hc.boot.average <<- bnlearn::chow.liu(DiscreteData)
      }
      #bn.hc.boot.average <<- bnlearn::hc(DiscreteData)
      bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = 'bayes')
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
      updateSelectInput(session,'event',choices = nodeNames)
      networkData = NetworkGraph[,1:2]
      selectedNodes <<- nodeNames
      src <- NetworkGraph$from
      target <- NetworkGraph$to
      nodes <- data.frame(name = nodeNames)
      nodes$id <- 0:(nrow(nodes) - 1)
      colnames(networkData) = c("src","target")
      edges <- networkData %>%
        left_join(nodes, by = c("src" = "name")) %>%
        select(-src) %>%
        rename(source = id) %>%
        left_join(nodes, by = c("target" = "name")) %>%
        select(-target) %>%
        rename(target = id)

      edges$width <- 1

      nodes$group <- "not in use"
      nodes[which(nodes$name %in% EvidenceNode),3] = "Evidence"
      nodes[which(nodes$name == EventNode),3] = "Event"
      visNodes<- data.frame(id = selectedNodes,
                            label = selectedNodes,
                            group = nodes$group)
      visEdges<- data.frame(from = NetworkGraph$from,
                            to = NetworkGraph$to)
      output$netPlot<-renderVisNetwork({
        visNetwork(visNodes, visEdges, width = "200%") %>%
          visEdges(arrows ="to",smooth = T,color = list(color = "grey",highlight = "black",hover = "black"))%>%
          visGroups(groupname = "not in use", color = list(background = "lightblue",highlight = 'blue', hover = "blue")) %>%
          visGroups(groupname = "Event", color = list(background = "lightgreen",highlight = "green", hover = "green"))%>%
          visGroups(groupname = "Evidence", color = list(background = "pink",highlight = "red", hover = "red")) %>%
          visLegend(width = 0.1, position = "left")%>%
          visNodes(shape = "dot") %>%
          visOptions(highlightNearest = list(enabled =TRUE, degree = input$degree,hover = T, hideColor = 'rgba(200,200,200,0)'), nodesIdSelection = TRUE)%>%
          visInteraction(navigationButtons = TRUE)%>%
          visIgraphLayout(layout = input$graph_layout)
      })
    },error = function(e){
      print("error 3")
      shinyalert(toString(e), type = "error")
      #output$netPlot<- renderForceNetwork({validate(e)})

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
        barplot(probs,
                col = "lightblue",
                main = "Conditional Probabilities",
                border = NA,
                xlab = "",
                ylab = "Probabilities",
                ylim = c(0,1),
                las=2)})

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
                       main = "Conditional Probabilities",
                       border = NA,
                       xlab = "",
                       ylab = "Probabilities",
                       ylim = c(0,1),
                       las=2)
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

    },error = function(e)
    {
      print(e)

    })

  })
  observeEvent(input$saveBtn2,{
    tryCatch({
      write.csv(NetworkGraph,file = input$path2,row.names = FALSE)


    },error = function(e)
    {
      print(e)

    })
  })
  observeEvent(input$degree,{
    for(elem in inserted)
    {
      EvidenceNode = c(EvidenceNode,input[[elem]])
    }
    if(temp==1)
    {
      EventNode = nodeNames[1]
      temp = temp + 1
    }
    else
    {
      EventNode = input$event
    }

    print(EventNode)
    networkData <<- NetworkGraph[,1:2]
    src <- NetworkGraph$from
    target <- NetworkGraph$to
    nodes <- data.frame(name = selectedNodes)
    nodes$id <- 0:(nrow(nodes) - 1)
    colnames(networkData) = c("src","target")
    edges <- networkData %>%
      left_join(nodes, by = c("src" = "name")) %>%
      select(-src) %>%
      rename(source = id) %>%
      left_join(nodes, by = c("target" = "name")) %>%
      select(-target) %>%
      rename(target = id)

    edges$width <- 1
    nodes$group <- "not in use"
    nodes[which(nodes$name %in% EvidenceNode),3] = "Evidence"
    nodes[which(nodes$name == EventNode),3] = "Event"
    visNodes<- data.frame(id = selectedNodes,
                          label = selectedNodes,
                          group = nodes$group)
    visEdges<- data.frame(from = NetworkGraph$from,
                          to = NetworkGraph$to)
    output$netPlot<-renderVisNetwork({
      visNetwork(visNodes, visEdges, width = "200%") %>%
        visEdges(arrows ="to",smooth = T,color = list(color = "grey",highlight = "black",hover = "black"))%>%
        visGroups(groupname = "not in use", color = list(background = "lightblue",highlight = 'blue', hover = "blue")) %>%
        visGroups(groupname = "Event", color = list(background = "lightgreen",highlight = "green", hover = "green"))%>%
        visGroups(groupname = "Evidence", color = list(background = "pink",highlight = "red", hover = "red")) %>%
        visLegend(width = 0.1, position = "left")%>%
        visNodes(shape = "dot") %>%
        visOptions(highlightNearest = list(enabled =TRUE, degree = input$degree,hover = T, hideColor = 'rgba(200,200,200,0)'), nodesIdSelection = TRUE)%>%
        visInteraction(navigationButtons = TRUE)%>%
        visIgraphLayout(layout = input$graph_layout)
    })

  })
  observeEvent(input$graph_layout,{
    for(elem in inserted)
    {
      EvidenceNode = c(EvidenceNode,input[[elem]])
    }
    if(temp==1)
    {
      EventNode = nodeNames[1]
      temp = temp + 1
    }
    else
    {
      EventNode = input$event
    }

    print(EventNode)
    networkData <<- NetworkGraph[,1:2]
    src <- NetworkGraph$from
    target <- NetworkGraph$to
    nodes <- data.frame(name = selectedNodes)
    nodes$id <- 0:(nrow(nodes) - 1)
    colnames(networkData) = c("src","target")
    edges <- networkData %>%
      left_join(nodes, by = c("src" = "name")) %>%
      select(-src) %>%
      rename(source = id) %>%
      left_join(nodes, by = c("target" = "name")) %>%
      select(-target) %>%
      rename(target = id)

    edges$width <- 1
    nodes$group <- "not in use"
    nodes[which(nodes$name %in% EvidenceNode),3] = "Evidence"
    nodes[which(nodes$name == EventNode),3] = "Event"
    visNodes<- data.frame(id = selectedNodes,
                          label = selectedNodes,
                          group = nodes$group)
    visEdges<- data.frame(from = NetworkGraph$from,
                          to = NetworkGraph$to)
    output$netPlot<-renderVisNetwork({
      visNetwork(visNodes, visEdges, width = "200%") %>%
        visEdges(arrows ="to",smooth = T,color = list(color = "grey",highlight = "black",hover = "black"))%>%
        visGroups(groupname = "not in use", color = list(background = "lightblue",highlight = 'blue', hover = "blue")) %>%
        visGroups(groupname = "Event", color = list(background = "lightgreen",highlight = "green", hover = "green"))%>%
        visGroups(groupname = "Evidence", color = list(background = "pink",highlight = "red", hover = "red")) %>%
        visLegend(width = 0.1, position = "left")%>%
        visNodes(shape = "dot") %>%
        visOptions(highlightNearest = list(enabled =TRUE, degree = input$degree,hover = T, hideColor = 'rgba(200,200,200,0)'), nodesIdSelection = TRUE)%>%
        visInteraction(navigationButtons = TRUE)%>%
        visIgraphLayout(layout = input$graph_layout)
    })
  })
  observeEvent(input$graphBtn,{
    for(elem in inserted)
    {
      EvidenceNode = c(EvidenceNode,input[[elem]])
    }
    EventNode = input$event
    networkData <<- NetworkGraph[,1:2]
    src <- NetworkGraph$from
    target <- NetworkGraph$to
    nodes <- data.frame(name = selectedNodes)
    nodes$id <- 0:(nrow(nodes) - 1)
    colnames(networkData) = c("src","target")
    edges <- networkData %>%
      left_join(nodes, by = c("src" = "name")) %>%
      select(-src) %>%
      rename(source = id) %>%
      left_join(nodes, by = c("target" = "name")) %>%
      select(-target) %>%
      rename(target = id)

    edges$width <- 1
    nodes$group <- "not in use"
    nodes[which(nodes$name %in% EvidenceNode),3] = "Evidence"
    nodes[which(nodes$name == EventNode),3] = "Event"
    visNodes<- data.frame(id = selectedNodes,
                          label = selectedNodes,
                          group = nodes$group)
    visEdges<- data.frame(from = NetworkGraph$from,
                          to = NetworkGraph$to)
    output$netPlot<-renderVisNetwork({
      visNetwork(visNodes, visEdges, width = "200%") %>%
        visEdges(arrows ="to",smooth = T,color = list(color = "grey",highlight = "black",hover = "black"))%>%
        visGroups(groupname = "not in use", color = list(background = "lightblue",highlight = 'blue', hover = "blue")) %>%
        visGroups(groupname = "Event", color = list(background = "lightgreen",highlight = "green", hover = "green"))%>%
        visGroups(groupname = "Evidence", color = list(background = "pink",highlight = "red", hover = "red")) %>%
        visLegend(width = 0.1, position = "left")%>%
        visNodes(shape = "dot") %>%
        visOptions(highlightNearest = list(enabled =TRUE, degree = input$degree,hover = T, hideColor = 'rgba(200,200,200,0)'), nodesIdSelection = TRUE)%>%
        visInteraction(navigationButtons = TRUE)%>%
        visIgraphLayout(layout = input$graph_layout)
    })



  })

})
