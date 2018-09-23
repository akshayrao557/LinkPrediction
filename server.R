library(shiny)
library(igraph)
library(sna)
library(proxy)



shinyServer(function(input, output, session) {
  output$myText1 <- renderText({
    
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    el <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    g <- graph.data.frame(el, directed = input$directed)
    g <- simplify(g)
    
    paste("The graph contains ", vcount(g)," vertices")
    
  })
  
  output$myText2 <- renderText({
    
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    el <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    g <- graph.data.frame(el, directed = input$directed)
    g <- simplify(g)
    
    paste("The graph contains ", ecount(g)," edges")
    
  })
  
  
  
  
  output$contents <- renderTable({
      inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    rd <-read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  })
  
  
  
    
    output$dcompplot <- renderPlot({
      inFile <- input$file1
      rd <-read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
      rg <- graph.data.frame(rd,directed=F)
      E(rg)$color <- "black"
      if(ecount(rg)<50)
      {
        plot(rg, layout=layout.auto, vertex.size=input$vsize, edge.width=input$esize, edge.arrow.size=0.1) 
      }
      else
      {
        plot(rg, layout=layout.auto, vertex.size=input$vsize, edge.width=input$esize, edge.arrow.size=0.1,vertex.label=NA)
      }
  #    plot(rg, layout=layout.auto, vertex.size=input$vsize, edge.width=input$esize, edge.arrow.size=0.1) 
    })
  
  
  
    output$outplot <- renderPlot({
     inFile <- input$file1
     rd <-read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
     rg <- graph.data.frame(rd, directed=F)
     adj<-get.adjacency(rg)
     g1<-as.matrix(adj)
     euclidean<-simil(g1,method ="Euclidean", diag = FALSE, upper = FALSE, pairwise = TRUE, by_rows = TRUE, convert_dismilarities = TRUE,auto_convert_data_frames = TRUE)
     euclidean[euclidean >= input$th]<-1
     euclidean[euclidean <= input$th]<-0
     sink("rstudsink.txt")
     print(apply(which(as.matrix(euclidean)==1,arr.ind=T),2, function(x) rownames(as.matrix(euclidean))[x]))
     sink()
     system("cat rstudsink.txt|awk -F\" \" 'NR!=1{print $2\",\"$3}'|sed -e 's/\" \"/,/g'>rstudsinked.txt")
     system("cat rstudsink.txt|awk -F\" \" 'NR!=1{print $2\",\"$3}'|sed -e 's/\"//g'>rstudsinked.txt")
     od <- read.csv("rstudsinked.txt",fill=T,header=F, sep=",", quote = '' )
     od[,1]=as.character(od[,1])
     od[,2]=as.character(od[,2])
     E(rg)$color <- "black"
     for( i in 1:length(readLines("rstudsinked.txt"))){ 
      rg <- rg+edge(od[,1][i-1],od[,2][i-1],color="red") }
     if(ecount(rg)<50)
     {
      plot(rg, layout=layout.auto, vertex.size=input$vsize, edge.width=input$esize, edge.arrow.size=0.1) 
     }
     else
     {
       plot(rg, layout=layout.auto, vertex.size=input$vsize, edge.width=input$esize, edge.arrow.size=0.1,vertex.label=NA)
     }
   })
  
  go <- eventReactive(input$action,{
    inFile <- input$file1
    rd <-read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    rg <- graph.data.frame(rd, directed=F)
    adj<-get.adjacency(rg)
    g1<-as.matrix(adj)
    euclidean<-simil(g1,method ="Euclidean", diag = FALSE, upper = FALSE, pairwise = TRUE, by_rows = TRUE, convert_dismilarities = TRUE,auto_convert_data_frames = TRUE)
    euclidean[euclidean >= input$th]<-1
    euclidean[euclidean <= input$th]<-0
    sink("rstudsink.txt")
    print(apply(which(as.matrix(euclidean)==1,arr.ind=T),2, function(x) rownames(as.matrix(euclidean))[x]))
    sink()
    system("cat rstudsink.txt|awk -F\" \" 'NR!=1{print $2\",\"$3}'|sed -e 's/\" \"/,/g'>rstudsinked.txt")
    system("cat rstudsink.txt|awk -F\" \" 'NR!=1{print $2\",\"$3}'|sed -e 's/\"//g'>rstudsinked.txt")
    od <- read.csv("rstudsinked.txt",fill=T,header=F, sep=",", quote = '' )
    od[,1]=as.character(od[,1])
    od[,2]=as.character(od[,2])
    E(rg)$color <- "black"
    for( i in 1:length(readLines("rstudsinked.txt"))){ 
      rg <- rg+edge(od[,1][i-1],od[,2][i-1],color="red") }
    tkplot(rg, layout=layout.auto, vertex.size=input$vsize, edge.width=input$esize, edge.arrow.size=0.1)
  })
  output$myPlot2 <- renderPlot({
    go()
  })
  
  
})

