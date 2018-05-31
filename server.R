
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(igraph)
#library(shinyRGL)
library(rgl)
library(stringr)

#rdf<<-graph.data.frame(NULL,directed=FALSE)

shinyServer(function(input, output) {
  o <- observe({
    
    #importing rdf files
   # inFile <- input$file1
     inputrdftype=input$rdftype
     inputcsvtype=input$csvtype
    
    if ((is.null(inputrdftype)) && (is.null(inputcsvtype)))
      return(NULL)
    #else
    #{
     # if(FALSE)
    #  {  
    if(is.null((inputcsvtype)) || inputcsvtype=="")
     { 
     if(inputrdftype=="choose file")
      {
        rdffile=file.choose()
        newfile="test.nt"
        str=paste("java -jar rdf.jar ", rdffile, " ",newfile)
        system(str)
        str2=paste("python nt2csv.py ",newfile)
        system(str2)
        #library(igraph)
        # read the converted csv file
        dat=read.csv("test.csv",header=FALSE) #problem
        rdf=graph.data.frame(dat,directed=TRUE)
        inputrdftype<-NULL
      }
     }   
    #  }  
      
    
   # if(is.null(input$csvtype))
    #{return(NULL)}
    
      #csv in the file of adjacency matrix
    else if(is.null(inputrdftype))
    {
     if (inputcsvtype=="Adjacency Matrix")
      {
        # choose an adjacency matrix from a .csv file
        dat=read.csv(file.choose(),header=TRUE,row.names=1,check.names=FALSE)
        #gd<-graph.data.frame(dat)
        m=as.matrix(dat)
        rdf=graph.adjacency(m,mode="undirected",weighted=NULL)
        #output$text1 <- renderText("gc")
      }
      else if(inputcsvtype=="Edge List")
      {
        dat=read.csv(file.choose(),header=TRUE) # choose an edgelist in .csv file format
        #g<-graph.data.frame(dat)
        el=as.matrix(dat) # coerces the data into a two-column matrix format that igraph likes
        el[,1]=as.character(el[,1])
        el[,2]=as.character(el[,2])
        rdf=graph.edgelist(el,directed=FALSE) # turns the edgelist into a 'graph object'
        inputcsvtype<-NULL
        #output$text1 <- renderText("gd")
      }
      else if(inputcsvtype=="Adjacency List")
      {
        lines=scan(file.choose(),what="character",sep="\n",skip=1) # read the csv file (skipping the header), line-by-line as character string.
        lines=gsub(","," ",lines) # replace commas with spaces
        lines=gsub("[ ]+$","",gsub("[ ]+"," ",lines)) # remove trailing and multiple spaces.
        adjlist=strsplit(lines," ") # splits the character strings into list with different vector for each line
        col1=unlist(lapply(adjlist,function(x) rep(x[1],length(x)-1))) # establish first column of edgelist by replicating the 1st element (=ID number) by the length of the line minus 1 (itself)
        col2=unlist(lapply(adjlist,"[",-1)) # the second line I actually don't fully understand this command, but it takes the rest of the ID numbers in the character string and transposes it to list vertically
        el=cbind(col1,col2) # creates the edgelist by combining column 1 and 2.
        
        el[,1]=as.character(el[,1])
        el[,2]=as.character(el[,2])
        rdf=graph.edgelist(el,directed=FALSE) # turns the edgelist into a 'graph object'
        input$csvtype<-NULL
        #output$text1 <- renderText("ge")
      }
    }
     rdf2=rdf
     rdf3=rdf
    # subgraph detection
      ebc <- edge.betweenness.community(rdf, directed=F)
      mods <- sapply(0:ecount(rdf), function(i){
        g2 <- delete.edges(rdf, ebc$removed.edges[seq(length=i)])
        cl <- clusters(g2)$membership
        modularity(rdf,cl)
      })
      #  plot(mods, pch=20)
      g2<-delete.edges(rdf, ebc$removed.edges[seq(length=which.max(mods)-1)])
      output$sd<-renderPlot({V(rdf)$color=clusters(g2)$membership
      rdf$layout <- layout.fruchterman.reingold
      plot(rdf, vertex.label=NA) })
    
    output$gr <- renderPlot({  plot(rdf,				#the graph to be plotted
                                    layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
                                    main='Graph',	#specifies the title
                                    vertex.label.dist=1,			#puts the name labels slightly off the dots
                                    vertex.frame.color='blue', 		#the color of the border of the dots 
                                    vertex.label.color='purple',		#the color of the name labels
                                    vertex.label.font=1,			#the font of the name labels
                                    vertex.label=V(rdf)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
                                    vertex.label.cex=1			#specifies the size of the font of the labels. can also be made to vary)
    )
    })
    
    three<- function() {rglplot(rdf, layout=coords)}
    rglplot(rdf)
    observeEvent(input$do, {
      library(rgl)
      rglplot(rdf)})
    
    
    #output$gr <- renderWebGL({rglplot(g)})
    
    output$deg <- renderPlot({  plot(rdf,				#the graph to be plotted
                                     layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
                                     main='The values in the vertices represents its \n Degree Centrality',	#specifies the title
                                     vertex.label.dist=2,			#puts the name labels slightly off the dots
                                     vertex.frame.color='blue', 		#the color of the border of the dots 
                                     vertex.label.color='blue',		#the color of the name labels
                                     vertex.label.font=1,			#the font of the name labels
                                     vertex.label=centralization.degree(rdf)$res,		#specifies the lables of the vertices. in this case the 'name' attribute is used
                                     vertex.label.cex=1			#specifies the size of the font of the labels. can also be made to vary)
    )
    })
    output$bet <- renderPlot({  plot(rdf,				#the graph to be plotted
                                     layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
                                     main='The values in the vertices represents its \n Betweenness Centrality',	#specifies the title
                                     vertex.label.dist=2,			#puts the name labels slightly off the dots
                                     vertex.frame.color='blue', 		#the color of the border of the dots 
                                     vertex.label.color='blue',		#the color of the name labels
                                     vertex.label.font=1,			#the font of the name labels
                                     vertex.label=centralization.betweenness(rdf)$res,		#specifies the lables of the vertices. in this case the 'name' attribute is used
                                     vertex.label.cex=1			#specifies the size of the font of the labels. can also be made to vary)
    )
    })
    output$close <- renderPlot({  plot(rdf,				#the graph to be plotted
                                       layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
                                       main='The values in the vertices represents its \n Closeness Centrality',	#specifies the title
                                       vertex.label.dist=1,			#puts the name labels slightly off the dots
                                       vertex.frame.color='blue', 		#the color of the border of the dots 
                                       vertex.label.color='blue',		#the color of the name labels
                                       vertex.label.font=1,			#the font of the name labels
                                       vertex.label=round(centralization.closeness(rdf)$res,digits=3),		#specifies the lables of the vertices. in this case the 'name' attribute is used
                                       vertex.label.cex=1			#specifies the size of the font of the labels. can also be made to vary)
    )
    })
    output$eig <- renderPlot({  plot(rdf,				#the graph to be plotted
                                     layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
                                     main='The values in the vertices represents its \n Eigenvector Centrality',	#specifies the title
                                     vertex.label.dist=1,			#puts the name labels slightly off the dots
                                     vertex.frame.color='blue', 		#the color of the border of the dots 
                                     vertex.label.color='blue',		#the color of the name labels
                                     vertex.label.font=1,			#the font of the name labels
                                     vertex.label=centralization.evcent(rdf)$res,		#specifies the lables of the vertices. in this case the 'name' attribute is used
                                     vertex.label.cex=1			#specifies the size of the font of the labels. can also be made to vary)
    )
    })
    #Shortest Path
    pa <- get.shortest.paths(rdf, 1,5)[[1]]
    
    output$sp <- renderPlot({V(rdf)[pa[[1]]]$color <- 'green'
    E(rdf)$color <- "SkyBlue2"
    E(rdf, path=pa[[1]])$color <- "red"
    plot(rdf,vertex.label.dist=1)})
    ########################################################
    #highest edge weight
    # g1<-as_edgelist(rdf)
    a<-delete.vertices(rdf,c(1))
    g4<-get.edgelist(a)
    #g<-graph(g4, n=max(g4), directed=TRUE)
    h<-max(g4)
    #h2<-vcount(h1)
    V(rdf2)[h]$color="red"
    #E(rdf)[h]$color="red"
    output$high <- renderPlot({  plot(rdf2,				#the graph to be plotted
                                      layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
                                      main='The highest vertices value',	#specifies the title
                                      vertex.label.dist=1,			#puts the name labels slightly off the dots
                                      vertex.frame.color='blue', 		#the color of the border of the dots 
                                      vertex.label.color='blue',		#the color of the name labels
                                      vertex.label.font=1,			#the font of the name labels
                                      # vertex.label=centralization.evcent(rdf)$res,		#specifies the lables of the vertices. in this case the 'name' attribute is used
                                      vertex.label.cex=1			#specifies the size of the font of the labels. can also be made to vary)
    )
    })
    
    
    
    ########################################################
    #Lowest vertex value
    
    a<-delete.vertices(rdf,c(1))
    g4<-get.edgelist(a)
    
    h<-min(g4)
    #h2<-vcount(h1)
    V(rdf3)[h]$color="red"
    #E(rdf)[h]$color="red"
    output$min <- renderPlot({  plot(rdf3,				#the graph to be plotted
                                     layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
                                     main='The lowest vertices value',	#specifies the title
                                     vertex.label.dist=1,			#puts the name labels slightly off the dots
                                     vertex.frame.color='blue', 		#the color of the border of the dots 
                                     vertex.label.color='blue',		#the color of the name labels
                                     vertex.label.font=1,			#the font of the name labels
                                     # vertex.label=centralization.evcent(rdf)$res,		#specifies the lables of the vertices. in this case the 'name' attribute is used
                                     vertex.label.cex=1			#specifies the size of the font of the labels. can also be made to vary)
    )
    })
    ######################################################
    #Standard deviation
    a<-delete.vertices(rdf,c(1))
    g4<-get.edgelist(a)
    std<-sd(g4)
    output$std <- renderPlot({  plot(rdf,				#the graph to be plotted
                                     layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
                                     main=std,
                                     #mainPanel('The standard deviation of the grapg is')	,#specifies the title
                                     # main=sd(g4),
                                     vertex.label.dist=1,			#puts the name labels slightly off the dots
                                     vertex.frame.color='blue', 		#the color of the border of the dots 
                                     vertex.label.color='blue',		#the color of the name labels
                                     vertex.label.font=1,			#the font of the name labels
                                     # vertex.label=centralization.evcent(rdf)$res,		#specifies the lables of the vertices. in this case the 'name' attribute is used
                                     vertex.label.cex=1			#specifies the size of the font of the labels. can also be made to vary)
    )
    })########################################################
    #highest edge weight
    # g1<-as_edgelist(rdf)
    a<-delete.vertices(rdf,c(1))
    g4<-get.edgelist(a)
    #g<-graph(g4, n=max(g4), directed=TRUE)
    h<-max(g4)
    #h2<-vcount(h1)
    V(rdf2)[h]$color="red"
    #E(rdf)[h]$color="red"
    output$high <- renderPlot({  plot(rdf2,				#the graph to be plotted
                                      layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
                                      main='The highest vertices value',	#specifies the title
                                      vertex.label.dist=1,			#puts the name labels slightly off the dots
                                      vertex.frame.color='blue', 		#the color of the border of the dots 
                                      vertex.label.color='blue',		#the color of the name labels
                                      vertex.label.font=1,			#the font of the name labels
                                      # vertex.label=centralization.evcent(rdf)$res,		#specifies the lables of the vertices. in this case the 'name' attribute is used
                                      vertex.label.cex=1			#specifies the size of the font of the labels. can also be made to vary)
    )
    })
    
    
    
    ########################################################
    #Lowest vertex value
    
    a<-delete.vertices(rdf,c(1))
    g4<-get.edgelist(a)
    
    h<-min(g4)
    #h2<-vcount(h1)
    V(rdf3)[h]$color="red"
    #E(rdf)[h]$color="red"
    output$min <- renderPlot({  plot(rdf3,				#the graph to be plotted
                                     layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
                                     main='The lowest vertices value',	#specifies the title
                                     vertex.label.dist=1,			#puts the name labels slightly off the dots
                                     vertex.frame.color='blue', 		#the color of the border of the dots 
                                     vertex.label.color='blue',		#the color of the name labels
                                     vertex.label.font=1,			#the font of the name labels
                                     # vertex.label=centralization.evcent(rdf)$res,		#specifies the lables of the vertices. in this case the 'name' attribute is used
                                     vertex.label.cex=1			#specifies the size of the font of the labels. can also be made to vary)
    )
    })
    ######################################################
    #Standard deviation
    a<-delete.vertices(rdf,c(1))
    g4<-get.edgelist(a)
    std<-sd(g4)
    
    output$std <- renderPlot({  plot(rdf,				#the graph to be plotted
                                     layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
                                     main=std,
                                     #mainPanel('The standard deviation of the grapg is')	,#specifies the title
                                     # main=sd(g4),
                                     vertex.label.dist=1,			#puts the name labels slightly off the dots
                                     vertex.frame.color='blue', 		#the color of the border of the dots 
                                     vertex.label.color='blue',		#the color of the name labels
                                     vertex.label.font=1,			#the font of the name labels
                                     # vertex.label=centralization.evcent(rdf)$res,		#specifies the lables of the vertices. in this case the 'name' attribute is used
                                     vertex.label.cex=1			#specifies the size of the font of the labels. can also be made to vary)
    )
    })

 
  #Shortest Path
 r<-  reactive({
  # output$sp 
   pa <- get.shortest.paths(rdf, input$f,input$t)[[1]]
  
  output$res <- renderPlot({V(rdf)[pa[[1]]]$color <- 'green'
  E(rdf)$color <- "SkyBlue2"
  E(rdf, path=pa[[1]])$color <- "red"
  tkplot(rdf,vertex.label.dist=1)}) })
  })
})



    
