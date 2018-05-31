
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
f<-1
t<-1
library(shiny)
library(shinydashboard)
#library(shinyRGL)
shinyUI(dashboardPage(
  dashboardHeader(title = "Grapho"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Graph Visualisation",tabName ="graph"),
      menuItem("Graph Properties", icon = icon("joomla"),
               #menuSubItem("  ", tabName ="hhh"),
               menuSubItem("Degree Centrality", tabName ="dc"),
               menuSubItem("Closeness Centrality", tabName="cc"),
               menuSubItem("Eigen Vector Centrality",tabName="ec"),
               menuSubItem("Betweenness Centrality",tabName="bc")
               ),
      #left shortest path betweeness centrality and 
      #current flow based betweeness centrality
      menuItem("Subgraph Detection",tabName="sub"),
     #menuItem(HTML("<p title='Uses Mallet'>Topic Detection </p>"),tabName ="topic"),
      menuItem(HTML("<p title='Uses Eigenvector Centrality'>Most Popular </p>"), tabName="pop"),
      #no space to mention dijkstra's for weighted and bellman ford for negative weights
      menuItem(HTML("<p title='Uses BFS for unweighted graphs'>Shortest Path </p>"),tabName="short"),
      menuItem(HTML("<p title='Uses high degree centrality'>Most Connected Node </p>"),tabName="con"),
     menuItem( icon = icon("list-ol"),"Basic functions", HTML("<p>(for numeric data)</p>"),
              menuSubItem("Highest vertex value",tabName="vr"),
              menuSubItem("Lowest vertex value",tabName="low"),
              menuSubItem("Standard deviation",tabName="stan"))
                      
     #left recommendation, logistics and finding abnormalities
      #menuItem(HTML("<p title='Uses rgl Plot'>3D Visualisation </p>"),tabName="con")
               
      
    )
    
    
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      column(width=8, 
       #box(
        # width = NULL, solidHeader = TRUE,
        
        #actionButton("do", "3D Visualisation") 
         
        
       #),
        tabItems(
          tabItem(tabName = "graph",
                  
                  imageOutput("gr")
                  ),
          
          tabItem(tabName = "dc",
                  #h2("dc")
                  imageOutput("deg")
          ),
          tabItem(tabName = "short",
                  #h2("dc")
                  imageOutput("sp"),
                  numericInput("f", "Starting Vertex:", f,
                               min = 1),
                  numericInput("t", "Ending Vertex:", t,
                                            min = 1),  
                
                  imageOutput("res")
          ),
          tabItem(tabName = "sub",
                  #h2("dc")
                  imageOutput("sd")
          )
        ,
        tabItem(tabName = "vr",
                #h2("dc")
                imageOutput("high")
        ),
        tabItem(tabName = "low",
                #h2("dc")
                imageOutput("min")
        ),
        tabItem(tabName = "stan",
                #h2("dc")
                imageOutput("std")
        )
 #      , webGLOutput("gr")
 , tabItem(tabName = "cc",
           
           imageOutput("close")
 ),
 
 tabItem(tabName = "ec",
         
         imageOutput("eig")
 ),
 tabItem(tabName = "bc",
         
         imageOutput("bet")
 )
 
 
                    
      )),
      column(width=4,
          tabBox(
            title = "Import",status="primary",
            # The id lets us use input$tabset1 on the server to find the current tab
            id = "tabset1", width = NULL,
            tabPanel("RDF",
                     radioButtons(inputId="rdftype", label = ".",
                                 choices=list("choose file"),
                                 selected=" "
                                 ),
            submitButton("Submit"))
           ,
            tabPanel("CSV",
            radioButtons(inputId="csvtype", label = "Type of input",
                         choices=list("Adjacency Matrix",
                           "Edge List",
                           "Adjacency List"),
                         selected=" "
                         ),
            submitButton("Submit"))
                ))
          
          #box(
           # title = "Subgraph", status = "primary", solidHeader = TRUE,
            #collapsible = TRUE,width="50px",id="sub"
            
          #)
      )
 )
 ))