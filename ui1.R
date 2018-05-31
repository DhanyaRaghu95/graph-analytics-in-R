
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

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
               menuSubItem("Betweenness Centrality",tabName="bc"),
               menuSubItem("subgraph for rdf",tabName="subRDF"),
               menuSubItem("Subgraph Detection",tabName="sub")),
      menuItem(HTML("<p title='Uses Eigenvector Centrality'>Most Popular </p>"), tabName="pop"),
      #no space to mention dijkstra's for weighted and bellman ford for negative weights
      menuItem(HTML("<p title='Uses BFS for unweighted graphs'>Shortest Path </p>"),tabName="short"),
      menuItem(HTML("<p title='Uses high degree centrality'>Most Connected Node </p>"),tabName="con")
      
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
                       imageOutput("sp")
               ),
               tabItem(tabName = "sub",
                       #h2("dc")
                       imageOutput("sd")
               ),
               tabItem(tabName = "subRDF",
                       #h2("dc")
                       imageOutput("sdrdf")
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
  #    column(width=4,
   
            tabBox(
               title = "Import",status="primary",
              #  The id lets us use input$tabset1 on the server to find the current tab
               id = "tabset1", width = NULL,
              # tabPanel("RDF", 
               #                         )
            
                      submitButton("Choose"))
            tabPanel("RDF", radioButtons(inputId="rdf", label = "Choose File",
                          choices=list("Choose File"
                          ),
                          selected="Choose File"
             ),
             submitButton("Submit")))
      
    
      )
               ) 
               )
             )
             
             #     box(
             #      title = "Subgraph", status = "primary", solidHeader = TRUE,
             #     collapsible = TRUE,width="50px",id="sub"
             
     )

