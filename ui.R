#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(shinyGlobe)
library(networkD3)
library(plotly)
library(leaflet)
library(markdown)
library(rjson)
library(wordcloud)
library(tm)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
 
   tags$head(
    tags$style(HTML("
                    
                    /* Work around CSS properties introduced on img by bootstrap */
                    
                    img.leaflet-tile {
                    padding: 0;
                    margin: 0;
                    border-radius: 0;
                    border: none;
                    }
                    .info {
                    padding: 2px 2px;
                    font: 8px/9px Arial, Helvetica, sans-serif;
                    background: white;
                    background: rgba(255,255,255,0.8);
                    box-shadow: 0 0 15px rgba(0,0,0,0.2);
                    border-radius: 4px;
                    }
                    .legend {
                    line-height: 18px;
                    color: #555;
                    }
                    .legend svg text {
                    fill: #555;
                    }
                    .legend svg line {
                    stroke: #555;
                    }
                    .legend i {
                    width: 9px;
                    height: 9px;
                    margin-right: 4px;
                    opacity: 0.7;
                    display: inline-block;
                    vertical-align: top;
                    /*For IE 7*/
                    zoom: 1;
                    *display: inline;
                    }

                    #globe > canvas{
                    margin-left: 202px;
                    }
                    
                   /* #wordCloud > img
                    {position: relative;
                    left: 362px;} */
                   
                    #countryHist{
                    margin-left: 227px;
                    }
                    
                  #lineplotGroups{
                    margin-left: 234px;
                    }
                  
                    #wordCloud{margin-left: 141px;}
                      
                    #sankeyChart{margin-left: 240px;}
                    "))
    
  ),
  
   h1("Global", span("Terrorism Analysis", style = "font-weight: 300"), 
     style = "font-family: 'Source Sans Pro';
     color: #fff; text-align: center;
     background-image: url('texturebg.png');
     padding: 20px"),
  
   # Sidebar with a slider input for number of bins 
  
  
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Global Overview",  
                           br(),
                           h4("Loss Of Life Worldwide From Terrorist Attacks",style = "margin-left: 545px;font-weight:bold;"),
                           p("The worldmap visualises the terror attacks around the world for the last decade, all the terror attacks have been color coded to represent the attack type and their size corresponds to the deaths. Please click on the attack locations to get an summary of the specific attack. The timeline below the map can be played to animate the map  ",
                             style =" margin-left: 302px; height: 61px; width: 857px;"),
                           fluidRow(
                             column(width=12,offset = 3,
                           leafletOutput(outputId = "mymap"))), 
                          # br(),
                           fluidRow(
                             column(width=12,offset = 7,
                                           sliderInput("decade", "Decade",
                                                       min = min(bigTerrorAttack$iyear), max = max(bigTerrorAttack$iyear),
                                                       value = min(bigTerrorAttack$iyear),step = 1, animate = TRUE)
                             )
                           ),hr(),
                          h4("Aggregated Fatalaties Around The Globe for Years 2007 to 2017",style = "margin-left: 418px;;font-weight:bold;"),
                          p("The terror attacks between years 2007 and 2017 are represented by vertical bars on the 3D globe, the color and length of the bars represents the aggregated fatalities due to terror attack at the location. We can see the Middle East, South Asia and Northern African regions are the most affected parts of the world.",
                            style =" margin-left: 302px; height: 61px; width: 857px;"),
                          
                          fluidRow( offset = 7,globeOutput("globe"))
                           
                           ),
                  tabPanel("Terror Groups Analysis",
                           br(),
                           h4("Top 15 Countries Most Affected By Terrorism ",style = "margin-left: 545px;font-weight:bold;"),
                           p("The Histogram visualises the worlds mosts terrorism affected countries, quantified by the number of deaths due to terror attack in the last 10 years (2007 - 2017). Y axis : Number of terror related deaths, X axis: Countries. It can be seen that almost all the countries are South Asian or Middle Eastern. This is an interactive visualization and is linked to the next visualization, the line plot. The country can be selected by clicking on the corresponding bar, the line chart below will plot for the terror groups affecting the selected country.",
                             style =" margin-left: 302px; height: 71px; width: 857px;"),
                            plotlyOutput("countryHist"),
                           hr(),
                           h4("Top 6 Terror Groups Terrorizing The Country",style = "margin-left: 545px;font-weight:bold;"),
                           p("The line plot shows the trend of fatalities caused by the top 6 terror groups over the last 10 years in the specific country. Y Axis : Fatalities, X Axis : Years. The plot shows the trend for the country selected in the histogram above. This chart is linked to the word cloud below. To get the word cloud of summaries of all the attacks conducted by a specific terror group over the last 10 years, please click on a point on the trend of the specific terror group. This will lead to selection of the terror group and the generation of word cloud associated with it.",
                             style =" margin-left: 302px; height: 93px; width: 857px;"),
                           p("Note: You need to select a points on the line plot corresponding to terror groups to generate its word cloud. Please click on the point only when you see the hovertext with group's name",
                                  style = "margin-left: 302px; height: 90px; width: 857px;font-weight: bold;" ),
                           plotlyOutput("lineplotGroups"),
                          hr(),
                          h4("Group Terror Attack Summary Analysis - Word Cloud",style = "margin-left: 480px;font-weight:bold;"),
                          p("The word cloud shows the most frequently mentioned words in the summary of the terror attacks undertaken by the terror groups over the last 10 years in the specific country. The terror group is the one selected in the line plot above. Word cloud might take some time to populate",
                            style = "margin-left: 302px; height: 90px; width: 857px;"),
                         
                          br(),
                          plotOutput("wordCloud")
                           # fluidRow(column(width = 7,plotlyOutput("lineplotGroups")),
                           #          column(width = 5,plotOutput("wordCloud"))
                           # )
                           
                  ), # detailed analysis
                  tabPanel("Terror Attack Analysis",
                           br(),
                           h4("Terror Attack Analysis - Sankey Chart",style = "margin-left: 570px;font-weight:bold;"),
                           p("The sankey chart explores the relationship between the global regions and the terror attack events. The sankey chart is used explore the relationships between terror attack target, attack type and weapons used. It can be seen that most preferred weapons were explosives and firearms. The attackers were more likely to attack citizens and military establishments and would engage in bombing and armed assults. ",
                             style =" margin-left: 302px; height: 50px; width: 857px;"),
                           br(),
                           plotlyOutput("sankeyChart")
                  ) # Sankey Analysis
                  
                  
                 
      
       
       )#tabsetPanel
        ) # main panel
  
)# fluid page
)# shiny
