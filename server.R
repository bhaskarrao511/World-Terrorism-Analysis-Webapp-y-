#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$mymap <- renderLeaflet({
    
    pal2 <- colorFactor(
      topo.colors(9), bigTerrorAttack$attacktype1_txt)
    
    
    leaflet(data) %>% 
      setView(lng = 29, lat = 7 , zoom = 2)  %>% #setting the view over ~ center of south sudan
      addTiles() %>% 
      addCircles(data = bigTerrorAttack[which(bigTerrorAttack$iyear == input$decade),], lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~nkill*1200, popup = ~summary,
                 color = ~pal2(attacktype1_txt), label = ~city, fillOpacity = 0.5) %>%
      addLegend("bottomright", pal = pal2, values = bigTerrorAttack$attacktype1_txt,
                title = "Terror Attack Types",
                opacity = 1)
  })
  
  output$globe <- renderGlobe({
    aggbigTerrorAttack2
  })
  
  
############################# For Analysis Panel ##################################
  output$countryHist <- renderPlotly({ 
    plot_ly(y=as.character(countryTerrorAttack$totalAttacks), x= as.character(countryTerrorAttack$country_txt), histfunc='sum', type = "histogram",source = "histCount") %>%
      layout(yaxis=list(type='linear'))})
  
  output$selection <- renderPrint({
    s <- event_data("plotly_click", source = "histCount")
    if (length(s) == 0) {
      
    } else {
      cat("You selected: \n\n")
      as.list(s)
    }
  })
  
  output$click_info <- renderPrint({
    s <- event_data("plotly_click", source = "lineGroup")
    if (length(s) == 0) {
      "Click on a cell in the line graph to display a scatterplot"
      
    } else {
      cat("You selected: \n\n")
      as.list(s)
      print(as.character(unique(dfyg$gname))[s[["curveNumber"]]+1])
    }
  })
  
  output$lineplotGroups <- renderPlotly({
    s <- event_data("plotly_click", source = "histCount")
    if (length(s)) {
      
      countTerrorGroupi <- countTerrorGroup[which(countTerrorGroup== s[["x"]]),]
      dfg <-  countTerrorGroupi %>% group_by(gname) %>% summarise(nevents = sum(totalAttacks)) %>% top_n(n=6) %>% ungroup() 
      
      dfg %>% filter(gname != "Unknown") -> fGroup
      dfyg <- countTerrorGroupi %>%  filter(gname %in% fGroup$gname) %>% group_by(iyear,gname) %>% summarise(nevents = sum(totalAttacks)) %>% ungroup() 
      dfyg <- as.data.frame(dfyg)
      plotlyLinedata <- reshape(dfyg,timevar="gname",idvar="iyear",direction="wide")
      names(plotlyLinedata)[-1]<- as.character(unique(dfyg$gname))
      plotlyLinedata[is.na(plotlyLinedata)]<-0
      
      
      #   colnames(dfyg) <-c("Year","Perpetrator_Group","Attack_Deaths")
      #  ggplot(data = dfyg, aes(x = Year, y = Attack_Deaths, colour = Perpetrator_Group)) +       
      #   geom_line() + geom_point() + theme_bw() + theme(legend.position=c(0.25,0.8))
      plot_ly(plotlyLinedata, x = plotlyLinedata$iyear, y = plotlyLinedata[2], name = colnames(plotlyLinedata[2]), type = 'scatter', mode = 'lines+markers',source = "lineGroup") %>%
        add_trace(y = plotlyLinedata[[3]], name = colnames(plotlyLinedata[3]), mode = 'lines+markers') %>%
        add_trace(y = plotlyLinedata[[4]], name = colnames(plotlyLinedata[4]), mode = 'lines+markers')%>%
        add_trace(y = plotlyLinedata[[5]], name = colnames(plotlyLinedata[5]), mode = 'lines+markers')%>%
        add_trace(y = plotlyLinedata[[6]], name = colnames(plotlyLinedata[6]), mode = 'lines+markers')%>%
        add_trace(y = plotlyLinedata[[7]], name = colnames(plotlyLinedata[7]), mode = 'lines+markers')%>%
      layout(title=paste("Top Terror Groups in",s[["x"]],sep=" "))
      
      
    }else{
      countTerrorGroupi <- countTerrorGroup[which(countTerrorGroup=="Iraq"),]
      countTerrorGroupi %>% group_by(gname) %>% summarise(nevents = sum(totalAttacks)) %>% top_n(n=6) %>% ungroup() -> dfg
      
      dfg %>% filter(gname != "Unknown") -> fGroup
      dfyg <- countTerrorGroupi %>%  filter(gname %in% fGroup$gname) %>% group_by(iyear,gname) %>% summarise(nevents = sum(totalAttacks)) %>% ungroup()
      dfyg <- as.data.frame(dfyg)
      plotlyLinedata <- reshape(dfyg,timevar="gname",idvar="iyear",direction="wide")
      names(plotlyLinedata)[-1]<- as.character(unique(dfyg$gname))
      plotlyLinedata[is.na(plotlyLinedata)]<-0
      
      plot_ly(plotlyLinedata, x = plotlyLinedata$iyear, y = plotlyLinedata[2], name = colnames(plotlyLinedata[2]), type = 'scatter', mode = 'lines+markers',source = "lineGroup") %>%
        add_trace(y = plotlyLinedata[[3]], name = colnames(plotlyLinedata[3]), mode = 'lines+markers') %>%
        add_trace(y = plotlyLinedata[[4]], name = colnames(plotlyLinedata[4]), mode = 'lines+markers')%>%
        add_trace(y = plotlyLinedata[[5]], name = colnames(plotlyLinedata[5]), mode = 'lines+markers')%>%
        add_trace(y = plotlyLinedata[[6]], name = colnames(plotlyLinedata[6]), mode = 'lines+markers')%>%
        add_trace(y = plotlyLinedata[[7]], name = colnames(plotlyLinedata[7]), mode = 'lines+markers')%>%
        layout(title= "Top Terror Groups in Iraq")
      
    }
    
    
    
    #   plot_ly(data, x = ~x, y = ~trace_0, name = 'trace 0', type = 'scatter', mode = 'lines') %>%
    #   add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers') %>%
    #   add_trace(y = ~trace_2, name = 'trace 2', mode = 'markers')
    
  })
  
  output$wordCloud <- renderPlot({
    sw <- event_data("plotly_click", source = "histCount")
    lc <- event_data("plotly_click", source = "lineGroup")
    #lc[["curveNumber"]]
    if (length(sw)) {
      # if(1==0){
      countTerrorGroupi <- countTerrorGroup[which(countTerrorGroup== sw[["x"]]),]
      dfg <-  countTerrorGroupi %>% group_by(gname) %>% summarise(nevents = sum(totalAttacks)) %>% top_n(n=6) %>% ungroup() 
      
      dfg %>% filter(gname != "Unknown") -> fGroup
      dfyg <- countTerrorGroupi %>%  filter(gname %in% fGroup$gname) %>% group_by(iyear,gname) %>% summarise(nevents = sum(totalAttacks)) %>% ungroup() 
      dfyg <- as.data.frame(dfyg)
      if(length(lc)){
        wordCloudTerrorGroup <- wordCloudTerrorGroupMain
        wordCloudTerrorGroup <- wordCloudTerrorGroup[which(wordCloudTerrorGroup$country_txt == sw[["x"]] ),]
        wordCloudTerrorGroup <- wordCloudTerrorGroup[which(wordCloudTerrorGroup$gname == as.character(unique(dfyg$gname))[lc[["curveNumber"]]+1] ),] #"Al-Qaida in Iraq"),] #
        wordCloudTerrorGroup %>% filter(!is.na(summary)) -> dfn0
        dfn0 %>% filter(summary != "") -> dfn
        text <- dfn$summary
        myCorpus <- Corpus(VectorSource(text))
        #myCorpus = tm_map(myCorpus, content_transformer(tolower))
        # remove punctuation
        myCorpus = tm_map(myCorpus, removePunctuation)
        myCorpus = tm_map(myCorpus, removeNumbers)
        myCorpus = tm_map(myCorpus, removeWords,c(stopwords("english"), stopwords("SMART"), "the"))
        #create DTM
        myDtm = TermDocumentMatrix(myCorpus,
                                   control = list(minWordLength = 3))
        #Frequent Terms and Associations
        freqTerms <- findFreqTerms(myDtm, lowfreq=1)
        m <- as.matrix(myDtm)
        # calculate the frequency of words
        v <- sort(rowSums(m), decreasing=TRUE)
        myNames <- names(v)
        d <- data.frame(word=myNames, freq=v)
        wordcloud(d$word, d$freq, min.freq=50, colors=brewer.pal(9,"Set1"))
      }
    }else{
      wordCloudTerrorGroup <- wordCloudTerrorGroupMain
      wordCloudTerrorGroup <- wordCloudTerrorGroup[which(wordCloudTerrorGroup$country_txt == "Iraq"),] #sw[["x"]] ),]
      wordCloudTerrorGroup <- wordCloudTerrorGroup[which(wordCloudTerrorGroup$gname == "Al-Qaida in Iraq"),] #as.character(unique(dfyg$gname))[p] ),]
      wordCloudTerrorGroup %>% filter(!is.na(summary)) -> dfn0
      dfn0 %>% filter(summary != "") -> dfn
      text <- dfn$summary
      myCorpus <- Corpus(VectorSource(text))
      #myCorpus = tm_map(myCorpus, content_transformer(tolower))
      # remove punctuation
      myCorpus = tm_map(myCorpus, removePunctuation)
      myCorpus = tm_map(myCorpus, removeNumbers)
      myCorpus = tm_map(myCorpus, removeWords,c(stopwords("english"), stopwords("SMART"), "the"))
      #create DTM
      myDtm = TermDocumentMatrix(myCorpus,
                                 control = list(minWordLength = 3))
      #Frequent Terms and Associations
      freqTerms <- findFreqTerms(myDtm, lowfreq=1)
      m <- as.matrix(myDtm)
      # calculate the frequency of words
      v <- sort(rowSums(m), decreasing=TRUE)
      myNames <- names(v)
      d <- data.frame(word=myNames, freq=v)
      wordcloud(d$word, d$freq, min.freq=50, colors=brewer.pal(9,"Set1"))
      
      
      
    }
    
  })
  
  output$sankeyChart <- renderPlotly({
    plot_ly(
      type = "sankey",
      orientation = "h",
      
      node = list(
        label = vectorNamesSankey,
        color = colSankey, #c("blue", "blue", "blue", "blue", "blue", "blue"),
        pad = 15,
        thickness = 20,
        line = list(
          color = "black",
          width = 0.5
        )
      ),
      
      link = list(
        source = DataFrameSankey$sourceIndex,
        target = DataFrameSankey$targetIndex,
        value =  DataFrameSankey$Value
      )
    ) %>% layout(
        title = "Region                               Target Attacked                            Attack Type                  Weapon Used",
        font = list(
          size = 10
        )
      )
  })
  
 
  
})
