library(dplyr)
library(shinyGlobe)
library(networkD3)
library(plotly)
library(leaflet)
library(markdown)
library(rjson)
library(wordcloud)
library(tm)

################### Read Me ##############

## please note, you would need all the above libraries to execute this code.
# the required libraries are "shiny","dplyr","shinyGlobe","networkD3","plotly","leaflet","markdown",rjson",wordcloud","tm" and "devtools"

# these are the specific libraries which i have taken from Github, use the following command to install them (after removing comments)
# devtools::install_github("trestletech/shinyGlobe")


### execute the code after changing the working directory to the location of app
################### Read Me end ###############
#install.packages("flipPlots")
#devtools::install_github("Displayr/flipPlots")
print(getwd())
#terrorDatax <- read.csv("D:\\Study\\Masters\\Data Visualization\\Assignment4\\gtd\\globalterrorismdb_0718dist.csv")
#terrorData10 <- terrorDatax[which(terrorDatax$iyear>2006),c("eventid","iyear","latitude","longitude","summary","nkill","city","targtype1_txt","attacktype1_txt","weaptype1_txt","region_txt","country_txt","gname"  )]
#write.csv(terrorData10,"data/terrorAttackFiltered.csv")
terrorData10 <- read.csv("data/terrorAttackFiltered.csv")
bigTerrorAttack <- terrorData10[which(terrorData10$nkill > 2) ,]
aggbigTerrorAttack <- bigTerrorAttack %>% group_by(latitude,longitude) %>% summarise(totalDeaths = sum(nkill, na.rm=TRUE))
colnames(aggbigTerrorAttack) <- c("Latitude","Longitude","Population")
aggbigTerrorAttack2 = as.data.frame(aggbigTerrorAttack[which(aggbigTerrorAttack$Population >3),])
aggbigTerrorAttack2$Population = aggbigTerrorAttack2$Population/max(aggbigTerrorAttack2$Population)
aggbigTerrorAttack2 <- (aggbigTerrorAttack2[which(is.na(aggbigTerrorAttack2$Latitude)==FALSE),])

.global <- new.env()

initResourcePaths <- function() {
  if (is.null(.global$loaded)) {
    shiny::addResourcePath(
      prefix = 'shinyGlobe',
      directoryPath = system.file('www', package = 'shinyGlobe'))
    .global$loaded <- TRUE
  }
  HTML("")
}

globeOutput <- function(outputId){
  tagList(
    singleton(tags$head(
      initResourcePaths(),
      tags$script(src = 'shinyGlobe/third-party/three.min.js', type = "text/javascript"),
      tags$script(src = 'shinyGlobe/third-party/Detector.js', type = "text/javascript"),
      tags$script(src = 'shinyGlobe/third-party/Tween.js', type = "text/javascript"),
      tags$script(src = 'shinyGlobe/globe.js', type = "text/javascript"),
      tags$script(src = 'shinyGlobe/shinyGlobe.js', type = "text/javascript")#,
      # includeCSS(system.file("/www/style.css", package = "shinyGlobe"))
    )),
    div(id = outputId, class = "shiny-globe-output") 
  )
}

#### Sankey chart diagram



allTerrorData10 <- terrorData10[which(terrorData10$iyear>2006),]
regionVector <- as.character(unique(allTerrorData10$region_txt))

targetVector <- as.character(unique(allTerrorData10$targtype1_txt))
targetVector <- replace(targetVector, targetVector=="Unknown", "Unknown Targets")
targetVector <- replace(targetVector, targetVector=="Other", "Other Targets")

attackTypeVector <- as.character(unique(allTerrorData10$attacktype1_txt))
attackTypeVector <- replace(attackTypeVector, attackTypeVector=="Unknown", "Unknown Attack")

weaponTypeVector <- as.character(unique(allTerrorData10$weaptype1_txt))
weaponTypeVector <- replace(weaponTypeVector, weaponTypeVector=="Unknown", "Unknown Weapons")
weaponTypeVector <- replace(weaponTypeVector, weaponTypeVector=="Other", "Other Weapons")

vectorNamesSankey <- append(append(append(regionVector,targetVector),attackTypeVector),weaponTypeVector)
indexNamesSankey <- c(1:length(vectorNamesSankey)-1)
dfNamesSankey <- data.frame(indexNamesSankey,vectorNamesSankey)

#region and target sankey
regionTarget <- allTerrorData10 %>% group_by(region_txt,targtype1_txt) %>% summarise(totalDeaths = sum(nkill, na.rm=TRUE))
regionTarget <- as.data.frame(regionTarget[which(regionTarget$totalDeaths>10),])
regionTarget$region_txt <- as.character(regionTarget$region_txt)
regionTarget$targtype1_txt <- as.character(regionTarget$targtype1_txt)
regionTarget$targtype1_txt[regionTarget$targtype1_txt=="Unknown"] = "Unknown Targets"
regionTarget$targtype1_txt[regionTarget$targtype1_txt=="Other"] = "Other Targets"
colnames(regionTarget) <- c("Source","Target","Value")

# target and attack type df
targetAttack <-  allTerrorData10 %>% group_by(targtype1_txt,attacktype1_txt) %>% summarise(totalDeaths = sum(nkill, na.rm=TRUE))
targetAttack <- as.data.frame(targetAttack[which(targetAttack$totalDeaths>10),])
targetAttack$targtype1_txt <- as.character(targetAttack$targtype1_txt)
targetAttack$attacktype1_txt <- as.character(targetAttack$attacktype1_txt)
targetAttack$targtype1_txt[targetAttack$targtype1_txt=="Unknown"] = "Unknown Targets"
targetAttack$targtype1_txt[targetAttack$targtype1_txt=="Other"] = "Other Targets"
targetAttack$attacktype1_txt[targetAttack$attacktype1_txt == "Unknown"] = "Unknown Attack"
colnames(targetAttack) <- c("Source","Target","Value")



# attack type and weapon type df
attackWeapon <-  allTerrorData10 %>% group_by(attacktype1_txt,weaptype1_txt) %>% summarise(totalDeaths = sum(nkill, na.rm=TRUE))
attackWeapon <- as.data.frame(attackWeapon[which(attackWeapon$totalDeaths>10),])
attackWeapon$attacktype1_txt <- as.character(attackWeapon$attacktype1_txt)
attackWeapon$weaptype1_txt <- as.character(attackWeapon$weaptype1_txt)
attackWeapon$attacktype1_txt[attackWeapon$attacktype1_txt == "Unknown"] = "Unknown Attack"
unique(attackWeapon$weaptype1_txt)
attackWeapon$weaptype1_txt[attackWeapon$weaptype1_txt == "Unknown"] = "Unknown Weapons"
attackWeapon$weaptype1_txt[attackWeapon$weaptype1_txt == "Other"] = "Other Weapons"
colnames(attackWeapon) <- c("Source","Target","Value")

DataFrameSankey <- rbind(rbind(regionTarget,targetAttack),attackWeapon)

for(i in 1:nrow(DataFrameSankey)){
  DataFrameSankey$sourceIndex[i] = dfNamesSankey$indexNamesSankey[which(dfNamesSankey$vectorNamesSankey == as.character(DataFrameSankey$Source[i]))]
  DataFrameSankey$targetIndex[i] = dfNamesSankey$indexNamesSankey[which(dfNamesSankey$vectorNamesSankey == as.character(DataFrameSankey$Target[i]))]
  print(i)
}


# for color in sanky chart
colorAll = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
colSankey =sample(colorAll, 55)
#pie(rep(1,55), col=sample(color, 55))


####### for plotly histogram

#getting the top 10 countries with most number of terror attacks

countryTerrorAttack <- allTerrorData10 %>% group_by(country_txt) %>% summarise(totalAttacks = n())
countryTerrorAttack <- countryTerrorAttack[order(-countryTerrorAttack$totalAttacks),]
countryTerrorAttack <- head(countryTerrorAttack,15)

# p <- plot_ly(y=as.character(countryTerrorAttack$totalAttacks), x= as.character(countryTerrorAttack$country_txt), histfunc='sum', type = "histogram") %>%
#   layout(yaxis=list(type='linear'))
# p


########### for plotly line chart
countTerrorGroup <- allTerrorData10 %>% group_by(country_txt,iyear,gname) %>% summarise(totalAttacks = sum(nkill))
countTerrorGroup <- as.data.frame(countTerrorGroup[which(countTerrorGroup$country_txt %in% as.character(unique(countryTerrorAttack$country_txt))),])

######### for word cloud
wordCloudTerrorGroupMain <- allTerrorData10[which(allTerrorData10$country_txt %in% as.character(unique(countryTerrorAttack$country_txt))),c("country_txt","iyear","gname","summary")]
wordCloudTerrorGroupMain <- wordCloudTerrorGroupMain %>% filter(gname != "Unknown")

groupName <- "Asa'ib Ahl al-Haqq"
