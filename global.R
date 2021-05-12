library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(shinydashboard)
library(shinyBS)
library(shinyalert)
library(shinydashboardPlus)
library(DT)
library(dplyr)
library(sodium)
library(here)
library(slickR)
#setwd("/Users/penelopeferron/Desktop/ShinyContest")


credentials = data.frame(
  username_id = c("manager"),
  passod   = sapply(c("manager"),password_store),
  permission  = c("manager"), 
  stringsAsFactors = F
)

app.infos1<-read.csv("03-Rapport/data_new_app.csv")


for(i in seq(ncol(app.infos1))){
  
  app.infos1[, i] <- as.character(app.infos1[, i])
  
}

cat<-unique(app.infos1$category)

#runApp(launch.browser = TRUE, host = "0.0.0.0", port = 1234)



