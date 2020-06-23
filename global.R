library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(shinycssloaders)
library(shinyWidgets)

#setwd("/home/alicja.wolny-dominiak/covid")

##main function
covidExp <- function(data, m, cutL, cutU, date, startVal){
  # cuttL <- which(data$report == cutL)
  # cuttU <- which(data$report == cutU)
  
  cuttL <- data$report[which(data$d == as.Date(cutL))]
  cuttU <- data$report[which(data$d == as.Date(cutU))]
  
  
  dataCut <- subset(data,  report %in% cuttL:cuttU)
  
  # fit non-linear model y = a* e^(b*x)
  modE <- nls(dataCut$all ~ a * exp(b*dataCut$report), data = dataCut, start = list(a = startVal, b = 0.1))
  a <- summary(modE)$parameters[1]
  b <- summary(modE)$parameters[2]
  
  #fitted values
  modEFit <- round(a * exp(b*dataCut$report))
  error <- modEFit - dataCut$all
  rmse <- sqrt(sum(error^2))
  
  dfFitted <- data.frame(dataCut, allFitted = modEFit)
  
  #predict 
  x <- 1:m + dataCut$report[length(dataCut$report)]
  modEPr <- round(a * exp(b*x)) 
  dfPred <- data.frame(report = x, all = modEPr)
  dfPred$d <- subset(date, report %in% x)$d
  
  return(list(modE = modE, dfPred = dfPred, dfFitted = dfFitted, rsme = rmse, a = a, b = b, error = error, 
              data = data, dataCut = dataCut))  
}

###Date
datee <<- data.frame(report = 1:100, d = seq( as.Date("2020-01-21"), by=1, len = 100))

##WHO data
dfcovv <- read.csv2('http://web.ue.katowice.pl/woali/Rdane/shinyCovid/data.csv')
dfcovv$d <- subset(datee, report %in% dfcovv$report)$d

