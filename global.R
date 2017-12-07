library(rdrop2)
library(shiny)
library(DT)
library(dplyr)
library(RMySQL)
library(scales)
library(shinyjs)
library(tidyr)

# Matando la notación científica
options(scipen = 999)

#Cargando desde Dropbox
token <-readRDS("droptoken.rds")
drop_acc(dtoken = token)

#Fondos
source("funciones.R",local=FALSE)

folder_address <- "//192.168.0.223/CIFONDOS/Funds"
runApp(appDir = folder_address,launch.browser = TRUE)