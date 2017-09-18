library(shiny)
library(ggplot2)

precios <- read.csv("Precios.csv",header = TRUE)
fondos <- read.csv("Fondos.csv",header = TRUE)
namesfondos <- as.character(unique(fondos$Fondo))
namesprecios <- as.character(unique(precios$Instrumento))


fluidPage(
  
  tags$head(
    tags$style(HTML("
                    @import url('https://fonts.googleapis.com/css?family=Roboto+Slab');"),
                    '#sidebar{font-family:"Roboto Slab", serif;color: #fefefe;background-color:#16620a} 
                    body{background-color:#4fa22a')) ,
  
  headerPanel(h1("Simulador de operaciones", 
                 style = "font-family: 'Roboto Slab', serif;
                 font-weight: 500; line-height: 1.1; 
                 color: #fefefe;")),
  
  sidebarPanel(
    
    id="sidebar",
    
    selectInput('fondo', 'Fondos', namesfondos),
    
    #Venta de instrumentos
    uiOutput('venta'),
    bootstrapPage(
      div(style="display:inline-block",numericInput('montov','Monto', value = 0,min = 0)),
      div(style="display:inline-block",numericInput('titulosv','Títulos', value = 0, min = 0))),
    
    #Se debe cambiar de acuerdo al prospecto de cada fondo.
    uiOutput('compra'),
    div(style="display:inline-block",numericInput('montoc','Monto', value = 0,min = 0)),
    div(style="display:inline-block",numericInput('titulosc','Títulos', value = 0, min = 0))))
