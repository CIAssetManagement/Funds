library(shiny)
library(ggplot2)

precios <- read.csv("Precios.csv",header = TRUE)
fondos <- read.csv("Fondos.csv",header = TRUE)
namesfondos <- as.character(unique(fondos$Fondo))
namesprecios <- as.character(unique(precios$Instrumento))


fluidPage(
  
  titlePanel("Simulador de Operaciones"),
  
  sidebarPanel(
    
    selectInput('fondo', 'Fondos', namesfondos),
    uiOutput('snpselect'),
    #Se debe cambiar de acuerdo al prospecto de cada fondo.
    selectInput('instrumentoc', 'Compra Instrumento', namesprecios)
    
  )
)
