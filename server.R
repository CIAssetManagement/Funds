library(shiny)
library(ggplot2)
library(DT)

function(input, output) {
  
  precios <- read.csv("Precios.csv",header = TRUE)
  precios <- cbind(precios,Tipo = sub("-.*","",precios$Instrumento))
  fondos <- read.csv("Fondos.csv",header = TRUE)
  
  dataset <- reactive({
    precios
  })
  
  #Instrumentos que se pueden vender.
  output$venta <- renderUI({
    selected_value <- input$fondo
    selectizeInput('instrumentov', 'Venta de Instrumento', instrumentoventa(fondos,selected_value))
  })
  
  #Instrumentos que se pueden comprar.
  output$compra <- renderUI({
    selected_value <- input$fondo
    selectizeInput('instrumentoc', 'Compra de Instrumento', instrumentocompra(precios,selected_value))
  })
  
  #eventReactive()
  
  output$prueba = DT::renderDataTable({fondos})
}

