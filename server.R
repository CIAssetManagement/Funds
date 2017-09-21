library(shiny)
library(ggplot2)
library(DT)

#Bases de datos
precios <- read.csv("Precios.csv",header = TRUE)
precios <- cbind(precios,Tipo = sub("-.*","",precios$Instrumento))
fondos <- read.csv("Fondos.csv",header = TRUE)
#Tener cuidado si existe un tipo de valor z
mercados <- read.csv("mercados.csv",header=TRUE,stringsAsFactors = FALSE)

#Lista de instrumentos que los fondos pueden vender.
instrumentoventa <- function(fondos,nombre){
  valores <- unique(fondos$Emisora[which(fondos$Fondo == nombre)])
  return(valores)
}

#Lista de instrumentos que los fondos pueden comprar.
instrumentocompra <- function(precios,nombre, mercados){
  valores <- switch(nombre,
                    "+CIGUB"={precios$Instrumento[precios$Tipo %in% mercados$deudagub]},
                    "+CIGUMP"={precios$Instrumento[precios$Tipo %in% mercados$deudagub]},
                    "+CIGULP"={precios$Instrumento[precios$Tipo %in% mercados$deudagub]},
                    "+CIPLUS"={},
                    "+CIBOLS"={},
                    "+CIUSD"={},
                    "+CIEQUS"={}
  )
  valores <- unique(valores)
  return(valores)
}

#Función servidor
function(input, output, session) {
  
  dataset <- reactive({
    precios
  })
  
  #Instrumentos que se pueden vender.
  output$venta <- renderUI({
    selected_value <- input$fondo
    selectizeInput('instrumentov',label ='Venta de Instrumento',instrumentoventa(fondos,selected_value))
  })

  updateSelectizeInput(session, 'instrumentoc', choices = precios$Instrumento, server = TRUE)
  
  #eventReactive()
  
  output$prueba = DT::renderDataTable({fondos})
}

