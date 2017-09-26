library(shiny)
library(ggplot2)
library(DT)
library(dplyr)

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
  
  #Instrumentos que se pueden vender.
  output$venta <- renderUI({
    selected_value <- input$fondo
    selectizeInput('instrumentov',label ='Venta de Instrumento',instrumentoventa(fondos,selected_value))
  })

  updateSelectizeInput(session, 'instrumentoc', choices = precios$Instrumento, server = TRUE)

#Calculo del monto o titulos venta
montovv <- function(monto, precio){
    if(monto == 0){
      monto = input$titulosv*precio
    }
    else{
      monto = input$montov
    }
  return(monto)
  }
titulosvv <- function(titulos,precio){
    if(titulos == 0){
      titulos = input$montov/precio
    }
    else{
      titulos = input$titulosv
    }

    return(titulos)
}

#Calculo del monto o titulos compra
montocc <- function(monto, precio){
  if(monto == 0){
    monto = input$titulosg*precio
  }
  else{
    monto = input$montog
  }
  return(monto)
}
tituloscc <- function(titulos,precio){
  if(titulos == 0){
    titulos = input$montog/precio
  }
  else{
    titulos = input$titulosg
  }
  
  return(titulos)
}

###precio venta por instrumento
preciovv <- reactive({preciov <- fondos$Precio[match(input$instrumentov,fondos$Emisora)]
            return(preciov)})
###precio compra por instrumento
preciocc <- reactive({precioc <- precios$Precio[match(input$instrumentoc,precios$Instrumento)]
            return(precioc)})

#Data frame para alimentar la tabla con los instrumentos venta
  dfv <- eventReactive(input$addv,{
    fond <- input$fondo
    instrumento <- input$instrumentov
    monto <- montovv(input$montov,preciovv())
    titulos <- titulosvv(input$titulosv,preciovv())

    new_rowv <- data.frame(fond,instrumento,monto,titulos)
    #new_row <- proxy %>% DT::addRow(new_row)
    return(new_rowv)
  })

#Data frame para alimentar la tabla con los instrumentos compra
  dfc <- eventReactive(input$addc,{
    fondc <- input$fondo
    instrumentocc <- input$instrumentoc
    montoc <- montocc(input$montog,preciocc())
    titulosc <- tituloscc(input$titulosg,preciocc())
    
    new_rowc <- data.frame(fondc,instrumentocc,montoc, titulosc)
    #new_row <- proxy %>% DT::addRow(new_row)
    return(new_rowc)
  })
  output$ventav = DT::renderDataTable({dfv()})
  output$prueba = DT::renderDataTable({dfc()})
}

