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
precios1 <-  precios

#Lista de instrumentos que los fondos pueden vender.
instrumentoventa <- function(fondos,nombre){
  valores <- unique(fondos$Emisora[which(fondos$Fondo == nombre)])
  return(valores)
}

#Lista de instrumentos que los fondos pueden comprar.
instrumentocompra <- function(precios,nombre, mercados){
  deudagub <- precios$Tipo %in% mercados$deudagub
  deudacorp <- precios$Tipo %in% mercados$deudacorp
  stocksmx <- precios$Tipo %in% mercados$stocksmx
  stocksint <- precios$Tipo %in% mercados$stocksint
  fondos <- precios$Tipo %in% mercados$fondos
  deudausd <- precios$Tipo %in% mercados$deudausd
  pagares <- precios$Tipo %in% mercados$pagares
  usd <- precios$Tipo %in% mercados$usd
  trac <- precios$Tipo %in% mercados$trac
  
  valores <- switch(nombre,
                    "+CIGUB"={precios$Instrumento[deudagub]},
                    "+CIGUMP"={precios$Instrumento[deudagub]},
                    "+CIGULP"={precios$Instrumento[deudagub]},
                    "+CIPLUS"={precios$Instrumento[c(deudagub,deudacorp,pagares)]},
                    "+CIBOLS"={precios$Instrumento[c(deudagub,stocksmx,stocksint,fondos,trac)]},
                    "+CIUSD"={precios$Instrumento[c(usd,trac)]},
                    "+CIEQUS"={precios$Instrumento[c(stocksint,trac,usd)]}
  )
  valores <- unique(valores)
  return(valores)
}

#Función servidor
function(input, output, session) {
 
  observe({
    selected_value <- input$fondo
    #Instrumentos que se pueden vender.
    updateSelectizeInput(session,inputId='instrumentov',choices=instrumentoventa(fondos,selected_value))
    #Instrumentos que se pueden comprar.
    updateSelectizeInput(session,inputId='instrumentoc',choices=instrumentocompra(precios,selected_value,mercados))
  })
  
<<<<<<< HEAD
#Calculo del monto o titulos venta
montovv <- function(monto, precio){
=======
  updateSelectizeInput(session, 'instrumentoc', choices = precios$Instrumento, server = TRUE)
  
  #Calculo del monto o titulos venta
  montovv <- function(monto, precio){
>>>>>>> ac723018c24c50431d9e8349a55a8c535493c143
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
  
  rowdatac<- c()
  rowdatav <- c()
  
  #Data frame para alimentar la tabla con los instrumentos venta
  dfv <- eventReactive(input$addv,{
    fond <- input$fondo
    instrumento <- input$instrumentov
    monto <- montovv(input$montov,preciovv())
    titulos <- titulosvv(input$titulosv,preciovv())
    
    new_rowv <- data.frame(Fondo=fond,Instrumento=instrumento,Monto=monto,Titulos=titulos)
    rowdatav <<- rbind(rowdatav,new_rowv)
    return(rowdatav)
  })
  
  #Data frame para alimentar la tabla con los instrumentos compra
  dfc <- eventReactive(input$addc,{
    fondc <- input$fondo
    instrumentocc <- input$instrumentoc
    montoc <- montocc(input$montog,preciocc())
    titulosc <- tituloscc(input$titulosg,preciocc())
    
    new_rowc <- data.frame(Fondo=fondc,Instrumento=instrumentocc,Monto=montoc,Titulos=titulosc)
    rowdatac <<- rbind(rowdatac,new_rowc)
    return(rowdatac)
  })
  output$ventav = DT::renderDataTable({dfv()})
  output$comprac = DT::renderDataTable({dfc()})
}