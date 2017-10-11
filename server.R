library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(readxl)
library(xts)
library(quantmod)
library(RMySQL)
library(FundTools)

#Instrumentos
instrumentos <- read.csv("Instrumentos.csv",header=TRUE)
#Fondos
fondos <- read.csv("Fondos.csv",header = TRUE)
#Mercados
mercados <- read.csv("mercados.csv",header=TRUE,stringsAsFactors = FALSE)

#Temporales
precios <- read.csv("precios.csv")
precios2 <- read_xlsx("precios2.xlsx")
Fecha <- precios2$Fecha
precios2$Fecha <- NULL

#Lista de instrumentos que los fondos pueden vender.
instrumentoventa <- function(nombre){
  valores <- unique(fondos$Emisora[which(fondos$Fondo == nombre)])
  return(valores)
}

#Lista de instrumentos que los fondos pueden comprar.
tipovalor <- function(nombre){
  
  deudagub <- instrumentos$TipoValor %in% mercados$deudagub
  deudacorp <- instrumentos$TipoValor %in% mercados$deudacorp
  stocksmx <- instrumentos$TipoValor %in% mercados$stocksmx
  stocksint <- instrumentos$TipoValor %in% mercados$stocksint
  fondos <- instrumentos$TipoValor %in% mercados$fondos
  deudausd <- instrumentos$TipoValor %in% mercados$deudausd
  pagares <- instrumentos$TipoValor %in% mercados$pagares
  usd <- instrumentos$TipoValor %in% mercados$usd
  trac <- instrumentos$TipoValor %in% mercados$trac
  
  valores <- switch(nombre,
                    "+CIGUB"={instrumentos$TipoValor[deudagub]},
                    "+CIGUMP"={instrumentos$TipoValor[deudagub]},
                    "+CIGULP"={instrumentos$TipoValor[deudagub]},
                    "+CIPLUS"={instrumentos$TipoValor[c(deudagub,deudacorp,pagares)]},
                    #"+CIBOLS"={instrumentos$TipoValor[c(deudagub,stocksmx,stocksint,fondos,trac)]},
                    "+CIBOLS"={colnames(precios2)},
                    "+CIUSD"={instrumentos$TipoValor[c(usd,trac)]},
                    "+CIEQUS"={instrumentos$TipoValor[c(stocksint,trac,usd)]}
  )
  
  valores <- unique(valores)
  return(valores)
}

emisora <- function(tv){
  
  tipo <- instrumentos$TipoValor %in% tv
  instrumento <- instrumentos$Emision[tipo]
  return(instrumento)
}

instrumentocompra <- function(tv, emisora){
  
  tipo <- instrumentos$TipoValor %in% tv
  emisora <- instrumentos$Emision %in% emisora
  indices <- ifelse(tipo == TRUE,emisora,tipo)
  instrumento <- instrumentos$id[indices]
  return(instrumento)
}

#Función servidor
function(input, output, session) {
  
  #Instrumentos a vender
  observe({
    selected_fund <- input$fondo
    #Instrumentos que se pueden vender.
    updateSelectizeInput(session,inputId='instrumentov',choices=instrumentoventa(selected_fund))
  })
  
  #Seleccion del Tipo de Valor
  observe({
    selected_fund <- input$fondo
    #Selección del Tipo Valor
    updateSelectizeInput(session,inputId='TipoValor',choices=tipovalor(selected_fund))
  })
  
  #Seleccion de la Emisora
  observe({
    selected_type <- input$TipoValor
    #Instrumentos que se pueden comprar.
    updateSelectizeInput(session,inputId='Emisora',choices=emisora(selected_type))
  })
  
  #Instrumentos para comprar
  observe({
    selected_type <- input$TipoValor
    selected_value <- input$Emisora
    #Instrumentos que se pueden comprar.
    updateSelectizeInput(session,inputId='instrumentoc',choices=instrumentocompra(selected_type,selected_value))
  })
  
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
  
  ####################################### Indicadores ##############################################
  
  portafolio <- fondos %>% filter(Fondo == "+CIBOLS" & I == "D")
  indices <- array(match(portafolio$Emisora, colnames(precios2)))
  titulos <- portafolio$Títulos / sum(portafolio$Títulos)
  
  
  portafolioa <- t(titulos * t(precios2[indices]))
  prices <- xts(portafolioa,as.Date(Fecha))
  rend <- rowSums(ROC(prices, type = "discrete"),na.rm = TRUE)
  
  ###Calculo del var###

  varant <- quantile(rend,0.05)
  vardes <- 0.05   
  
  ###Calculo del rendimineto###
  rendacu1 <- (prod(1 + rend/100) - 1)
  
  #Mensaje de error para la venta
  if(varant<vardes){
  observeEvent(input$addv, {
    showModal(modalDialog(title = "ERROR",paste0("No se puede vender ",
    titulosvv(input$titulosv,preciovv())," titulos de ",input$instrumentov," sobrepsa el VaR permitido")))
  })}
  
  #Mensaje de error para la compra
  # if(varant<vardes){
  # observeEvent(input$addc, {
  #   showModal(modalDialog(title = "ERROR",paste0("No se puede comprar ",
  #   tituloscc(input$titulosg,preciocc())," titulos de ",input$instrumentoc," sobrepsa el VaR permitido")))
  # })}
  # 
  # #Data frame para alimentar la tabla con los instrumentos compra
  # dfc <- eventReactive(input$addc,{
  #   fondc <- input$fondo
  #   validate(need(input$instrumentoc != "", "Favor de seleccionar un instrumento"))
  #   instrumentocc <- input$instrumentoc
  #   montoc <- montocc(input$montog,preciocc())
  #   titulosc <- tituloscc(input$titulosg,preciocc())
  #   
  #   new_rowc <- data.frame(Fondo=fondc,Instrumento=instrumentocc,Monto=montoc,Titulos= titulosc)
  #   rowdatac <<- rbind(rowdatac,new_rowc)
  #   return(rowdatac)
  # })
  
  #Data frame foto actual Fondos
  dfinda <-data.frame( Fondo= "CIBOLS",VaR=varant,Rendimiento=rendacu1)
  
  #Data frame simulador
  rowdata <- c()
  dfindd <- eventReactive(input$addv | input$addc,{
    fond <- input$fondo
    VaR <- varant
    rendd <- rendacu1

    newrow <- data.frame(Fondo=fond,VaR=varant,Rendimiento=rendd)
    rowdata <<- rbind(rowdata,newrow)
    return(rowdata)
  })
  
  
  #Bonton delete row
  # del <- eventReactive(input$deleteSelected,{
  #   if(!is.null(input$indd_rows_selected)){
  #     rowdata <- rowdata[-as.numeric(input$indd_rows_selected),]
  #   }
  #   return(rowdata)
  # })
  
  output$ventav <- renderTable({dfv()})
  output$comprac <- renderTable({dfc()})
  
  options(DT.options = list(pageLength = 5))
  output$inda = DT::renderDataTable({dfinda})
  output$indax=renderPrint(input$inda_rows_selected)
  output$indd = DT::renderDataTable({dfindd()})
  output$inddx=renderPrint(input$indd_rows_selected)
}