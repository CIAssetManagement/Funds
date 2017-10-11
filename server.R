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
fondos <- fondos[-array(which(fondos$Emisora == 'TOTALES')),]
#Mercados
mercados <- read.csv("mercados.csv",header=TRUE,stringsAsFactors = FALSE)

#Lista de instrumentos que los fondos pueden vender.
tipovalorventa <- function(fondo){
  indices <- fondos$Fondo %in% fondo
  tipo <- fondos$TV[indices]
  #Eliminando los tipos CHD (chequeras en dólares) ya que no se pueden vender.
  tipo <- tipo[which(tipo != "CHD")]
  return(tipo)
}

emisoraventa <- function(fondo,tv){
  fondo <- fondos$Fondo %in% fondo
  tipo <- fondos$TV %in% tv
  indices <- ifelse(fondo == TRUE,tipo,fondo)
  emisora <- fondos$Emisora[indices]
  return(emisora)
}

instrumentoventa <- function(fondo,tv,emisora){
  fondo <- fondos$Fondo %in% fondo
  tipo <- fondos$TV %in% tv
  emisora <- fondos$Emisora %in% emisora
  indices <- ifelse(fondo == TRUE,tipo,fondo)
  indices <- ifelse(indices == TRUE, emisora,indices)
  
  tip <- fondos$TV[indices]
  emi <- fondos$Emisora[indices]
  ser <- fondos$Serie[indices]
  instrumento <- paste0(tip,"-",emi,"-",ser)
  return(instrumento)
}

#Lista de instrumentos que los fondos pueden comprar.
tipovalorcompra <- function(nombre){
  
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
                    "+CIBOLS"={instrumentos$TipoValor[c(deudagub,stocksmx,stocksint,fondos,trac)]},
                    "+CIUSD"={instrumentos$TipoValor[c(usd,trac)]},
                    "+CIEQUS"={instrumentos$TipoValor[c(stocksint,trac,usd)]}
  )
  
  valores <- na.omit(unique(valores))
  return(valores)
}

emisoracompra <- function(tv){
  
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
  
  
  #Seleccion del Tipo de Valor
  observe({
    selected_fund <- input$fondo
    #Selección del Tipo Valor para compra
    updateSelectizeInput(session,inputId='TipoValorc',choices=tipovalorcompra(selected_fund))
    #Seleccion del Tipo de Valor para venta
    updateSelectizeInput(session,inputId='TipoValorv',choices=tipovalorventa(selected_fund))
  })
  
  #Seleccion de la Emisora
  observe({
    selected_typec <- input$TipoValorc
    selected_typev <- input$TipoValorv
    selected_fund <- input$fondo
    #Selección de la Emisora para compra
    updateSelectizeInput(session,inputId='Emisorac',choices=emisoracompra(selected_typec))
    #Selección de la emisora para venta
    updateSelectizeInput(session,inputId='Emisorav',choices=emisoraventa(selected_fund,selected_typev))
  })
  
  #Instrumentos para comprar y vender
  observe({
    selected_fund <- input$fondo
    selected_typec <- input$TipoValorc
    selected_valuec <- input$Emisorac
    selected_typev <- input$TipoValorv
    selected_valuev <- input$Emisorav
    #Instrumentos que se pueden comprar.
    updateSelectizeInput(session,inputId='instrumentoc',choices=instrumentocompra(selected_typec,selected_valuec))
    #Instrumentos que se pueden vender
    updateSelectizeInput(session,inputId='instrumentov',choices=instrumentoventa(selected_fund,
                                                                                 selected_typev,selected_valuev))
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
  preciovv <- reactive({preciov <- get_prices(as.character(Sys.Date()-1),input$instrumentov)[1,2]
  return(preciov)})
  ###precio compra por instrumento
  preciocc <- reactive({precioc <- get_prices(as.character(Sys.Date()-1),input$instrumentoc)[1,2]
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
  
  #portafolio <- fondos %>% filter(Fondo == "+CIBOLS" & I == "D")
  #indices <- array(match(portafolio$Emisora, colnames(precios2)))
  #titulos <- portafolio$Títulos / sum(portafolio$Títulos)
  
  
  #portafolioa <- t(titulos * t(precios2[indices]))
  #prices <- xts(portafolioa,as.Date(Fecha))
  #rend <- rowSums(ROC(prices, type = "discrete"),na.rm = TRUE)
  
  ###Calculo del var###

  #varant <- quantile(rend,0.05)
  #vardes <- 0.05   
  
  ###Calculo del rendimineto###
  #rendacu1 <- (prod(1 + rend/100) - 1)
  
  #Mensaje de error para la venta
  # if(varant<vardes){
  # observeEvent(input$addv, {
  #   showModal(modalDialog(title = "ERROR",paste0("No se puede vender ",
  #   titulosvv(input$titulosv,preciovv())," titulos de ",input$instrumentov," sobrepsa el VaR permitido")))
  # })}
  # 
  #Mensaje de error para la compra
  # if(varant<vardes){
  # observeEvent(input$addc, {
  #   showModal(modalDialog(title = "ERROR",paste0("No se puede comprar ",
  #   tituloscc(input$titulosg,preciocc())," titulos de ",input$instrumentoc," sobrepsa el VaR permitido")))
  # })}
  # 
  
  #Data frame para alimentar la tabla con los instrumentos compra
  dfc <- eventReactive(input$addc,{
    fondc <- input$fondo
    validate(need(input$instrumentoc != "", "Favor de seleccionar un instrumento"))
    instrumentocc <- input$instrumentoc
    montoc <- montocc(input$montog,preciocc())
    titulosc <- tituloscc(input$titulosg,preciocc())

    new_rowc <- data.frame(Fondo=fondc,Instrumento=instrumentocc,Monto=montoc,Titulos= titulosc)
    rowdatac <<- rbind(rowdatac,new_rowc)
    return(rowdatac)
  })
  
  #Data frame foto actual Fondos
  #dfinda <-data.frame( Fondo= "CIBOLS",VaR=varant,Rendimiento=rendacu1)
  
  #Data frame simulador
  #rowdata <- c()
  #dfindd <- eventReactive(input$addv | input$addc,{
  #  fond <- input$fondo
  #  VaR <- varant
  #  rendd <- rendacu1

  #  newrow <- data.frame(Fondo=fond,VaR=varant,Rendimiento=rendd)
  #  rowdata <<- rbind(rowdata,newrow)
  #  return(rowdata)
  
  #})
  
  
  #Bonton delete row
  # del <- eventReactive(input$deleteSelected,{
  #   if(!is.null(input$indd_rows_selected)){
  #     rowdata <- rowdata[-as.numeric(input$indd_rows_selected),]
  #   }
  #   return(rowdata)
  #   reactive(input$deleteSelected <- 0)
  # })
  
  output$ventav <- renderTable({dfv()})
  output$comprac <- renderTable({dfc()})
  
  options(DT.options = list(pageLength = 5))
  output$inda = DT::renderDataTable({dfinda})
  output$indax=renderPrint(input$inda_rows_selected)
  output$indd = DT::renderDataTable({dfindd()})
  output$inddx=renderPrint(input$indd_rows_selected)
}