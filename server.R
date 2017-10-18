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

#Funcion que da las calificaciones de los instrumentos en los que puede invertir cada fondo
calificaciones <- function(fondo,tv){
  valores <- c(mercados$cigump,"71","72","73","74","75","90","91","92","93","94","95","96","97")
  if(tv %in% valores){
    calificacion <- switch(fondo,
                           "+CIGUB"=c("AAA"),
                           "+CIGUMP"=c("AAA"),
                           "+CIGULP"=c("AAA"),
                           "+CIEQUS"=c("AAA","AA+","AA","AA-","A+","A"),
                           "+CIBOLS"=c("AAA","AA+","AA","AA-","A+","A"),
                           "+CIPLUS"=c("AAA","AA+","AA","AA-","A+","A"),
                           "+CIUSD"=c("AAA","AA+","AA","AA-","A+","A"))
  } else {
    calificacion <- c("-")
  }
  return(calificacion)
}
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
  
  cigub <- instrumentos$TipoValor %in% mercados$cigub
  cigump <- instrumentos$TipoValor %in% mercados$cigump
  cigulp <- instrumentos$TipoValor %in% mercados$cigulp
  ciplus <- instrumentos$TipoValor %in% mercados$ciplus
  cibols <- instrumentos$TipoValor %in% mercados$cibols
  ciequs <- instrumentos$TipoValor %in% mercados$ciequs
  ciusd <- instrumentos$TipoValor %in% mercados$ciusd
  
  valores <- switch(nombre,
                    "+CIGUB"={instrumentos$TipoValor[cigub]},
                    "+CIGUMP"={instrumentos$TipoValor[cigump]},
                    "+CIGULP"={instrumentos$TipoValor[cigulp]},
                    "+CIPLUS"={instrumentos$TipoValor[ciplus]},
                    "+CIBOLS"={instrumentos$TipoValor[cibols]},
                    "+CIUSD"={instrumentos$TipoValor[ciusd]},
                    "+CIEQUS"={instrumentos$TipoValor[ciequs]}
  )
  
  valores <- na.omit(unique(valores))
  return(valores)
}

emisoracompra <- function(nombre,tv){
  #Posibles instrumentos
  calificacion <- calificaciones(nombre,tv)
  #Double match
  indicei <- instrumentos$TipoValor %in% tv
  indicec <- instrumentos$Calificacion %in% calificacion
  indices <- ifelse(indicei==TRUE,indicec,indicei)
  #Instrumentos
  instrumento <- instrumentos$Emision[indices]
  
  return(instrumento)
}

instrumentocompra <- function(nombre,tv,emisora){
  #Posibles instrumentos
  calificacion <- calificaciones(nombre,tv)
  #Triple match
  indicet <- instrumentos$TipoValor %in% tv
  indicee <- instrumentos$Emision %in% emisora
  indicec <- instrumentos$Calificacion %in% calificacion
  indicete <- ifelse(indicet == TRUE,indicee,indicet)
  indices <- ifelse(indicete == TRUE,indicec,indicete)
  #Instrumentos
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
    updateSelectizeInput(session,inputId='Emisorac',choices=emisoracompra(selected_fund,selected_typec))
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
    updateSelectizeInput(session,inputId='instrumentoc',choices=instrumentocompra(selected_fund,selected_typec,
                                                                                  selected_valuec))
    #Instrumentos que se pueden vender
    updateSelectizeInput(session,inputId='instrumentov',choices=instrumentoventa(selected_fund,selected_typev,
                                                                                 selected_valuev))
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
 #observeEvent(input$addv, {
 #   showModal(modalDialog(title = "ERROR",paste0("No se puede vender ",
 #   titulosvv(input$titulosv,preciovv())," titulos de ",input$instrumentov," sobrepsa el VaR permitido")))
 #})}

  #Mensaje de error para la compra
  # if(varant<vardes){
  # observeEvent(input$addc, {
  #   showModal(modalDialog(title = "ERROR",paste0("No se puede comprar ",
  #   tituloscc(input$titulosg,preciocc())," titulos de ",input$instrumentoc," sobrepsa el VaR permitido")))
  # })}
  # 
  
  #Data frame foto actual Fondos
  instrumento <- paste0(fondos$TV,"-",fondos$Emisora,"-",fondos$Serie)
  dfunda <- data.frame(
    Fondo=fondos$Fondo,Instrumento=instrumento,Titulos=fondos$Títulos, Monto=fondos$Costo.Total)
  
  #Data frame nueva foto Fondos
  dffund <- eventReactive(input$summit,{
    fundv <- data.frame(Fondo=rowdatav$Fondo,Instrumento=rowdatav$Instrumento,Monto=rowdatav$Monto*-1,Titulos=rowdatav$Titulos*-1)
    fundd <- rowdatac
    fundn <- rbind.data.frame(fundv,fundd)
    
    funds <- merge(dfunda,fundn,by=c("Fondo","Instrumento"), all=TRUE)
    MontoA=ifelse(is.na(funds$Monto.x)==TRUE,0,funds$Monto.x)
    TitulosA=ifelse(is.na(funds$Titulos.x)==TRUE,0,funds$Titulos.x)
    MontoN=ifelse(is.na(funds$Monto.y)==TRUE,0,funds$Monto.y)
    TitulosN=ifelse(is.na(funds$Titulos.y)==TRUE,0,funds$Titulos.y)
    funds <- data.frame(cbind(funds[,1:2],TitulosA,MontoA,TitulosN,MontoN))
    colnames(funds) <- c("Fondo", "Instrumento","TitulosA","MontoA","TitulosN","MontoN")
    Titulos <- funds$TitulosA+funds$TitulosN
    Monto <- funds$MontoA+funds$MontoN
    fundb <- data.frame(cbind(funds[,1:2],Titulos,Monto))
    return(fundb)
  })
  
 output$ventav <- renderTable({dfv()})
 output$comprac <- renderTable({dfc()})
  
 options(DT.options = list(pageLength = 10))
 output$funda = DT::renderDataTable({subset(dfunda,Fondo %in% input$show_vars)})
 output$fundd = DT::renderDataTable({subset(dffund(),Fondo %in% input$show_vars)})
 #output$inda = DT::renderDataTable({})
 #output$indd = DT::renderDataTable({})
 output$inddx=renderPrint(input$indd_rows_selected)
}
