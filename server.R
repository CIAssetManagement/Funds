library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(readxl)
library(xts)
library(quantmod)
library(RMySQL)
library(FundTools)

#Bases de datos
precios <- read.csv("Precios.csv", header=TRUE)
precios2 <- read_xlsx("Precios2.xlsx")
Fecha <- precios2$Fecha
precios2$Fecha <- NULL
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
                    #"+CIBOLS"={precios[c(deudagub,stocksmx,stocksint,fondos,trac)]},
                    "+CIBOLS"={colnames(precios2)},
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