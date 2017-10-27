library(shiny)
library(DT)
library(dplyr)
library(readxl)
library(xts)
library(quantmod)
library(RMySQL)
library(FundTools)
library(scales)


#Instrumentos
instrumentos <- read.csv("//192.168.0.223/CIFONDOS/Instrumentos.csv",header=TRUE,stringsAsFactors = FALSE)
#Fondos
fondos <- read_excel("//192.168.0.223/CIFONDOS/Fondos2.xlsx")
colnames(fondos) <- c("I","Fondo","TV","Emisora","Serie","Títulos","Costo.Total")
fondos[is.na(fondos)] <- ""
#Mercados
mercados <- read.csv("//192.168.0.223/CIFONDOS/mercados.csv",header=TRUE,stringsAsFactors = FALSE)
#Restricciones de los fondos
#maximo <- read_excel("//192.168.0.223/CIFONDOS/limites.xlsx")
#minimo <- read_excel("//192.168.0.223/CIFONDOS/limites.xlsx")

#Dia hábil
diah <-  function(fecha){
  fechabase0 <- as.Date("2017-08-06")
  entero <- as.integer(fecha - fechabase0 )
  if(entero %% 7 == 6 | entero %% 7 == 0){return(diah(fecha-1))}
  else {return(fecha)}
}

#Funcion que da las calificaciones de los instrumentos en los que puede invertir cada fondo
calificaciones <- function(fondo,tv){
  valores <- unique(c(mercados$revision))
  valores <- valores[ifelse(valores == "",FALSE,TRUE)]
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
  instrumento <- instrumentos$Emisora[indices]
  
  return(instrumento)
}

instrumentocompra <- function(nombre,tv,emisora){
  #Posibles instrumentos
  calificacion <- calificaciones(nombre,tv)
  #Triple match
  indicet <- instrumentos$TipoValor %in% tv
  indicee <- instrumentos$Emisora %in% emisora
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
  
  #Update de los fondos seleccionados
  observe({
    selected_fund <- input$fondo
    updateCheckboxGroupInput(session,"show_vars",selected=selected_fund)
  })
  
  #Calculo del monto o titulos venta
  montovv <- function(monto, precio){
    if(monto == 0 | length(monto) == 0){
      monto <-  floor(input$titulosv)*precio
    }
    else{
      titulos <- floor(input$montov/precio)
      monto <-  titulos*precio
    }
    return(monto)
  }
  titulosvv <- function(titulos,precio){
    if(titulos == 0 | length(titulos) == 0){
      titulos <-  floor(input$montov/precio)
    }
    else{
      titulos <-  floor(input$titulosv)
    }
    
    return(titulos)
  }
  
  #Calculo del monto o titulos compra
  montocc <- function(monto, precio){
    if(monto == 0 | length(monto) == 0){
      monto <-  floor(input$titulosg)*precio
    }
    else{
      titulos <- floor(input$montog/precio)
      monto <- titulos*precio
    }
    return(monto)
  }
  tituloscc <- function(titulos,precio){
    if(titulos == 0 | length(titulos) == 0){
      titulos = floor(input$montog/precio)
    }
    else{
      titulos = floor(input$titulosg)
    }
    
    return(titulos)
  }
  
  ###precio venta por instrumento
  preciovv <- reactive({preciov <- get_prices(diah(Sys.Date()-1),input$instrumentov)[1,2]
  return(preciov)})
  ###precio compra por instrumento
  preciocc <- reactive({precioc <- get_prices(diah(Sys.Date()-1),input$instrumentoc)[1,2]
  return(precioc)})
  
  rowdatac<- c()
  rowdatav <- c()
  
  #Data frame para alimentar la tabla con los instrumentos venta
  dfv <- eventReactive(input$addv,{
    fond <- input$fondo
    instrumento <- input$instrumentov
    monto <- montovv(input$montov,preciovv())
    titulos <- titulosvv(input$titulosv,preciovv())
    
    new_rowv <- data.frame(Fondo=fond,Instrumento=instrumento,
                           Monto=print.default(paste0("$", formatC(as.numeric(monto),
                            format="f", digits=2, big.mark=","))),Titulos=comma(titulos))
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

    new_rowc <- data.frame(Fondo=fondc,Instrumento=instrumentocc,
                           Monto=print.default(paste0("$", formatC(as.numeric(montoc),
                           format="f", digits=2, big.mark=","))),Titulos= comma(titulosc))
    rowdatac <<- rbind(rowdatac,new_rowc)
    return(rowdatac)
  })

  ####################################### Indicadores ##############################################
  
  observe({
    selected_fund <- input$fondo
    
    dfunda$Titulos <- as.numeric(gsub(",","",dfunda$Titulos))
    dfunda$Monto <- substr(dfunda$Monto, 2, 100)
    dfunda$Monto <- as.numeric(gsub(",","",dfunda$Monto))
    
    fondos <- c("+CIGUB","+CIGUMP","+CIGULP","+CIPLUS")
    especiales <- c("TOTALES","EFECTIVO", "0-CASITA-*")
    fondo <- dfunda %>%
      filter(!(Instrumento %in% especiales))
    
    indicesa <- fondo$Fondo %in% selected_fund
    instrumentos <- fondo$Instrumento[indicesa]
    pesos <- fondo$Porcentaje[indicesa]
    titulos <- fondo$Titulos[indicesa]
    
    if (selected_fund %in% fondos){
      
      durant <- round(PortfolioDuration(instrumentos,pesos)*360,digits=0)
      convexant <- round(PortfolioConvexity(instrumentos,pesos),digits=0) 
      varant <- paste0(round(ValueAtRisk(instrumentos,titulos)*100,digits=2),"%")
      metrics <- data.frame(t(c(Fondo=selected_fund,Duracion=durant,Convexidad=convexant,VaR=varant)))
      output$inda = DT::renderDataTable({metrics})
      
    } else {
      
      varant <- paste0(round(ValueAtRisk(instrumentos,titulos)*100,digits=2),"%")
      metrics <- data.frame(t(c(Fondo=selected_fund)))
      metrics <- data.frame(t(c(Fondo=selected_fund,VaR=varant)))
      output$inda = DT::renderDataTable({metrics})
    }

  })
  
  #durdes <- PortfolioDuration(instrumentos,pesos)
  #convexdes <- PortfolioConvexity(instrumentos,pesos)
  
    ######################## Nuevas medidas con fundb ###############################
  metricsd <- c()
  dfindd <- eventReactive(input$summit,{
    
    if(is.null(rowdatav) | is.null(rowdatac)) {
      if(is.null(rowdatav)){
        rowdatac$Monto <- substr(rowdatac$Monto, 2, 100)
        rowdatac$Monto <- as.numeric(gsub(",","",rowdatac$Monto))
        rowdatac$Titulos <- as.numeric(gsub(",","",rowdatac$Titulos))
        Totalc <- rowdatac %>% group_by(Fondo) %>% summarise(sum(Monto))
        colnames(Totalc) <- c("Fondo", "MontoC")
        Totalv <- data.frame(Fondo=Totalc$Fondo,MontoV=rep(0,length(Totalc$Fondo)))
        Total <- merge(Totalc,Totalv,by=c("Fondo"), all=TRUE)
        colnames(Total) <- c("Fondo", "MontoC","MontoV")
      }
      else{
        rowdatav$Monto <- substr(rowdatav$Monto, 2, 100)
        rowdatav$Monto <- as.numeric(gsub(",","",rowdatav$Monto))
        rowdatav$Titulos <- as.numeric(gsub(",","",rowdatav$Titulos))
        Totalv <- rowdatav %>% group_by(Fondo) %>% summarise(sum(Monto))
        colnames(Totalv) <- c("Fondo", "MontoV")
        Totalc <- data.frame(Fondo=Totalv$Fondo,MontoC=rep(0,length(Totalv$Fondo)))
        Total <- merge(Totalc,Totalv,by=c("Fondo"), all=TRUE)
        colnames(Total) <- c("Fondo", "MontoC","MontoV")
      }
    }
    else{
      rowdatac$Monto <- substr(rowdatac$Monto, 2, 100)
      rowdatac$Monto <- as.numeric(gsub(",","",rowdatac$Monto))
      rowdatav$Monto <- substr(rowdatav$Monto, 2, 100)
      rowdatav$Monto <- as.numeric(gsub(",","",rowdatav$Monto))
      rowdatac$Titulos <- as.numeric(gsub(",","",rowdatac$Titulos)) 
      rowdatav$Titulos <- as.numeric(gsub(",","",rowdatav$Titulos)) 
      Totalc <- rowdatac %>% group_by(Fondo) %>% summarise(sum(Monto))
      Totalv <- rowdatav %>% group_by(Fondo) %>% summarise(sum(Monto))
      colnames(Totalc) <- c("Fondo", "MontoC")
      colnames(Totalv) <- c("Fondo", "MontoV")
      Total <- merge(Totalc,Totalv,by=c("Fondo"), all=TRUE)
      colnames(Total) <- c("Fondo", "MontoC","MontoV")
    }

    Total2 <- data.frame(Total$Fondo,Instrumento="TOTAL",Total$MontoC,Total$MontoV) 
    colnames(Total2) <- c("Fondo","Instrumento","MontoC","MontoV")
    Total2 <- merge(Total,e,by = c("Fondo"),all=TRUE)
    MontoTotal <- Total2$MontoC - Total2$MontoV
    EfectivoFinal <- Total2$Monto - Total2$MontoC + Total2$MontoV
    Total2 <- data.frame(Total2$Fondo,MontoTotal,Total2$Monto,EfectivoFinal)
    colnames(Total2) <- c("Fondo","MontoTotal","Efectivo","EfectivoFinal")
    
    fundv <- data.frame(Fondo=rowdatav$Fondo,Instrumento=rowdatav$Instrumento,Monto=rowdatav$Monto*-1,
                        Titulos=rowdatav$Titulos*-1)
    fundc <- rowdatac
    fundn <- rbind.data.frame(fundv,fundc)
    fundn <- fundn %>% group_by(Fondo, Instrumento) %>% summarise(Titulos=sum(Titulos),Monto=sum(Monto))
    
    dfunda$Titulos <- as.numeric(gsub(",","",dfunda$Titulos))
    dfunda$Monto <- substr(dfunda$Monto, 2, 100)
    dfunda$Monto <- as.numeric(gsub(",","",dfunda$Monto))
    
    funds <- merge(dfunda,fundn,by=c("Fondo","Instrumento"), all=TRUE)
    TitulosA=round(ifelse(is.na(funds$Titulos.x)==TRUE,0,funds$Titulos.x),digits = 0)
    MontoA=round(ifelse(is.na(funds$Monto.x)==TRUE,0,funds$Monto.x),digits = 2)
    TitulosN=round(ifelse(is.na(funds$Titulos.y)==TRUE,0,funds$Titulos.y),digits = 0)
    MontoN=round(ifelse(is.na(funds$Monto.y)==TRUE,0,funds$Monto.y),digits = 2)
    funds <- data.frame(cbind(funds[,1:2],TitulosA,MontoA,TitulosN,MontoN))
    colnames(funds) <- c("Fondo", "Instrumento","TitulosA","MontoA","TitulosN","MontoN")
    Titulos <- round(funds$TitulosA+funds$TitulosN,digits = 0)
    Monto <- round(funds$MontoA+funds$MontoN,digits = 2)
    fundb <- data.frame(cbind(funds[,1:2],Titulos,Monto))
    efectivo <- c()
    for (x in unique(fundb$Fondo)){
      indicesf <- fundb$Fondo %in% x
      indicese <- fundb$Instrumento %in% "EFECTIVO"
      indices <- ifelse(indicesf == TRUE, indicese,indicesf)
      nuevoindice <- Total2$Fondo %in% x
      montoinicial <- fundb$Monto[indices]
      cond <- is.na(Total2$EfectivoFinal[nuevoindice])
      montofinal <- ifelse(cond==TRUE,montoinicial,Total2$EfectivoFinal[nuevoindice])
      fundb$Monto[indices] <- montofinal
    }
    
    #Falta efectivo para la compra
    error <- ifelse(Total2$EfectivoFinal<0,TRUE,FALSE)
    fond <- Total2$Fondo[error]
    fond <- paste(fond[!is.na(fond)],collapse=",")
    #Vendiste de más
    error2 <- c()
    for (i in seq(1,length(dfunda$Instrumento),1)){
      indice <- match(fundn$Instrumento,dfunda$Instrumento[i])
      indice <- indice[!is.na(indice)]
      if(length(indice) > 0){
        if(dfunda$Monto[i] + fundn$Monto[indice] < 0){
          error2 <- c(error2,TRUE)
        } else {
          error2 <- c(error2,FALSE)
        }
      } else {
        error2 <- c(error2,FALSE)
      }
    }
    if(TRUE %in% error){
      stop()
    }
    if(TRUE %in% error2){
      stop()
    }
    #Porcentajes de los fondos después de operaciones
    perc <- c()
    dias <- c()
    for (i in seq(1,length(fundb$Fondo),1)){
      #Porcentaje
      indice1 <- fundb$Fondo %in% fundb$Fondo[i]
      indice2 <- fundb$Instrumento %in% "TOTALES"
      indices <- ifelse(indice1 == TRUE,indice2,indice1)
      total <- fundb$Monto[indices]
      p <- round(fundb$Monto[i]/total,digits = 2)
      perc <- c(perc,p)
      #Dias por vencer
      bono <- get_bonds(fundb$Instrumento[i])
      if(is.na(bono[1,1])==TRUE){
        d <- '-'
      } else {
        vencimiento <- as.Date(bono$FechaVencimiento[1],format='%Y-%m-%d')
        d <- vencimiento - Sys.Date()
      }
      dias <- c(dias,d)
    }
    fundb$Porcentaje <- perc
    fundb$DiasxVencer <- dias
    
  ########### New Metrics #####################
    selected_fund <- input$fondo
    
    fondos <- c("+CIGUB","+CIGUMP","+CIGULP","+CIPLUS")
    especiales <- c("TOTALES","EFECTIVO", "0-CASITA-*")
    fondo <- fundb %>%
      filter(!(Instrumento %in% especiales))
    
    indicesa <- fondo$Fondo %in% selected_fund
    instrumentos <- fondo$Instrumento[indicesa]
    pesos <- fondo$Porcentaje[indicesa]
    titulos <- fondo$Titulos[indicesa]
    
    if (selected_fund %in% fondos){
      
      durdes <- round(PortfolioDuration(instrumentos,pesos)*360,digits=0)
      convexdes <- round(PortfolioConvexity(instrumentos,pesos),digits=0) 
      vardes <- paste0(round(ValueAtRisk(instrumentos,titulos)*100,digits=2),"%")
      metricsd <- data.frame(t(c(Fondo=selected_fund,Duracion=durdes,Convexidad=convexdes,VaR=vardes)))
      
    } else {
      
      vardes <- paste0(round(ValueAtRisk(instrumentos,titulos)*100,digits=2),"%")
      metricsd <- data.frame(t(c(Fondo=selected_fund,VaR=vardes)))
    }
    
    return(metricsd)
  })
  
  #Warning para compra de ETFs
   observeEvent(input$TipoValorc, {
     selected_fund <- input$fondo
     selected_type <- input$TipoValorc
     if(selected_fund == "+CIUSD" & selected_type == "1ISP"){
       showModal(modalDialog(title = "WARNING",paste0("Asegurar que el ETF seleccionado es de renta fija. ")))
     }
     if(selected_fund == "+CIPLUS" & selected_type == "1ISP"){
       showModal(modalDialog(title = "WARNING",paste0("Asegurar que el ETF seleccionado es de renta fija. ")))
     }
   })

  #Data frame foto actual Fondos
  instrumento <- paste0(fondos$TV,"-",fondos$Emisora,"-",fondos$Serie)
  instrumento <- ifelse(instrumento == "-TOTALES-","TOTALES",instrumento)
  dfunda <- fondos
  colnames(dfunda) <- c("I","Fondo","TV","Emisora","Serie","Titulos","Monto")
  dfunda$Instrumento <- as.character(instrumento)
  dfunda$Instrumento <- ifelse(dfunda$I!="R",dfunda$Instrumento,dfunda$Instrumento <- "EFECTIVO")
  dfunda$TV <- NULL
  dfunda$Emisora <-  NULL
  dfunda$Serie <- NULL
  efec <- dfunda %>% group_by(Fondo, Instrumento) %>% summarise(sum(Titulos), 
                                                                sum(Monto))
  colnames(efec) <- c("Fondo","Instrumento","Titulos","Monto")
  ind <- ifelse(efec$Instrumento=="EFECTIVO",efec$Titulos== 0,efec$Titulos==efec$Titulos)
  Tit <- data.frame(Titulos=ifelse(ind=="TRUE",efec$Titulos,0))
  dfunda <- data.frame(cbind(efec[,1:2]),Tit,Monto=efec$Monto)
  
  #Porcentajes de los instrumentos y dias por vencer
  perc <- c()
  dias <- c()
  for (i in seq(1,length(dfunda$Fondo),1)){
    #Porcentajes
    indice1 <- dfunda$Fondo %in% dfunda$Fondo[i]
    indice2 <- dfunda$Instrumento %in% "TOTALES"
    indices <- ifelse(indice1 == TRUE,indice2,indice1)
    total <- dfunda$Monto[indices]
    p <- round(dfunda$Monto[i]/total,digits = 2)
    perc <- c(perc,p)
    #Dias por vencer
    bono <- get_bonds(dfunda$Instrumento[i])
    if(is.na(bono[1,1])==TRUE){
      d <- '-'
    } else {
      vencimiento <- as.Date(bono$FechaVencimiento[1],format='%Y-%m-%d')
      d <- vencimiento - Sys.Date()
    }
    dias <- c(dias,d)
  }
  dfunda$Porcentaje  <- perc
  dfunda$DiasxVencer <- dias
  
 dfunda$Titulos <- comma(dfunda$Titulos)
 dfunda$Monto <- paste0("$", formatC(as.numeric(dfunda$Monto),format="f", digits=2, big.mark=","))
  
  e <- filter(efec,Instrumento=="EFECTIVO")
  
  #Data frame nueva foto Fondos
  dffund <- eventReactive(input$summit,{
    if(is.null(rowdatav) | is.null(rowdatac)) {
      if(is.null(rowdatav)){
        rowdatac$Titulos <- as.numeric(gsub(",","",rowdatac$Titulos)) 
        rowdatac$Monto <- substr(rowdatac$Monto, 2, 100)
        rowdatac$Monto <- as.numeric(gsub(",","",rowdatac$Monto))
        Totalc <- rowdatac %>% group_by(Fondo) %>% summarise(sum(Monto))
        colnames(Totalc) <- c("Fondo", "MontoC")
        Totalv <- data.frame(Fondo=Totalc$Fondo,MontoV=rep(0,length(Totalc$Fondo)))
        Total <- merge(Totalc,Totalv,by=c("Fondo"), all=TRUE)
        colnames(Total) <- c("Fondo", "MontoC","MontoV")
      }
      else{
        rowdatav$Titulos <- as.numeric(gsub(",","",rowdatav$Titulos))
        rowdatav$Monto <- substr(rowdatav$Monto, 2, 100)
        rowdatav$Monto <- as.numeric(gsub(",","",rowdatav$Monto))
        Totalv <- rowdatav %>% group_by(Fondo) %>% summarise(sum(Monto))
        colnames(Totalv) <- c("Fondo", "MontoV")
        Totalc <- data.frame(Fondo=Totalv$Fondo,MontoC=rep(0,length(Totalv$Fondo)))
        Total <- merge(Totalc,Totalv,by=c("Fondo"), all=TRUE)
        colnames(Total) <- c("Fondo", "MontoC","MontoV")
      }
    }
    else{
      rowdatac$Monto <- substr(rowdatac$Monto, 2, 100)
      rowdatac$Monto <- as.numeric(gsub(",","",rowdatac$Monto))
      rowdatav$Monto <- substr(rowdatav$Monto, 2, 100)
      rowdatav$Monto <- as.numeric(gsub(",","",rowdatav$Monto))
      rowdatac$Titulos <- as.numeric(gsub(",","",rowdatac$Titulos)) 
      rowdatav$Titulos <- as.numeric(gsub(",","",rowdatav$Titulos)) 
      Totalc <- rowdatac %>% group_by(Fondo) %>% summarise(sum(Monto))
      Totalv <- rowdatav %>% group_by(Fondo) %>% summarise(sum(Monto))
      colnames(Totalc) <- c("Fondo", "MontoC")
      colnames(Totalv) <- c("Fondo", "MontoV")
      Total <- merge(Totalc,Totalv,by=c("Fondo"), all=TRUE)
      colnames(Total) <- c("Fondo", "MontoC","MontoV")
    }

    Total2 <- data.frame(Total$Fondo,Instrumento="TOTAL",Total$MontoC,Total$MontoV) 
    colnames(Total2) <- c("Fondo","Instrumento","MontoC","MontoV")
    Total2 <- merge(Total,e,by = c("Fondo"),all=TRUE)
    MontoTotal <- Total2$MontoC - Total2$MontoV
    EfectivoFinal <- Total2$Monto - Total2$MontoC + Total2$MontoV
    Total2 <- data.frame(Total2$Fondo,MontoTotal,Total2$Monto,EfectivoFinal)
    colnames(Total2) <- c("Fondo","MontoTotal","Efectivo","EfectivoFinal")
    
    fundv <- data.frame(Fondo=rowdatav$Fondo,Instrumento=rowdatav$Instrumento,Monto=rowdatav$Monto*-1,Titulos=rowdatav$Titulos*-1)
    fundc <- rowdatac
    fundn <- rbind.data.frame(fundv,fundc)
    fundn <- fundn %>% group_by(Fondo, Instrumento) %>% summarise(Titulos=sum(Titulos),Monto=sum(Monto))
    
    dfunda$Titulos <- as.numeric(gsub(",","",dfunda$Titulos))
    dfunda$Monto <- substr(dfunda$Monto, 2, 100)
    dfunda$Monto <- as.numeric(gsub(",","",dfunda$Monto))
    
    funds <- merge(dfunda,fundn,by=c("Fondo","Instrumento"), all=TRUE)
    TitulosA=round(ifelse(is.na(funds$Titulos.x)==TRUE,0,funds$Titulos.x),digits = 0)
    MontoA=round(ifelse(is.na(funds$Monto.x)==TRUE,0,funds$Monto.x),digits = 2)
    TitulosN=round(ifelse(is.na(funds$Titulos.y)==TRUE,0,funds$Titulos.y),digits = 0)
    MontoN=round(ifelse(is.na(funds$Monto.y)==TRUE,0,funds$Monto.y),digits = 2)
    funds <- data.frame(cbind(funds[,1:2],TitulosA,MontoA,TitulosN,MontoN))
    colnames(funds) <- c("Fondo", "Instrumento","TitulosA","MontoA","TitulosN","MontoN")
    Titulos <- round(funds$TitulosA+funds$TitulosN,digits = 0)
    Monto <- round(funds$MontoA+funds$MontoN,digits = 2)
    fundb <- data.frame(cbind(funds[,1:2],Titulos,Monto))
    efectivo <- c()
    for (x in unique(fundb$Fondo)){
      indicesf <- fundb$Fondo %in% x
      indicese <- fundb$Instrumento %in% "EFECTIVO"
      indices <- ifelse(indicesf == TRUE, indicese,indicesf)
      nuevoindice <- Total2$Fondo %in% x
      montoinicial <- fundb$Monto[indices]
      cond <- is.na(Total2$EfectivoFinal[nuevoindice])
      montofinal <- ifelse(cond==TRUE,montoinicial,Total2$EfectivoFinal[nuevoindice])
      fundb$Monto[indices] <- montofinal
    }
    
    #No tienes suficiente efectivo para la compra
    error <- ifelse(Total2$EfectivoFinal<0,TRUE,FALSE)
    fond <- Total2$Fondo[error]
    fond <- paste(fond[!is.na(fond)],collapse=",")
    #Vendiste de más
    error2 <- c()
    for (i in seq(1,length(dfunda$Instrumento),1)){
      indice <- match(fundn$Instrumento,dfunda$Instrumento[i])
      indice <- indice[!is.na(indice)]
      if(length(indice) > 0){
        if(dfunda$Monto[i] + fundn$Monto[indice] < 0){
          error2 <- c(error2,TRUE)
        } else {
          error2 <- c(error2,FALSE)
        }
      } else {
        error2 <- c(error2,FALSE)
      }
    }
    fond2 <- dfunda$Instrumento[error2]
    fond2 <- paste(fond2[!is.na(fond2)],collapse=",")
    if(TRUE %in% error){
      showModal(modalDialog(title = "ERROR",paste0("No hay suficiente efectivo para realizar la operacion ",
                                                   "en los siguientes fondos: ",fond)))
      stop()
    }
    if(TRUE %in% error2){
      showModal(modalDialog(title = "ERROR",paste0("No tienes suficientes títulos para realizar la venta ",
                                                   "de los siguientes instrumentos: ",fond2)))
      stop()
    }
    ######################################################################################################
    #Revisando las políticas de inversión de los fondos
    
    
    
    #Porcentajes de los fondos después de operaciones
    perc <- c()
    dias <- c()
    op <- c()
    for (i in seq(1,length(fundb$Fondo),1)){
      #Porcentaje
      indice1 <- fundb$Fondo %in% fundb$Fondo[i]
      indice2 <- fundb$Instrumento %in% "TOTALES"
      indices <- ifelse(indice1 == TRUE,indice2,indice1)
      total <- fundb$Monto[indices]
      p <- round(fundb$Monto[i]/total,digits = 2)
      perc <- c(perc,p)
      #Dias por vencer
      bono <- get_bonds(fundb$Instrumento[i])
      if(is.na(bono[1,1])==TRUE){
        d <- '-'
      } else {
        vencimiento <- as.Date(bono$FechaVencimiento[1],format='%Y-%m-%d')
        d <- vencimiento - Sys.Date()
      }
      dias <- c(dias,d)
    }
    fundb$Porcentaje <- perc
    fundb$DiasxVencer <- dias
    #Operaciones realizadas
    n1 <- fundb$Instrumento %in% rowdatav$Instrumento
    n2 <- fundb$Instrumento %in% rowdatac$Instrumento
    nuevos <- ifelse(n1 == FALSE,n2,n1)
    fundb$Nuevo <- ifelse(nuevos==FALSE,"",nuevos)

    fundb$Titulos <- comma(fundb$Titulos)
    fundb$Monto <- print.default(paste0("$", formatC(as.numeric(fundb$Monto),format="f", digits=2, big.mark=",")))
    
    return(fundb)
  })
  
 output$ventav <- renderTable({dfv()})
 output$comprac <- renderTable({dfc()})
  
 options(DT.options = list(pageLength = 100))
 output$funda = DT::renderDataTable({subset(dfunda,Fondo %in% input$show_vars)},rownames=FALSE)
 output$fundd = DT::renderDataTable({subset(dffund(),Fondo %in% input$show_vars)},rownames=FALSE)
 #output$inda = DT::renderDataTable({medidas})
 output$indd = DT::renderDataTable({dfindd()})
 output$inddx=renderPrint(input$indd_rows_selected)
}
