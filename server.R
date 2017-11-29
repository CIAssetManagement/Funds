library(readxl)
library(shiny)
library(DT)
library(dplyr)
library(xts)
library(quantmod)
library(RMySQL)
library(FundTools)
library(scales)
library(shinyjs)

# Matando la notación científica
options(scipen = 999)

#Instrumentos
info_diaria <- read.csv("C:/Github/Funds/Instrumentos.csv",header=TRUE,stringsAsFactors = FALSE)
#Fondos
fondos <- read_xlsx("C:/Github/Funds/Fondos.xlsx")
colnames(fondos) <- c("I","Fondo","TV","Emisora","Serie","Títulos","Costo.Total")
fondos[is.na(fondos)] <- ""
fondos$Títulos <- as.numeric(as.character(fondos$Títulos))
fondos$Costo.Total <- as.numeric(as.character(fondos$Costo.Total))
fondos <- fondos[!(fondos$Emisora == "CASITA"),]

#Comparables de los fondos
#comparables <- data.frame(id=paste0(fondos$TV,"-",fondos$Emisora,"-",fondos$Serie),Comparable=fondos$Comparable)
#fondos$Comparable <-  NULL

#Mercados
mercados <- read.csv("C:/Github/Funds/mercados.csv",header=TRUE,stringsAsFactors = FALSE)
#Restricciones de los fondos
maximo <- read_excel("C:/Github/Funds/limites.xlsx",sheet = "Maximo")
minimo <- read_excel("C:/Github/Funds/limites.xlsx",sheet = "Minimo")
#Dias festivos
festivos <- read.csv("C:/Github/Funds/festivos.csv",header=TRUE,stringsAsFactors = FALSE)
festivos$dias <- as.Date(festivos$dias,format="%d/%m/%Y")
#Efectivo covaf
resumen <- read.csv("C:/Github/Funds/Resumen_Operaciones.csv",header=TRUE)
namesfondos <- c("+CIBOLS", "+CIEQUS", "+CIGUB", "+CIGULP", "+CIGUMP", "+CIPLUS", "+CIUSD")
resumen <-  unique(filter(resumen,descripcion %in% namesfondos ))
resumen <- resumen %>% mutate(Monto= saldo+compras-ventas-cintermediario+vintermediario) %>% 
                       mutate(Instrumento = "EFECTIVO") %>% mutate(Titulos = 0) %>% 
                       select(tipo,descripcion,Instrumento,Titulos,Monto)
colnames(resumen) <- c("I","Fondo","Instrumento","Titulos","Monto")

#Dia hábil
diah <-  function(fecha){
  fechabase0 <- as.Date("2017-08-06")
  entero <- as.integer(fecha - fechabase0 )
  if(entero %% 7 == 6 | entero %% 7 == 0){
    return(diah(fecha-1))
  } else {
    if(fecha %in% festivos$dia){
      return(diah(fecha-1))
    } else {return(fecha)}
    }
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
  indices1 <- fondos$Fondo %in% fondo
  indices2 <- !(fondos$I %in% "R")
  indices <- ifelse(indices1 == TRUE, indices2,indices1)
  tipo <- fondos$TV[indices]
  #Eliminando los tipos CHD (chequeras en dólares) ya que no se pueden vender.
  tipo <- sort(tipo[which(tipo != "CHD" & tipo != "")])
  return(tipo)
}

emisoraventa <- function(fondo,tv){
  fondo <- fondos$Fondo %in% fondo
  tipo <- fondos$TV %in% tv
  indices <- ifelse(fondo == TRUE,tipo,fondo)
  emisora <- sort(fondos$Emisora[indices])
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
  instrumento <- sort(paste0(tip,"-",emi,"-",ser))
  return(instrumento)
}

#Lista de instrumentos que los fondos pueden comprar.
tipovalorcompra <- function(nombre){
  
  cigub <- info_diaria$TipoValor %in% mercados$cigub
  cigump <- info_diaria$TipoValor %in% mercados$cigump
  cigulp <- info_diaria$TipoValor %in% mercados$cigulp
  ciplus <- info_diaria$TipoValor %in% mercados$ciplus
  cibols <- info_diaria$TipoValor %in% mercados$cibols
  ciequs <- info_diaria$TipoValor %in% mercados$ciequs
  ciusd <- info_diaria$TipoValor %in% mercados$ciusd
  
  valores <- switch(nombre,
                    "+CIGUB"={info_diaria$TipoValor[cigub]},
                    "+CIGUMP"={info_diaria$TipoValor[cigump]},
                    "+CIGULP"={info_diaria$TipoValor[cigulp]},
                    "+CIPLUS"={info_diaria$TipoValor[ciplus]},
                    "+CIBOLS"={info_diaria$TipoValor[cibols]},
                    "+CIUSD"={info_diaria$TipoValor[ciusd]},
                    "+CIEQUS"={info_diaria$TipoValor[ciequs]}
  )
  
  valores <- sort(na.omit(unique(valores)))
  if(nombre=="+CIPLUS"){
    valor <- valores[2]
    valores[2] <- valores[1]
    valores[1] <- valor
  }
  return(valores)
}

emisoracompra <- function(nombre,tv){
  #Posibles instrumentos
  calificacion <- calificaciones(nombre,tv)
  #Double match
  indicei <- info_diaria$TipoValor %in% tv
  indicec <- info_diaria$Calificacion %in% calificacion
  indices <- ifelse(indicei==TRUE,indicec,indicei)
  #Instrumentos
  instrumento <- sort(info_diaria$Emisora[indices])
  
  return(instrumento)
}

instrumentocompra <- function(nombre,tv,emisora){
  #Posibles instrumentos
  calificacion <- calificaciones(nombre,tv)
  #Triple match
  indicet <- info_diaria$TipoValor %in% tv
  indicee <- info_diaria$Emisora %in% emisora
  indicec <- info_diaria$Calificacion %in% calificacion
  indicete <- ifelse(indicet == TRUE,indicee,indicet)
  indices <- ifelse(indicete == TRUE,indicec,indicete)
  #Instrumentos
  instrumento <- sort(info_diaria$id[indices])
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
  
  #Bloqueo de titulos o monto
  observe({
    montoventa <- input$montov
    montocompra <- input$montog
    titulosventa <- input$titulosv
    tituloscompra <- input$titulosg
    
    montoventa <- ifelse(is.na(montoventa)==TRUE,0,montoventa)
    montocompra <- ifelse(is.na(montocompra)==TRUE,0,montocompra)
    titulosventa <- ifelse(is.na(titulosventa)==TRUE,0,titulosventa)
    tituloscompra <- ifelse(is.na(tituloscompra)==TRUE,0,tituloscompra)
    
    if(montoventa != 0){updateNumericInput(session,"titulosv",value = 0)}
    if(titulosventa != 0){updateNumericInput(session,"montov",value = 0)}
    if(montocompra != 0){updateNumericInput(session,"titulosg",value = 0)}
    if(tituloscompra != 0){updateNumericInput(session,"montog",value = 0)}
    
    shinyjs::onclick("montov",shinyjs::disable("titulosv") & shinyjs::enable("montov"))
    shinyjs::onclick("titulosv",shinyjs::disable("montov") & shinyjs::enable("titulosv"))
    shinyjs::onclick("montog",shinyjs::disable("titulosg") & shinyjs::enable("montog"))
    shinyjs::onclick("titulosg",shinyjs::disable("montog") & shinyjs::enable("titulosg"))
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
  
  vals <- reactiveValues()
  #Data frame para alimentar la tabla con los instrumentos venta
  vals$rowdatav <- c()
  observeEvent(input$addv,{
    fond <- input$fondo
    instrumento <- input$instrumentov
    monto <- montovv(input$montov,preciovv())
    titulos <- titulosvv(input$titulosv,preciovv())
    
    new_rowv <- data.frame(Fondo=fond,Instrumento=instrumento,
                           Monto=paste0("$", formatC(as.numeric(monto),
                            format="f", digits=2, big.mark=",")),Titulos=comma(titulos))
    vals$rowdatav<<- rbind(vals$rowdatav,new_rowv)
  })
  
  #Eliminando un dato para la venta
  observeEvent(input$delv,{
    row_to_delv = as.numeric(input$ventav_rows_selected)
    vals$rowdatav = vals$rowdatav[-row_to_delv,]
  })
  
  #Data frame para alimentar la tabla con los instrumentos compra
  vals$rowdatac<- c()
  observeEvent(input$addc,{
    fondc <- input$fondo
    validate(need(input$instrumentoc != "", "Favor de seleccionar un instrumento"))
    instrumentocc <- input$instrumentoc
    montoc <- montocc(input$montog,preciocc())
    titulosc <- tituloscc(input$titulosg,preciocc())
    
    new_rowc <- data.frame(Fondo=fondc,Instrumento=instrumentocc,
                           Monto=paste0("$", formatC(as.numeric(montoc),
                           format="f", digits=2, big.mark=",")),Titulos= comma(titulosc))
    vals$rowdatac <<- rbind(vals$rowdatac,new_rowc)
  })
  
  #Eliminando un dato para la compra
  observeEvent(input$delc,{
    row_to_delc = as.numeric(input$comprac_rows_selected)
    vals$rowdatac = vals$rowdatac[-row_to_delc,]
    
  })

  ####################################### Indicadores ##############################################
  
  observe({
    selected_fund <- input$fondo
    
    dfunda$Titulos <- as.numeric(gsub(",","",dfunda$Titulos))
    dfunda$Monto <- substr(dfunda$Monto, 2, 100)
    dfunda$Monto <- as.numeric(gsub(",","",dfunda$Monto))
    
    fondos <- c("+CIGUB","+CIGUMP","+CIGULP","+CIPLUS")
    especiales <- c("TOTALES","EFECTIVO")
    fondo <- dfunda %>%
      filter(!(Instrumento %in% especiales))
    
    indicesa <- fondo$Fondo %in% selected_fund
    instrumentos <- fondo$Instrumento[indicesa]
    pesos <- fondo$Porcentaje[indicesa]
    titulos <- fondo$Titulos[indicesa]
    
    indicese <- dfunda$Fondo %in% selected_fund
    indicese2 <- dfunda$Instrumento %in% "EFECTIVO"
    indexe <- ifelse(indicese == TRUE, indicese2,indicese)
    
    if (selected_fund %in% fondos){
      
      durant <- round(PortfolioDuration(diah(Sys.Date()-1),instrumentos,pesos)*360,digits=0)
      convexant <- round(PortfolioConvexity(diah(Sys.Date()-1),instrumentos,pesos),digits=0) 
      valueant <- abs(RiskValues(diah(Sys.Date()-1),instrumentos,titulos,"bonds"))
      varant <- paste0(round(valueant$VaR*100,digits=2),"%")
      cvarant <- paste0(round(valueant$CVaR*100,digits=2),"%")
      metrics <- data.frame(t(c(Fondo=selected_fund,Duracion=durant,Convexidad=convexant,VaR=varant,CVaR=cvarant)))
      output$inda = DT::renderDataTable({metrics},options = list(searching = FALSE, paging = FALSE))
      
    } else {
      
      valueant <- abs(RiskValues(diah(Sys.Date()-1),instrumentos,titulos,"stocks"))
      varant <- paste0(round(valueant$VaR*100,digits=2),"%")
      cvarant <- paste0(round(valueant$CVaR*100,digits=2),"%")
      metrics <- data.frame(t(c(Fondo=selected_fund)))
      metrics <- data.frame(t(c(Fondo=selected_fund,VaR=varant,CVaR=cvarant)))
      output$inda = DT::renderDataTable({metrics},options = list(searching = FALSE, paging = FALSE))
    }

  })
  
  #Warning para compra de ETFs
   observeEvent(input$instrumentoc, {
     selected_fund <- input$fondo
     selected_type <- input$TipoValorc
     if(selected_fund == "+CIUSD" & selected_type == "1ISP"){
       showModal(modalDialog(title = "WARNING",paste0("Asegurar que el ETF seleccionado es de renta fija. ")))
     }
     if(selected_fund == "+CIPLUS" & selected_type == "1ISP"){
       showModal(modalDialog(title = "WARNING",paste0("Asegurar que el ETF seleccionado es de renta fija. ")))
     }
   })
   
  #Efectivo ciusd
  ciusd <- resumen[resumen$Fondo == "+CIUSD",]
  
  #Data frame foto actual Fondos
  instrumento <- paste0(fondos$TV,"-",fondos$Emisora,"-",fondos$Serie)
  instrumento <- ifelse(instrumento == "-TOTALES-","TOTALES",instrumento)
  #dfunda <- fondos
  #colnames(dfunda) <- c("I","Fondo","TV","Emisora","Serie","Titulos","Monto")
  #dfunda$Instrumento <- as.character(instrumento)
  dfunda <- data.frame(fondos$I,fondos$Fondo,as.character(instrumento),fondos$Títulos,fondos$Costo.Total)
  colnames(dfunda) <- c("I","Fondo","Instrumento","Titulos","Monto")
  dfunda$Instrumento <- ifelse(dfunda$I!="R",as.character(dfunda$Instrumento),dfunda$Instrumento <- "REPORTO")
  dfunda <- merge(dfunda,resumen,all = TRUE)
  dfunda <- dfunda %>% group_by(Fondo, Instrumento) %>% summarise(sum(Titulos), sum(Monto))
  colnames(dfunda) <- c("Fondo","Instrumento","Titulos","Monto")
  efec <- filter(dfunda,Instrumento=="REPORTO")
  indrep <- which(dfunda$Instrumento=="REPORTO")
  dfunda <- dfunda[-indrep,]
  
  #Porcentajes de los instrumentos y dias por vencer
  perc <- c()
  dias <- c()
  for (i in seq(1,length(dfunda$Fondo),1)){
    #Porcentajes
    indice1 <- dfunda$Fondo %in% dfunda$Fondo[i]
    indice2 <- dfunda$Instrumento %in% "TOTALES"
    indices <- ifelse(indice1 == TRUE,indice2,indice1)
    total <- dfunda$Monto[indices]
    p <- round(dfunda$Monto[i]/total,digits = 3)
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
 
 dfunda2 <- dfunda
 dfunda2$Titulos <- comma(dfunda$Titulos)
 dfunda2$Monto <- paste0("$", formatC(as.numeric(dfunda$Monto),format="f", digits=2, big.mark=","))
 dfunda2$Porcentaje <- paste0(formatC(as.numeric(dfunda$Porcentaje*100),format="f", digits=2, big.mark=","),"%")
 e <- filter(dfunda,Instrumento=="EFECTIVO")
  
  #Data frame nueva foto Fondos
  vals$fundb <- c()
  vals$ValuesAtRisk <- TRUE
  observeEvent(input$summit,{
    rowdatav <- vals$rowdatav
    rowdatac <- vals$rowdatac
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
      Total[is.na(Total)] <- 0
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
    
    funds <- merge(dfunda,fundn,by=c("Fondo","Instrumento"), all=TRUE)
    TitulosA=round(ifelse(is.na(funds$Titulos.x)==TRUE,0,funds$Titulos.x),digits = 0)
    MontoA=ifelse(is.na(funds$Monto.x)==TRUE,0,funds$Monto.x)
    TitulosN=round(ifelse(is.na(funds$Titulos.y)==TRUE,0,funds$Titulos.y),digits = 0)
    MontoN=ifelse(is.na(funds$Monto.y)==TRUE,0,funds$Monto.y)
    funds <- data.frame(cbind(funds[,1:2],TitulosA,MontoA,TitulosN,MontoN))
    colnames(funds) <- c("Fondo", "Instrumento","TitulosA","MontoA","TitulosN","MontoN")
    Titulos <- round(funds$TitulosA+funds$TitulosN,digits = 0)
    Monto <- funds$MontoA+funds$MontoN
    vals$fundb <- data.frame(cbind(funds[,1:2],Titulos,Monto))
    efectivo <- c()
    for (x in unique(vals$fundb$Fondo)){
      indicesf <- vals$fundb$Fondo %in% x
      indicese <- vals$fundb$Instrumento %in% "EFECTIVO"
      indices <- ifelse(indicesf == TRUE, indicese,indicesf)
      nuevoindice <- Total2$Fondo %in% x
      montoinicial <- vals$fundb$Monto[indices]
      cond <- is.na(Total2$EfectivoFinal[nuevoindice])
      montofinal <- ifelse(cond==TRUE,montoinicial,Total2$EfectivoFinal[nuevoindice])
      vals$fundb$Monto[indices] <- montofinal
    }
    
    ######################################################################################################
    #Revisando las políticas de inversión de los fondos
    
    #Porcentajes de los fondos después de operaciones
    perc <- c()
    dias <- c()
    for (i in seq(1,length(vals$fundb$Fondo),1)){
      #Porcentaje
      indice1 <- vals$fundb$Fondo %in% vals$fundb$Fondo[i]
      indice2 <- vals$fundb$Instrumento %in% "TOTALES"
      indices <- ifelse(indice1 == TRUE,indice2,indice1)
      total <- vals$fundb$Monto[indices]
      p <- round(vals$fundb$Monto[i]/total,digits = 3)
      perc <- c(perc,p)
      #Dias por vencer
      bono <- get_bonds(vals$fundb$Instrumento[i])
      if(is.na(bono[1,1])==TRUE){
        d <- '-'
      } else {
        vencimiento <- as.Date(bono$FechaVencimiento[1],format='%Y-%m-%d')
        d <- vencimiento - Sys.Date()
      }
      dias <- c(dias,d)
    }
    vals$fundb$Porcentaje <- perc
    vals$fundb$DiasxVencer <- dias
    
    #Operaciones realizadas
    n1 <- vals$fundb$Instrumento %in% rowdatav$Instrumento
    n2 <- vals$fundb$Instrumento %in% rowdatac$Instrumento
    nuevos <- ifelse(n1 == FALSE,n2,n1)
    vals$fundb$Nuevo <- ifelse(nuevos==FALSE,"",nuevos)
    
    #No tienes suficiente efectivo para la compra
    error <- ifelse(Total2$EfectivoFinal<0,TRUE,FALSE)
    fond <- Total2$Fondo[error]
    fond <- paste(fond[!is.na(fond)],collapse=",")
    #Vendiste de mas
    error2 <- c()
    ddfunda <- dfunda %>% filter(Fondo == input$fondo)
    for (i in seq(1,length(ddfunda$Instrumento),1)){
      indice <- which(fundn$Instrumento==ddfunda$Instrumento[i])
      if(length(indice) > 0){
        if(ddfunda$Monto[i] + sum(fundn$Monto[indice]) < 0){
          error2 <- c(error2,TRUE)
        } else {
          error2 <- c(error2,FALSE)
        }
      } else {
        error2 <- c(error2,FALSE)
      }
    }
    fond2 <- ddfunda$Instrumento[error2]
    fond2 <- paste(fond2[!is.na(fond2)],collapse=",")
    if(TRUE %in% error){
      showModal(modalDialog(title = "ERROR",paste0("No hay suficiente efectivo para realizar la operacion ",
                                                   "en los siguientes fondos: ",fond)))
      vals$ValuesAtRisk <- FALSE
      return()
    } else {
      vals$ValuesAtRisk <- TRUE
    }
    if(TRUE %in% error2){
      showModal(modalDialog(title = "ERROR",paste0("No tienes suficientes títulos para realizar la venta ",
                                                   "de los siguientes instrumentos: ",fond2)))
      vals$ValuesAtRisk <- FALSE
      return()
    } else {
      vals$ValuesAtRisk <- TRUE
    }
    
    fundb2 <- vals$fundb
    fundb2$Titulos <- comma(fundb2$Titulos)
    fundb2$Monto <- paste0("$", formatC(as.numeric(fundb2$Monto),format="f", digits=2, big.mark=","))
    fundb2$Porcentaje <- paste0(formatC(as.numeric(fundb2$Porcentaje*100),format="f", digits=2, big.mark=","),"%")
    
    if(vals$ValuesAtRisk == TRUE)
      output$fundd = DT::renderDataTable({subset(fundb2,Fondo %in% input$fondo)},rownames=FALSE,
                                       options = list(searching = FALSE, paging = FALSE))
    else
      output$fundd = DT::renderDataTable({c()},rownames=FALSE,
                                         options = list(searching = FALSE, paging = FALSE))
  })
  
  ######################## Nuevas medidas con fundb ###############################
  
  dfindd <- eventReactive(input$summit,{
    
    fundb <- vals$fundb
    
    selected_fund <- input$fondo
    
    fondoss <- c("+CIGUB","+CIGUMP","+CIGULP","+CIPLUS")
    especiales <- c("TOTALES","EFECTIVO")
    fondo <- fundb %>%
      filter(!(Instrumento %in% especiales))
    
    indicesa <- fondo$Fondo %in% selected_fund
    instrumentos <- fondo$Instrumento[indicesa]
    pesos <- fondo$Porcentaje[indicesa]
    titulos <- fondo$Titulos[indicesa]
    
    indicese <- fundb$Fondo %in% selected_fund
    indicese2 <- fundb$Instrumento %in% "EFECTIVO"
    indexe <- ifelse(indicese == TRUE, indicese2,indicese)
    
    if (selected_fund %in% fondoss){
      
      durdes <- round(PortfolioDuration(diah(Sys.Date()-1),instrumentos,pesos)*360,digits=0)
      convexdes <- round(PortfolioConvexity(diah(Sys.Date()-1),instrumentos,pesos),digits=0)
      valuesdes <- abs(RiskValues(diah(Sys.Date()-1),instrumentos,titulos, "bonds"))
      vardes <- paste0(round(valuesdes$VaR*100,digits=2),"%")
      cvardes <- paste0(round(valuesdes$CVaR*100,digits=2),"%")
      metricsd <- data.frame(t(c(Fondo=selected_fund,Duracion=durdes,Convexidad=convexdes,VaR=vardes,CVaR=cvardes)))
      
    } else {
      
      valuedes <- abs(RiskValues(diah(Sys.Date()-1),instrumentos,titulos,"stocks"))
      vardes <- paste0(round(valuedes$VaR*100,digits=2),"%")
      cvardes <- paste0(round(valuedes$CVaR*100,digits=2),"%")
      metricsd <- data.frame(t(c(Fondo=selected_fund,VaR=vardes,CVaR=cvardes)))
    }
    if(vals$ValuesAtRisk == TRUE)
      return(metricsd)
    else
      return()
  })
  
  #Warnings generales
  warnings <- eventReactive(input$summit,{
    
    fundb <- vals$fundb
    
    selected_fund <- input$fondo
    
    #Match del fondo con el archivo minimo
    colindex <- which(colnames(minimo) == selected_fund)
    #Los warnings
    fondoss <- c("+CIGUB","+CIGUMP","+CIGULP","+CIPLUS")

    #Para chequeras, sic, fácil realización y deuda en pesos.
    excepciones <- c("TOTALES","EFECTIVO")
    f <- c()
    funddb <- fundb %>% filter(Fondo == selected_fund & !(Instrumento %in% excepciones))
    advert <- c(" ")
    
    for (x in funddb$Instrumento){
      valor <- strsplit(x,"-")[[1]][1]
      f <- c(f,valor)
    }
    funddb$TV <- as.character(f)
    
    #Insumos para VaR y Duracion
    instrumentos <- funddb$Instrumento
    titulos <- funddb$Titulos

    indicese <- fundb$Fondo %in% selected_fund
    indicese2 <- fundb$Instrumento %in% "EFECTIVO"
    indexe <- ifelse(indicese == TRUE, indicese2,indicese)
    cash <- ifelse(selected_fund=="+CIUSD",0,fundb$Monto[indexe])

    #Para fácil realización
    rowindex <- which(minimo$limiteminimo == "facilrealizacion")
    indicesfr11 <- funddb$TV %in% mercados$facilrealizacion
    indicesfr22 <- as.numeric(funddb$DiasxVencer) < 93
    indicesfr <- ifelse(indicesfr11 == FALSE,indicesfr22,indicesfr11)
    
    
    #Valores en reporto
    indicesfr1 <- fundb$Fondo %in% selected_fund
    indicesfr2 <- fundb$Instrumento %in% "EFECTIVO"
    indicesfr3 <- ifelse(indicesfr1 == TRUE,indicesfr2,indicesfr1)
    
    if (!(TRUE %in% indicesfr))
      facilr <- 0
    else
      if(selected_fund != "+CIPLUS")
        facilr <- sum(funddb$Porcentaje[indicesfr],fundb$Porcentaje[indicesfr3],na.rm = TRUE)
      else 
        facilr <- sum(funddb$Porcentaje[indicesfr11],fundb$Porcentaje[indicesfr3],na.rm = TRUE)
        
    error <- ifelse(facilr < as.numeric(minimo[rowindex,colindex]),
                    paste0("Porcentaje de fácil realización por debajo del límite requerido de: ",
                           minimo[rowindex,colindex]),NA)
    advert <- c(advert,error)
    
    if(selected_fund == "+CIPLUS"){
      rowindex <- which(minimo$limiteminimo == "venc")
      indicesvenc <- as.numeric(funddb$DiasxVencer) < 93
      venc <- sum(funddb$Porcentaje[indicesvenc],na.rm = TRUE)
      error <- ifelse(venc < as.numeric(minimo[rowindex,colindex]),
                      paste0("Porcentaje de emisoras con vencimiento menor a 3 meses por debajo del límite 
                             requerido de: ",
                             minimo[rowindex,colindex]),NA)
      advert <- c(advert,error)
      
      
    }

    #Para los valores gubernamentales en México
    rowindex <- which(minimo$limiteminimo == "gubernacional")
    indicesgn <- funddb$TV %in% mercados$gubernacional
    gn <- sum(funddb$Porcentaje[indicesgn],funddb$Porcentaje[indexe])
    error <- ifelse(gn < as.numeric(minimo[rowindex,colindex]),
                    paste0("Porcentaje en valores gubernamentales nacionales por debajo del límite requerido de: ",
                           minimo[rowindex,colindex]),NA)
    advert <- c(advert,error)

    #Para los valores gubernamentales en el extranjero
    rowindex <- which(maximo$limitemaximo == "guberinternacional")
    indicesgi <- funddb$TV %in% mercados$guberinternacional
    gi <- sum(funddb$Porcentaje[indicesgi])
    gi <- ifelse(is.na(gi)==TRUE,0,gi)
    error <- ifelse(gi > as.numeric(maximo[rowindex,colindex]),
                    paste0("Porcentaje en valores gubernamentales en el extranjero por encima del 
                           límite requerido de: ",
                           maximo[rowindex,colindex]),NA)
    advert <- c(advert,error)

    #Para las chequeras
    rowindex <- which(minimo$limiteminimo == "chd")
    indicesch <- funddb$TV %in% "CHD"
    chd <- sum(funddb$Porcentaje[indicesch])
    error <- ifelse(chd < as.numeric(minimo[rowindex,colindex]),
                    paste0("Porcentaje en chequera por debajo del límite requerido de: ",
                           minimo[rowindex,colindex]),NA)
    advert <- c(advert,error) 

    #Para el SIC
    rowindex <- which(minimo$limiteminimo == "sic")
    indicessic <- funddb$TV %in% mercados$sic
    sic <- sum(funddb$Porcentaje[indicessic])
    error <- ifelse(sic < as.numeric(minimo[rowindex,colindex]),
                    paste0("Porcentaje en SIC por debajo del límite requerido de: ",
                           minimo[rowindex,colindex]),NA)
    advert <- c(advert,error)

    #Para la deuda en pesos
    rowindex <- which(minimo$limiteminimo == "deudamxn")
    indicesdmxn <- funddb$TV %in% mercados$deudamxn
    deudamxn <- sum(funddb$Porcentaje[indicesdmxn])
    error <- ifelse(deudamxn < as.numeric(minimo[rowindex,colindex]),
                    paste0("Porcentaje de deuda en pesos por debajo del límite requerido de: ",
                           minimo[rowindex,colindex]),NA)
    advert <- c(advert,error)
    
    #Para las FIBRAS
    rowindex <- which(maximo$limitemaximo == "fibras")
    indicesfib <- funddb$TV %in% mercados$fibras
    fibras <- sum(funddb$Porcentaje[indicesfib])
    error <- ifelse(fibras > as.numeric(maximo[rowindex,colindex]),
                    paste0("Porcentaje en FIBRAS por encima del límite requerido de: ",
                           maximo[rowindex,colindex]),NA)
    advert <- c(advert,error)
    
    #Para las UDI's
    rowindex <- which(maximo$limitemaximo == "udis")
    ud <- with(funddb, substring(Instrumento, nchar(Instrumento)))
    indicesudis <- ifelse(ud == "U" | funddb$TV == "S",TRUE,FALSE)
    udis <- sum(funddb$Porcentaje[indicesudis])
    error <- ifelse(udis > as.numeric(maximo[rowindex,colindex]),
                    paste0("Porcentaje en UDI's por encima del límite requerido de: ",
                           maximo[rowindex,colindex]),NA)
    advert <- c(advert,error)

    #Para los ETF's
    rowindex <- which(maximo$limitemaximo == "etfs")
    indicesetf <- funddb$TV %in% mercados$etf
    etf <- sum(funddb$Porcentaje[indicesetf])
    error <- ifelse(etf > as.numeric(maximo[rowindex,colindex]),
                    paste0("Porcentaje en ETF's por encima del límite requerido de: ",
                           maximo[rowindex,colindex]),NA)
    advert <- c(advert,error)

    #Para la duración
    if(selected_fund %in% fondoss){
      rowindex <- which(minimo$limiteminimo == "duracion")
      inst <- funddb$Instrumento
      pes <- funddb$Porcentaje
      duracion <- round(PortfolioDuration(diah(Sys.Date()-1),inst,pes)*360,digits=0)
      if(duracion < as.numeric(minimo[rowindex,colindex])){
        error <- paste0("Duración por debajo del límite requerido de: ",minimo[rowindex,colindex])
        advert <- c(advert,error)
      }
      if(duracion > as.numeric(maximo[rowindex,colindex])){
        error <- paste0("Duración por encima del límite requerido de: ", maximo[rowindex,colindex])
        advert <- c(advert,error)
      }
    }
      
    #Para el VaR
    fondoss <- c("+CIGUB","+CIGUMP","+CIGULP","+CIPLUS")
    rowindex <- which(maximo$limitemaximo == "var")
    if(selected_fund %in% fondoss)
      valuea <- abs(RiskValues(diah(Sys.Date()-1),instrumentos,titulos,"bonds")$VaR)
    else
      valuea <- abs(RiskValues(diah(Sys.Date()-1),instrumentos,titulos,"stocks")$VaR)
    error <- ifelse(valuea > as.numeric(maximo[rowindex,colindex]),
                    paste0("VaR superior al límite requerido de: ",
                           round(maximo[rowindex,colindex]*100,digits=2),"%"),NA)
    advert <- c(advert,error)

    #Para las calificaciones
    rowindex <- which(minimo$limiteminimo == "calificacion")
    nindices <- funddb$TV %in% mercados$aplicarcalificacion
    if(TRUE %in% nindices){
      inst <- funddb$Instrumento[nindices]
      percentage <- funddb$Porcentaje[nindices] 
      grade <- c()
      calif <- c()
      for (x in inst){
        indicesss <- info_diaria$id %in% x
        grade <- info_diaria$Calificacion[indicesss]
        calif <- c(calif,as.numeric(Grade2Number(grade)$Valor))
      }
      calificacion <- sum(percentage*calif)/sum(percentage)
      
      error <- ifelse(calificacion < as.numeric(Grade2Number(minimo[rowindex,colindex])$Valor),
                      paste0("Calificación promedio inferior al límite requerido de: ",
                             minimo[rowindex,colindex]),NA)
      advert <- c(advert,error)
    }

    advert <- advert[is.na(advert)==FALSE]

    war <- data.frame(Fondo=selected_fund,Warnings=advert)
    
    if(vals$ValuesAtRisk == TRUE)
      return(war)
    else
      return()
  })
  
  cv <- paste0("CVar es la máxima perdida esperada dado que se superó el VaR")
  report <- paste0("Reporto: sólo se puede realizar con contrapartes de calificación mínima de A")
  deri <- paste0("No se puede invertir en derivados, valores estructurados, certificados bursátiles fiduciarios o respaldados por activos")
  msj <- c(cv,report,deri)
  
  output$ventav <- DT::renderDataTable({subset(vals$rowdatav,Fondo %in% input$fondo)},
                                      options = list(searching = FALSE, paging = FALSE))
 
  output$comprac <- DT::renderDataTable({subset(vals$rowdatac,Fondo %in% input$fondo)},
                                       options = list(searching = FALSE, paging = FALSE))
  
  options(DT.options = list(pageLength = 100))
  output$funda = DT::renderDataTable({subset(dfunda2,Fondo %in% input$fondo)},rownames=FALSE,
                                    options = list(searching = FALSE, paging = FALSE))

 output$indd = DT::renderDataTable({subset(dfindd(),Fondo %in% input$fondo)},options = list(searching = FALSE, paging = FALSE))
 output$inddx=renderPrint(input$indd_rows_selected)
 output$warn = DT::renderDataTable({subset(warnings(),Fondo %in% input$fondo)},options = list(searching = FALSE, paging = FALSE))
 output$mensaje = renderPrint(msj)
}
