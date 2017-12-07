library(rdrop2)
library(shiny)
library(DT)
library(dplyr)
library(RMySQL)
library(scales)
library(shinyjs)
library(tidyr)

# Matando la notación científica
options(scipen = 999)

#Instrumentos
info_diaria <- drop_read_csv('Carpeta del equipo CIEstrategias/Instrumentos.csv',stringsAsFactors = FALSE)
#Fondos
fondos <- drop_read_csv('Carpeta del equipo CIEstrategias/Fondos.csv',stringsAsFactors = FALSE)
fondos$X <-  NULL
fondos$Fondo <- gsub("'","",fondos$Fondo)
fondos$Serie <- gsub("'","",fondos$Serie)
colnames(fondos) <- c("I","Fondo","TV","Emisora","Serie","Titulos","Costo.Total")
fondos[is.na(fondos)] <- ""
fondos$Titulos <- as.numeric(as.character(fondos$Titulos))
fondos$Costo.Total <- as.numeric(as.character(fondos$Costo.Total))
fondos <- fondos[!(fondos$Emisora == "CASITA"),]
fondos$Fondo <- gsub("'","",fondos$Fondo)

#Comparables de los fondos
#comparables <- data.frame(id=paste0(fondos$TV,"-",fondos$Emisora,"-",fondos$Serie),Comparable=fondos$Comparable)
#fondos$Comparable <-  NULL

#Mercados
mercados <- drop_read_csv('Carpeta del equipo CIEstrategias/mercados.csv',header=TRUE,stringsAsFactors = FALSE)
#Restricciones de los fondos
maximo <- drop_read_csv('Carpeta del equipo CIEstrategias/limitesmax.csv',header=TRUE)
minimo <- drop_read_csv('Carpeta del equipo CIEstrategias/limitesmin.csv',header=TRUE)
colnames(minimo) <- c('limiteminimo','+CIGUB','+CIGUMP','+CIGULP','+CIPLUS','+CIBOLS','+CIEQUS','+CIUSD')
colnames(maximo) <- c('limitemaximo','+CIGUB','+CIGUMP','+CIGULP','+CIPLUS','+CIBOLS','+CIEQUS','+CIUSD')
#Dias festivos
festivos <- drop_read_csv('Carpeta del equipo CIEstrategias/festivos.csv',header=TRUE,stringsAsFactors = FALSE)
festivos$dias <- as.Date(festivos$dias,format="%d/%m/%Y")
#Efectivo covaf
resumen <- drop_read_csv('Carpeta del equipo CIEstrategias/Resumen_Operaciones.csv',header=TRUE)
namesfondos <- sort(as.character(unique(fondos$Fondo)))
resumen <-  unique(filter(resumen,descripcion %in% namesfondos ))
resumen <- resumen %>% mutate(Monto= saldo+compras-ventas-cintermediario+vintermediario) %>% 
  mutate(Instrumento = "EFECTIVO") %>% mutate(Titulos = 0) %>% 
  select(tipo,descripcion,Instrumento,Titulos,Monto)
colnames(resumen) <- c("I","Fondo","Instrumento","Titulos","Monto")

#Cerrando las conexiones
lapply(DBI::dbListConnections(DBI::dbDriver(drvName = 'MySQL')),DBI::dbDisconnect)

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
  tipo <- sort(tipo[which(tipo != "CHD" & tipo != " ")])
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

#Funciones de la App
BondPrice <- function(mat,day,tcoupn,ytm,period=182){
  #ytm anualizado
  #tcoup as decimals
  #dates as strings
  #period in days
  
  #Turn ytm to effective per period
  ytm <-  period*ytm/360
  #Coupon
  coupn <- period*100*tcoupn/360
  #Day of pricing and maturity date
  maturity <-  as.Date(mat, format="%Y-%m-%d")
  today <- as.Date(day, format="%Y-%m-%d")
  #Number of coupons
  ncoupn <- ceiling(as.numeric(maturity - today)/period)
  #Days until the next coupon
  dcoupn <- (period - as.numeric(maturity - today)) %% period
  
  #Numerator of the formula
  num <- rep(coupn,ncoupn)
  num[length(num)] <- num[length(num)] + 100
  
  #Denominator of the formula
  if (dcoupn == period) {
    denom <- (1 + ytm)^(1:ncoupn)
  } else {
    denom <- (1+ytm)^(1-dcoupn/period)
    denom <- c(denom,(1+ytm)^(2:ncoupn))
  }
  p <- sum(num/denom)
  
  return (p)
}

MacaulayDuration <- function(mat,day,tcoupn, ytm, period=182) {
  #ytm anualizado
  #tcoup as decimals
  #dates as strings
  
  #Market price
  mprice <- BondPrice(mat,day,tcoupn,ytm,period)
  #Turn ytm to effective per period
  ytm <-  period*ytm/360
  #Coupon
  coupn <- period*100*tcoupn/360
  #Day of pricing and maturity date
  maturity <-  as.Date(mat, format="%Y-%m-%d")
  today <- as.Date(day, format="%Y-%m-%d")
  #Number of coupons
  ncoupn <- ceiling(as.numeric(maturity - today)/period)
  #Days until the next coupon
  dcoupn <- (period - as.numeric(maturity - today)) %% period
  
  #Numerator of the Macaulay Duration formula
  num <- seq(1,ncoupn,1)*coupn
  num[length(num)] <- num[length(num)] + 100*ncoupn
  
  #Denominator of the Macaulay Duration formula
  if (dcoupn == period) {
    denom <- (1 + ytm)^(1:ncoupn)
  } else {
    denom <- (1+ytm)^(1-dcoupn/period)
    denom <- c(denom,(1+ytm)^(2:ncoupn))
  }
  mcd <- sum(num/denom)
  #Half years
  mcd <- mcd/mprice
  #Complete years
  mcd <- mcd/(360/period)
  return (mcd)
}

ModifiedDuration <- function(mat,day,tcoupn, ytm,period=182) {
  #ytm anualizado
  #tcoup as decimals
  #dates as strings
  
  #Macaulay Duration
  mdur <- MacaulayDuration(mat,day,tcoupn,ytm,period=182)
  #Modified Duration
  dur <- mdur/(1+ytm)
  return (dur)
}

Convexity <- function(mat,day,tcoupn, ytm, period=182) {
  #ytm anualizado
  #tcoup as decimals
  #dates as strings
  
  #Market price
  mprice <- BondPrice(mat,day,tcoupn,ytm,period=182)
  #Turn ytm to effective per period
  ytm <-  period*ytm / 360
  #Coupon
  coupn <- period*100*tcoupn/360
  #Day of pricing and maturity date
  maturity <-  as.Date(mat, format="%Y-%m-%d")
  today <- as.Date(day, format="%Y-%m-%d")
  #Number of coupons
  ncoupn <- ceiling(as.numeric(maturity - today)/period)
  #Days until the next coupon
  dcoupn <- (period - as.numeric(maturity - today)) %% period
  
  #Numerator of the Convexity formula
  num <- seq(1,ncoupn,1)^2 * seq(1,ncoupn,1) * coupn
  num[length(num)] <- num[length(num)] + 100*ncoupn*ncoupn^2
  
  #Denominator of the Convexity formula
  if (dcoupn == period) {
    denom <- (1 + ytm)^(1:ncoupn)
  } else {
    denom <- (1+ytm)^(1-dcoupn/period)
    denom <- c(denom,(1+ytm)^(2:ncoupn))
  }
  conv <- sum(num/denom)/(1+ytm)^2
  conv <- conv/mprice
  return (conv)
}

PriceChange <- function(mat,day,tcoupn,ytm,period=182,cyield=0.0001){
  #ytm anualizado
  #tcoup as decimals
  #dates as strings
  
  #Duration
  dur <-  ModifiedDuration(mat,day,tcoupn,ytm,period)
  #Convexity
  conv <- Convexity(mat,day,tcoupn,ytm,period)
  #Price Change
  pc <- -dur*cyield + 0.5*conv*cyield^2
  
  return (pc)
}

YTM <- function(mat,day,tcoupn,precio, period=182){
  #ytm anualizado
  #tcoup as decimals
  #dates as strings
  #precio as numeric
  
  #Coupon
  coupn <- period*100*tcoupn/360
  #Day of pricing and maturity date
  maturity <-  as.Date(mat, format="%Y-%m-%d")
  today <- as.Date(day, format="%Y-%m-%d")
  #Number of coupons
  ncoupn <- ceiling(as.numeric(maturity - today)/period)
  #Days until the next coupon
  dcoupn <- (period - as.numeric(maturity - today)) %% period
  
  #################    Newton method    ###################
  
  #Function f(x) for the Newton method
  f <- function(ytm){
    #Numerator of the function
    f_x <- BondPrice(mat,day,tcoupn,ytm,period) - precio
    return(f_x)
  }
  
  #Function f'(x) for the Newton method
  f1 <- function(ytm){
    f <- -ModifiedDuration(mat,day,tcoupn,ytm,period)*precio
    return(f)
  }
  
  #Iterations of Newton method
  xn1 <- tcoupn
  tol <- 1
  while(tol > 1e-7){
    xn <- xn1 - (f(xn1)/f1(xn1))
    tol <- abs(xn - xn1)
    xn1 <-  xn
  }
  return(xn1)
}

bootstrap <- function(data,samp,size, remp = TRUE){
  datos <- matrix(c(replicate(samp,sample(data,size,replace = remp))),nrow = size, ncol = samp)
  return(datos)
}

PortfolioDuration <- function(date=Sys.Date()-1,instruments,weight){
  Bonos <- get_bonds(instruments)
  Bonos$Peso <- weight
  Bonos$TasaCupon <- Bonos$TasaCupon/100
  Precios <- t(get_prices(date,instruments))[,1]
  Bonos$Precio <- as.numeric(Precios[2:length(Precios)])
  Bonos$FechaHoy <- rep(Sys.Date(),length(Bonos$id))
  Bonos$YTM <- mapply(YTM,Bonos$FechaVencimiento,Bonos$FechaHoy,Bonos$TasaCupon,Bonos$Precio,Bonos$Frecuencia)
  Bonos$Duracion <- mapply(ModifiedDuration,Bonos$FechaVencimiento,Bonos$FechaHoy,Bonos$TasaCupon,Bonos$YTM,
                           Bonos$Frecuencia)
  duration <- sum(Bonos$Duracion*Bonos$Peso)
  return(duration)
}

PortfolioConvexity <- function(date=Sys.Date()-1,instruments,weight){
  Bonos <- get_bonds(instruments)
  Bonos$Peso <- weight
  Bonos$TasaCupon <- Bonos$TasaCupon/100
  Precios <- t(get_prices(date,instruments))[,1]
  Bonos$Precio <- as.numeric(Precios[2:length(Precios)])
  Bonos$FechaHoy <- rep(Sys.Date(),length(Bonos$id))
  Bonos$YTM <- mapply(YTM,Bonos$FechaVencimiento,Bonos$FechaHoy,Bonos$TasaCupon,Bonos$Precio,Bonos$Frecuencia)
  Bonos$Convexidad <- mapply(Convexity,Bonos$FechaVencimiento,Bonos$FechaHoy,Bonos$TasaCupon,Bonos$YTM,
                             Bonos$Frecuencia)
  conv <- sum(Bonos$Convexidad*Bonos$Peso)
  return(conv)
}

get_prices <- function(fecha = NULL, id = NULL){
  con <- DBI::dbConnect(drv=RMySQL::MySQL(),host="10.1.6.81",user="shinyapp", password="CIBANCO.00", dbname="mydb",port=3306)
  query <- paste("SELECT fecha, id, precio FROM prices")
  if(!is.null(fecha) | !is.null(id)) {
    query <- paste(query, "WHERE")
    if(!is.null(fecha)) {
      query <- paste0(query, " fecha IN ('", paste(fecha, collapse = "','"), "')")
      if (!is.null(id)) {
        query <- paste(query, "AND")
      }
    }
    if(!is.null(id)) {
      query <- paste0(query, " id IN ('", paste(id, collapse="','"), "')")
    }
  }
  precios <- dbGetQuery(con, query)
  DBI::dbDisconnect(con)
  precios %>%
    #as date
    mutate(fecha = as.Date(fecha)) %>%
    #repo
    spread(id, precio) %>%
    
    data.frame(check.names = FALSE)
}

seq_Date <- function(fecha = NULL){
  fechas <- NULL
  first <- as.Date(substring(fecha,1,8),format='%Y%m%d')
  last <- as.Date(substring(fecha,10,17),format='%Y%m%d')
  if(is.na(last) == TRUE){fechas <- seq.Date(first,Sys.Date(),1)}
  else {fechas <- seq.Date(first,last,1)}
  return(fechas)
}

get_bonds <- function(id = NULL){
  con <- DBI::dbConnect(drv=RMySQL::MySQL(),host="10.1.6.81",user="shinyapp", password="CIBANCO.00", dbname="mydb",port=3306)
  query <- paste("SELECT id,FechaEmision,FechaVencimiento,TasaCupon,TipoTasa,SobreTasa,Frecuencia FROM bonds ")
  if(!is.null(id)) {
    query <- paste0(query, " WHERE ", " id IN ('", paste(id, collapse="','"),"')")
  }
  datos <- dbGetQuery(con, query)
  DBI::dbDisconnect(con)
  datos
}

RiskValues <- function(fecha,instruments,shares,type,confidence = 0.95,period=252){
  #Get prices
  fechas <- seq.Date(fecha-2.5*period,fecha,1)
  precios <- tail(get_prices(fechas,instruments)[-1],period)
  #Calculate weights
  pesos <- tail(t(t(precios) * shares),1)
  pesos <- as.numeric(pesos / sum(pesos))
  #Calculate standard devation and correlation
  rendimientos <- (precios[-1,]/precios[-length(precios[,1]),])
  desv <- apply(rendimientos,2,function(df) sd=sd(df,na.rm = TRUE))
  correlation <- cor(rendimientos,use = "complete.obs")
  sigmap <- sqrt(as.vector(pesos*desv) %*% correlation %*% as.vector(t(pesos*desv)))
  if(type=="bonds"){
    con <- DBI::dbConnect(drv=RMySQL::MySQL(),host="10.1.6.81",user="shinyapp", password="CIBANCO.00", dbname="mydb",port=3306)
    fechas <- format(fechas,'%m/%d/%Y')
    query <- paste0("SELECT nivel FROM nodos WHERE id = 'CETES-28' AND fecha in ('",paste(fechas, collapse = "','"),
                    "')")
    y <- dbGetQuery(con, query)$nivel
    y <- tail(y,period)
    DBI::dbDisconnect(con)
    sigmay <- sd(y[-1]/y[-length(y)])
    sigmap <- sigmap*sigmay
  }
  valuea <- sigmap*qnorm(1-confidence, mean = 0, sd = 1)
  cvaluea <- mean(qnorm(seq(0.001,1-confidence,0.001),0,1)*as.vector(sigmap))
  
  
  values <- data.frame(Days=length(precios[,1]),VaR=valuea,CVaR=cvaluea)
  return(values)
}

Grade2Number <- function(note){
  con <- DBI::dbConnect(drv=RMySQL::MySQL(),host="10.1.6.81",user="shinyapp", password="CIBANCO.00", dbname="mydb",port=3306)
  query <- paste0("SELECT Calificadora,Valor FROM calificaciones WHERE Calificadora IN ('",paste(note,collapse = "','"),"')")
  number <- dbGetQuery(con, query)
  DBI::dbDisconnect(con)
  return(number)
}