library(dplyr)
library(magrittr)
library(tidyr)

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
  con <- DBI::dbConnect(drv=RMySQL::MySQL(),host="10.1.6.81",user="mau", password="CIBANCO.00", dbname="mydb")
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
  precios <- DBI::dbGetQuery(con, query)
  DBI::dbDisconnect(con)
  precios %>%
    #as date
    mutate(fecha = as.Date(fecha)) %>%
    #repo
    tidyr::spread(id, precio) %>%
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
  con <- DBI::dbConnect(drv=RMySQL::MySQL(),host="10.1.6.81",user="mau", password="CIBANCO.00", dbname="mydb")
  query <- paste("SELECT id,FechaEmision,FechaVencimiento,TasaCupon,TipoTasa,SobreTasa,Frecuencia FROM bonds ")
  if(!is.null(id)) {
    query <- paste0(query, " WHERE ", " id IN ('", paste(id, collapse="','"),"')")
  }
  datos <- DBI::dbGetQuery(con, query)
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
    con <- DBI::dbConnect(drv=RMySQL::MySQL(),host="10.1.6.81",user="mau", password="CIBANCO.00", dbname="mydb")
    fechas <- format(fechas,'%m/%d/%Y')
    query <- paste0("SELECT nivel FROM nodos WHERE id = 'CETES-28' AND fecha in ('",paste(fechas, collapse = "','"),
                    "')")
    y <- DBI::dbGetQuery(con, query)$nivel
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
  con <- DBI::dbConnect(drv=RMySQL::MySQL(),host="10.1.6.81",user="mau", password="CIBANCO.00", dbname="mydb")
  query <- paste0("SELECT Calificadora,Valor FROM calificaciones WHERE Calificadora IN ('",paste(note,collapse = "','"),"')")
  number <- DBI::dbGetQuery(con, query)
  DBI::dbDisconnect(con)
  return(number)
}