#Returns the instruments that a fund can buy.
instrumentocompra <- function(precios,nombre){
  cigub <- c("B","BI","E","F","IP","IS","IT","L","L0","LS","LT","M","M0","M3","M5","M7","MC","MP","N","S","S0","S3","S5","T","T3","T5")
  valores <- switch(nombre,
  "+CIGUB"={inst(precios,cigub)}
  )
  return(valores)
}
#Returns the instruments that a fund can sell.
instrumentoventa <- function(fondos,nombre){
  valores <- unique(fondos$Emisora[which(fondos$Fondo == nombre)])
  return(valores)
}
#Function to supply elements in the market of a fund.
inst <- function(precios, include){
  precios$Instrumento[ a %in% include]
}

