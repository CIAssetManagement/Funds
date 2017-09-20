#Returns the instruments that a fund can buy.
instrumentocompra <- function(precios,nombre){
  
  deudagub <- c("B","BI","IP","IS","IT","L","L0","LS","M","M0","M3","M5","S0","S3","S5","90","92","95","96")
  deudacorp <- c("71","72","73","74","75","CF","F","I","J","90","91","92","93","94","95","96","97")
  stocksmx <- c("0","1","1B","1F","1S","3")
  stocksint <- c("1A","1E","1I")
  fondos <- c("51","52","53")
  deudausd <- c("D1","D2","D4","D5","D6","D8")
  
  valores <- switch(nombre,
  "+CIGUB"={inst(precios,cigub)},
  "+CIGUMP"={inst(precios,cigump)}
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
  precios$Instrumento[ precios$Tipo %in% include]
}
