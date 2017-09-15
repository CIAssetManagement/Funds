instrumentocompra <- function(nombre){
  
}

instrumentoventa <- function(fondos,nombre){
  valores <- unique(fondos$Emisora[which(fondos$Fondo == nombre)])
  return(valores)
}