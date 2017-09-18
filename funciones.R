instrumentocompra <- function(precios,nombre){
  valores <- switch(nombre,
  "+CIGUB"={unique(precios$Instrumento[c(which(precios$Mercado == "EC"),which(precios$Mercado == "EO"),
             which(precios$Mercado == "ER"),which(precios$Mercado == "MC"),which(precios$Mercado == "NR"))])}
  )
  return(valores)
}

instrumentoventa <- function(fondos,nombre){
  valores <- unique(fondos$Emisora[which(fondos$Fondo == nombre)])
  return(valores)
}