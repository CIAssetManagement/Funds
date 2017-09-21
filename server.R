library(shiny)
library(ggplot2)
library(DT)

function(input, output) {
  
  precios <- read.csv("Precios.csv",header = TRUE)
  precios <- cbind(precios,Tipo = sub("-.*","",precios$Instrumento))
  fondos <- read.csv("Fondos.csv",header = TRUE)
  
  #Instrumentos que se pueden vender.
  output$venta <- renderUI({
    selected_value <- input$fondo
    selectizeInput('instrumentov', 'Venta de Instrumento', instrumentoventa(fondos,selected_value))
  })
  
  #Instrumentos que se pueden comprar.
  output$compra <- renderUI({
    selected_value <- input$fondo
    selectizeInput('instrumentoc', 'Compra de Instrumento', instrumentocompra(precios,selected_value))
  })
  
  #Tabla de intrumentos venta
  proxy = dataTableProxy('prueba')
  

  dft <- eventReactive(input$addv,{
    fond <- input$fondo
    instrumento <- input$instrumentov
    
    monto <- input$montov
    titulos <- input$titulosv

    new_row <- data.frame(fond,instrumento,monto, titulos)
    #new_row <- proxy %>% DT::addRow(new_row)
    return(new_row)
  })
  
  output$prueba = DT::renderDataTable({dft()})
}

