library(shiny)
library(ggplot2)

function(input, output) {
  
  precios <- read.csv("Precios.csv",header = TRUE)
  precios <- cbind(precios,Tipo = sub("-.*","",precios$Instrumento))
  fondos <- read.csv("Fondos.csv",header = TRUE)
  
  dataset <- reactive({
    precios
  })
  
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
  
  #output$plot <- renderPlot({
    
    #p <- ggplot(dataset()) + geom_point()
    
    # if (input$color != 'None')
    #   p <- p + aes_string(color=input$color)
    # 
    # facets <- paste(input$facet_row, '~', input$facet_col)
    # if (facets != '. ~ .')
    #   p <- p + facet_grid(facets)
    # 
    # if (input$jitter)
    #   p <- p + geom_jitter()
    # if (input$smooth)
    #   p <- p + geom_smooth()
    # 
    #print(p)
    
  #}, height=700)
  
}

