library(shiny)
library(shinyjs)

#Fondos
#Fondos
fondos <- read_excel("//192.168.0.223/CIFONDOS/Fondos.xlsx")
namesfondos <- sort(as.character(unique(fondos$Fondo)))

estilo <- "width: 90%; height:90%; position:relative; right:150px"
estilor <- "width: 103%; position:relative; right:170px"
estilo1 <- "width: 100%; height:90%; position:relative; right:100px"
estilor1 <- "width: 103%; position:relative; right:100px"
estilow <- "position:relative; right:100px"

fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
                    @import url('https://fonts.googleapis.com/css?family=Roboto+Slab');
                    .shiny-output-error-validation {color: #fefefe; font-size: 150%}"),
                    '#sidebar{font-family:"Roboto Slab", serif;color: #fefefe;background-color:#16620a; 
                    width:450px; height:1200px}
                    body{background-color:#4fa22a')) ,
  
  headerPanel(h1("Simulador de operaciones", 
                 style = "font-family: 'Roboto Slab', serif;
                 font-weight: 500; line-height: 1.1; 
                 color: #fefefe;")),
  
  sidebarPanel(
    
    id="sidebar",
    
    titlePanel("Selección de fondos"),
    selectInput('fondo','', namesfondos),
    
    #Venta de instrumentos
    titlePanel("Venta de Instrumentos"),
    selectizeInput('TipoValorv',label='Tipo de Valor',choices = NULL),
    selectizeInput('Emisorav',label='Emisora',choices = NULL),
    selectizeInput('instrumentov',label ='Instrumento',choices = NULL),
    div(style="display:inline-block",numericInput('montov','Monto', value = 0,min = 0)),
    div(style="display:inline-block",numericInput('titulosv','Títulos', value = 0, min = 0)),
    
    #Boton para agregar los valores de venta a la tabla.
    actionButton("addv","Agregar"),

    #Compra de instrumentos 
    titlePanel("Compra de Instrumentos"),
    selectizeInput('TipoValorc',label='Tipo de Valor',choices = NULL),
    selectizeInput('Emisorac',label='Emisora',choices = NULL),
    selectizeInput('instrumentoc',label ='Instrumento',choices = NULL),
    div(style="display:inline-block",numericInput('montog','Monto', value = 0,min = 0)),
    div(style="display:inline-block",numericInput('titulosg','Títulos', value = 0, min = 0)),

    #Boton para agregar los valores de compra a la tabla.
    actionButton("addc","Agregar"),
    
    #Tabla instrumentos venta 
    h4('Venta de Instrumentos',style="margin:20px 0 0 0;"),
    div(style="align:left; position:relative; top:20px;color: #000000;",DT::dataTableOutput('ventav'),
    actionButton("delv","Eliminar")),
    #Tabla instrumentos compra
    h4('Compra de Instrumentos',style="margin:50px 0 0 0;"),
    div(style="align:left; position:relative; top:20px; color: #000000;",DT::dataTableOutput('comprac'),
    actionButton("delc","Eliminar")),
    div(style="align:left; position:relative; top:50px;",
    actionButton("summit","Cargar"))),
  

  fluidRow(
    fluidRow(column(7,h2("Warnings",style = paste0("font-family: 'Roboto Slab', serif;
                 font-weight: 500; line-height: 1.1; color: #fefefe; ",estilow)),
                    hr(style=estilow),
                    div(DT::dataTableOutput("warn"),style=estilow)),
    column(4,h2("Fondos",style = paste0("font-family: 'Roboto Slab', serif;
                 font-weight: 500; line-height: 1.1; color: #fefefe; ",estilo)),
           hr(style=estilor),
    div(DT::dataTableOutput("funda"), style=estilo),
    h2("Medidas de Riesgo",style = paste0("font-family: 'Roboto Slab', serif;
                 font-weight: 500; line-height: 1.1;color: #fefefe; ",estilo)),
    hr(style=estilor),
    div(DT::dataTableOutput("inda"),style=estilo)),
    
  fluidRow(
    column(3,h2("Fondos Operaciones",style = paste0("font-family: 'Roboto Slab', serif;
                 font-weight: 500; line-height: 1.1; color: #fefefe; ",estilo1)),
           hr(style=estilor1),
    div(DT::dataTableOutput("fundd"), style=estilo1),
    h2("Medidas de Riesgo Simulación",style = paste0("font-family: 'Roboto Slab', serif;
                 font-weight: 500; line-height: 1.1;color: #fefefe; ",estilo1)),
    hr(style=estilor1),
    div(DT::dataTableOutput("indd"),style=estilo1)))
    fluidRow(column(7,hr(style="position:relative; left:450px;"),
                    div(style="position:relative; left:450px;",verbatimTextOutput('mensaje')))))
  )
)

