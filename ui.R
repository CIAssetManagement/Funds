library(shiny)
library(shinyjs)
library(rdrop2)

#Fondos
#Fondos
fondos <- drop_read_csv('Carpeta del equipo CIEstrategias/Fondos.csv',stringsAsFactors = FALSE)
fondos$Fondo <- gsub("'","",fondos$Fondo)
namesfondos <- sort(as.character(unique(fondos$Fondo)))

estilo <- "width: 90%; height:90%; position:relative; right:150px; color:#0163A5"
estilor <- "width: 103%; position:relative; right:170px; color:#0163A5"
estilo1 <- "width: 100%; height:90%; position:relative; right:625%; color:#0163A5"
estilor1 <- "width: 103%; position:relative; right:625%; color:#0163A5"
estilow <- "position:relative; right:625%; color:#0163A5"

fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
                    @import url('https://fonts.googleapis.com/css?family=Roboto+Slab');
                    .shiny-output-error-validation {color: #0878C2; font-size: 150%}"),type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }",
                    '#sidebar{font-family:"Roboto Slab", serif;color: #0878C2;background-color:#FFFFFF; 
                    width:450px}
                    body{background-color:#F0F0F0')) ,
  
  headerPanel(h1("Simulador de operaciones", 
                 style = "font-family: 'Roboto Slab', serif;
                 font-weight: 500; line-height: 1.1; 
                 color: #0163A5;")),
  
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
    actionButton("addv","Agregar Venta"),

    #Compra de instrumentos 
    titlePanel("Compra de Instrumentos"),
    selectizeInput('TipoValorc',label='Tipo de Valor',choices = NULL),
    selectizeInput('Emisorac',label='Emisora',choices = NULL),
    selectizeInput('instrumentoc',label ='Instrumento',choices = NULL),
    div(style="display:inline-block",numericInput('montog','Monto', value = 0,min = 0)),
    div(style="display:inline-block",numericInput('titulosg','Títulos', value = 0, min = 0)),

    #Boton para agregar los valores de compra a la tabla.
    actionButton("addc","Agregar Compra"),
    
    #Tabla instrumentos venta 
    h4('Venta de Instrumentos',style="margin:20px 0 0 0;"),
    div(style="align:left; position:relative; top:10px;color: #0878C2;",DT::dataTableOutput('ventav'),
    actionButton("delv","Eliminar")),
    #Tabla instrumentos compra
    h4('Compra de Instrumentos',style="margin:30px 0 0 0;"),
    div(style="align:left; position:relative; top:10px; color: #0878C2;",DT::dataTableOutput('comprac'),
    actionButton("delc","Eliminar")),
    div(style="left:100px; position:relative; top:15px;",
    actionButton("summit","Cargar",width = "200px"))),
  

  fluidRow(
    fluidRow(column(7,h2("Warnings",style = paste0("font-family: 'Roboto Slab', serif;
                 font-weight: 500; line-height: 1.1; color: #0878C2; ",estilow)),
                    hr(style=estilow),
                    div(DT::dataTableOutput("warn"),style=estilow)),
    column(4,h2("Fondos",style = paste0("font-family: 'Roboto Slab', serif;
                 font-weight: 500; line-height: 1.1; color: #0878C2; ",estilo)),
           hr(style=estilor),
    div(DT::dataTableOutput("funda"), style=estilo),
    h2("Medidas de Riesgo",style = paste0("font-family: 'Roboto Slab', serif;
                 font-weight: 500; line-height: 1.1;color: #0878C2; ",estilo)),
    hr(style=estilor),
    div(DT::dataTableOutput("inda"),style=estilo)),
    
  fluidRow(
    column(3,h2("Fondos Operaciones",style = paste0("font-family: 'Roboto Slab', serif;
                 font-weight: 500; line-height: 1.1; color: #0878C2; ",estilo1)),
           hr(style=estilor1),
    div(DT::dataTableOutput("fundd"), style=estilo1),
    h2("Medidas de Riesgo Simulación",style = paste0("font-family: 'Roboto Slab', serif;
                 font-weight: 500; line-height: 1.1;color: #0878C2; ",estilo1)),
    hr(style=estilor1),
    div(DT::dataTableOutput("indd"),style=estilo1))),
    fluidRow(column(7,hr(style="position:relative; left:500px;"),
                    div(style="position:relative; left:500px;",verbatimTextOutput('mensaje')))))
  )
)

