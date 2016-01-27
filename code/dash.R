####################################################
## This dashboard provides some tools for data   ###
## standarization.                               ###
## Utilities:                                    ###
## 1) Identification and anonymization           ###
##    of entities.                               ###
## 2) Identification and correction of states,   ###
##    municipalities and localities.             ###
## 3) Identification and correction of dates     ###
####################################################

##------------------------------
## Read in functions
##------------------------------
source("./functions.R")

##------------------------------
## UI
##------------------------------
ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(title = "IDMX"),
    ## Dashboard Side Bar
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard",
                     tabName = "dashboard",
                     icon = icon("dashboard"))
        ),
        column(11, offset = 1,
               helpText(h3("Nota: "),
                        p(
                            paste0("Ejemplificación de la corrección automática de errores comúnes",
                                   "en bases de datos públicas"),
                            style="text-align:justify"
                        )
                        )
               )
    ),
    ## Dashboard Main Body
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "dashboard",
                fluidRow(

                    ## Data base controls
                    box(
                        title       = "Archivo a validar",
                        width       = 4,
                        background  = "black",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = TRUE,
                        textInput("url",label = h3("URL del archivo"), value = "url"),
                        radioButtons("class", label = h3("Tipo de archivo"),
                                     choices = list("csv" = 1, "xlsx" = 2,
                                                    "dbf" = 3),
                                     selected = 1)
                    ),
                    
                    ## Selección de columnas
                    box(
                        title       = "Selección de columnas",
                        width       = 4,
                        background  = "black",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = TRUE,
                        textInput("date",
                                  label = h3("Columnas que contienen fechas"),
                                  value = "1,2,3..."),
                        textInput("states",
                                  label = h3("Columnas que contienen estados"),
                                  value = "1,2,3..."),
                        textInput("mun",
                                  label = h3("Columnas que contienen municipios"),
                                  value = "1,2,3..."),
                        textInput("loc",
                                  label = h3("Columnas que contienen localidades"),
                                  value = "1,2,3..."),
                        textInput("URLs",
                                  label = h3("Columnas que contienen URLs"),
                                  value = "1,2,3...")
                    ),

                    ## Descarga de resultados
                    box(
                        title       = "Descarga de resultados",
                        width       = 4,
                        background  = "black",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = TRUE,
                        radioButtons("d_class", label = h3("Formato de descarga"),
                                     choices = list("csv" = 1, "xls/xlsx" = 2,
                                                    "dbf" = 3),
                                     selected = 1)
                    ),

                    ## Base de datos inicial
                    box(
                        status      = "warning",
                        title       = "Base de datos inicial",
                        width       = 12,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = TRUE,
                        dataTableOutput("in_table")
                    ),

                    ## Base de datos final
                    box(
                        status      = "info",
                        title       = "Base de datos final",
                        width       = 12,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = TRUE
                    ),

                    ## Reporte
                    box(
                        status      = "primary",
                        title       = "Reporte",
                        width       = 12,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = TRUE
                    )
                )
            )
        )
    )
)


## Server
server <- function(input, output){
    datasetInput <- reactive({
        ## En esta seccin se carga la base de datos desde la liga que se proporciona.
        ## https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data
        ## https://raw.githubusercontent.com/lromang/IDMX/master/data/coords.csv
        if(input$class == 2){
            full_ent <- data.frame(read.xls(input$url, sheetIndex = 1))
        }else if(input$class == 1){
            full_ent <- read.csv(
                input$url,
                stringsAsFactors = FALSE,
                header = TRUE,
                colClasses = rep("character", 6),
                encoding = "UTF-8"
            )
        }else{
            full_ent <- read.dbf(input$url)
        }
        full_ent
    })

     output$in_table <- renderDataTable({
         ## Despliegue de resultados.
         if(str_length(input$url) > 3){
             datasetInput()
         }
     })

    output$out_table <- renderDataTable({
        if(str_detect(input$states,"...") != FALSE ){
            states <- paste0("c(",input$states,")")
            data <- datasetInput()
            data <- cbind(data, transform.all.col(data[,eval(parse(text = states))]))
            data
        }
    })
}

shinyApp(ui, server)

