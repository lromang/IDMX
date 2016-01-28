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
                        title       = "Base a validar",
                        width       = 3,
                        background  = "black",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = FALSE,
                        textInput("url",label = h3("URL del archivo"), value = "url"),
                        radioButtons("class", label = h3("Tipo de archivo"),
                                     choices = list("csv" = 1, "xlsx" = 2,
                                                    "dbf" = 3),
                                     selected = 1)
                    ),                    

                    ## Descarga de resultados
                    box(
                        title       = "Descarga de base",
                        width       = 3,
                        background  = "black",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = FALSE,
                        textInput("filename", label  = h3("Nombre archivo"),
                                  value = "archivo"),
                        radioButtons("dclass", label = h3("Formato de descarga"),
                                     choices = list("csv" = 1, "xlsx" = 2,
                                                    "dbf" = 3),
                                     selected = 1),
                        downloadButton("downloadData","Descarga")
                    ),

                    ## Reporte
                    box(
                        background  = "black",
                        title       = "Reporte",
                        width       = 6,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = TRUE
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
                    )
                )
            )
        )
    )
)


## Server
server <- function(input, output){

    ## Obtiene base de datos.
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

    ## Render original dataset.
     output$in_table <- renderDataTable({
         ## Despliegue de resultados.
         if(str_length(input$url) > 3){
             datasetInput()
         }
     })

    ## Render results after correction.
    output$out_table <- renderDataTable({
        if(str_detect(input$states,"...") != FALSE ){
            states <- paste0("c(",input$states,")")
            data <- datasetInput()
            data <- cbind(data, transform.all.col(data[,eval(parse(text = states))]))
            data
        }
    })

    ## Download dataset
    output$downloadData <- downloadHandler(
        filename = function(){
            if(input$dclass == 1){
                paste0(input$filename, ".csv")
            }else if(input$dclass == 2){
                paste0(input$filename, ".xlsx")
            }else{
                paste0(input$filename, ".dbf")
            }
        },
        content = function(file){
            if(input$dclass == 1){
                write.table(datasetInput(),
                            file,
                            sep = ",",
                            row.names = FALSE
                            )
            }else if(input$dclass == 2){
                write.xlsx(datasetInput(),
                           file,
                           sheetName = "Sheet1")
            }
        }
    )   
}

shinyApp(ui, server)

