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


################################
## UI
################################
ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(title = "Refinadora"),
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

                    ##---------------------------
                    ## Controles de base de datos
                    ##---------------------------
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
                                     selected = 1),
                        hr(),
                        checkboxInput("s_report", label = "Generar reporte"),
                        checkboxInput("s_correct", label = "Corregir base")
                    ),                    

                    ##---------------------------
                    ## Descarga de resultados
                    ##---------------------------
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
                    
                    ##---------------------------
                    ## Reporte
                    ##---------------------------
                    box(
                        background  = "black",
                        title       = "Reporte",
                        width       = 6,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = TRUE,
                        htmlOutput("report")
                    ),

                    ##---------------------------
                    ## Base de datos inicial
                    ##---------------------------
                    box(
                        status      = "warning",
                        title       = "Base de datos inicial",
                        width       = 12,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = TRUE,
                        DT::dataTableOutput("in_table")
                    ),

                    ##---------------------------
                    ## Base de datos final
                    ##---------------------------
                    box(
                        status      = "info",
                        title       = "Base de datos final",
                        width       = 12,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = TRUE,
                        DT::dataTableOutput("out_table")
                    )
                )
            )
        )
    )
)

################################
## SERVER
################################
server <- function(input, output){

    ## ------------------------------
    ## Obtiene base de datos.
    ## ------------------------------
    datasetInput <- reactive({
        ## En esta seccin se carga la base de datos desde la liga que se proporciona.
        ## https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data
        ## https://raw.githubusercontent.com/lromang/IDMX/master/data/coords.csv
        if(input$class == 2){
            full_ent <- data.frame(read.xls(input$url, sheetIndex = 1))
        }else if(input$class == 1){
           ## temporaryFile <- tempfile()
           ##download.file(input$url, destfile=temporaryFile, method="wget")
           ## full_ent <- read.csv(
             ##   temporaryFile,
               ## stringsAsFactors = FALSE,
               ## header = FALSE,
               ## colClasses = rep("character", 6),
               ## encoding = "UTF-8"
               ##)
            full_ent <- fread(input$url)
        }else{
            full_ent <- read.dbf(input$url)
        }
        full_ent
    })

    ## --------------------------------
    ## Despliega base de datos original
    ## --------------------------------
     output$in_table <- DT::renderDataTable({
         ## Despliegue de resultados.
         if(str_length(input$url) > 3){
             table <- datasetInput()
             if(input$s_report != FALSE){          
                 ents_cols <- run.a.test(table)[[2]]             
                 DT::datatable(table,
                               options = list(scrollX = TRUE)) %>%
                     formatStyle(ents_cols,
                                 ## backgroundColor = '#e57373',
                                 color = '#ff5252',
                                 fontWeight = 'bold')
             }else{
                 DT::datatable(table,
                               options = list(scrollX = TRUE)) 
             }
         }
     })

    ## ----------------------------------
    ## Despliega base de datos corregida
    ## ----------------------------------
    output$out_table <- DT::renderDataTable({
        data <- as.data.frame(datasetInput())
        if(input$s_correct == TRUE){
            ## Get analysis
            res       <- run.a.test(data)
            ent_cols  <- res[[2]]
            ## date_cols <- res[[3]]

            ## Dates
            ## for(i in date_cols){
               ## transform_date <- lapply(data[,i], function(t) t <- date_pre_proc(t)[[1]])
               ## data[,i] <- ldply(transform_date, function(t)t <- t)
            ## }

            ## Entities            
            for(i in ent_cols){
                transform_ent  <- transform.all.col(data[,i])[,1:2]
                data[,i] <- NULL
                if(i == 1){
                    data <- cbind(transform_ent, data)
                }else{
                    data <- cbind(data[,1:(i-1)], transform_ent, data[,i:ncol(data)])
                }
            }
            DT::datatable(data,
                          options = list(scrollX = TRUE)) %>%
                formatStyle(ent_cols:(ent_cols + 1),
                            ## backgroundColor = '#00c853',
                            color = '#00e676',
                            fontWeight = 'bold')
        }      
    })

    ## --------------------------------
    ## Despliega reporte
    ## --------------------------------
    output$report <- renderUI({
        if(input$s_report == TRUE){
            HTML(run.a.test(datasetInput())[[1]])
        }
    })
    
    ## ------------------------------
    ## Descarga base de datos
    ## ------------------------------
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
                }else{
                    write.dbf(datasetInput(),
                              file)
                }
            }
    )   
}

shinyApp(ui, server)

