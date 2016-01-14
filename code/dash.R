############################################
## Maria Fernanda Mora Alba
## 000103596
############################################

############################################
## This code displays in a dashboard three
## applications:
## 1) Stochastic integration
## 2) Bootstrap Bias Correction
## 3) Linear Congruential Generator
############################################


## Libraries
library(shiny)
library(shinydashboard)
library(plyr)
library(ggplot2)
library(tidyr)

## UI
ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(title = "CompuStats"),
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
                        paste0("Ejemplificación de la corrección de sesgo usando Bootstrap.",
                               " Uńicamente especifique el tamaño de muestra y el número de",
                               " muestras Bootstrap"),
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


                    ## Estimates importance sampling Graph
                    box(
                        title       = "IS Estimates",
                        width       = 4,
                        background  = "blue",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = TRUE,
                        plotOutput("is_plot",
                                   height = 250)
                    ),

                    ##  Estimates MC Graph
                    box(
                        title       = "Monte Carlo Estimates",
                        width       = 4,
                        background  = "blue",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = TRUE,
                        plotOutput("mc_plot",
                                   height = 250)
                    ),

                    ## Error mc & is density
                    box(
                        title       = "MC & IS Error",
                        width       = 4,
                        background  = "blue",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = TRUE,
                        plotOutput("is_mc_plot",
                                   height = 250)
                    ),

                    
                    ## Bootstrap Bias Graph
                    box(
                        title       = "Bootstrap Bias Correction",
                        width       = 12,
                        background  = "fuchsia",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = TRUE,
                        plotOutput("boots_plot",
                                   height = 250)
                    ),                    

                    ## LCG Summary
                    box(
                        title       = "LCG Summary",
                        width       = 5,
                        background  = "purple",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = TRUE,
                        tableOutput("lcg_summ")
                    ),

                    ## LCG plot
                    box(
                        title       = "LCG Plot",
                        width       = 5,
                        background  = "purple",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = TRUE,
                        plotOutput("lcg_plot")
                    ),

                    ## LCG head
                    box(
                        title       = "LCG Head",
                        width       = 2,
                        background  = "purple",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = TRUE,
                        tableOutput("lcg_head")
                    ),
                    
                    ## Error mc & is Controls
                    box(
                        title       = "MC & IS Controls",
                        width       = 4,
                        background  = "black",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = TRUE,
                        sliderInput("alpha",
                                    "alpha:", min = 0.0001, max = 1, step = .1, value = .5),
                        sliderInput("N_is",
                                    "N:", min = 100, max = 1000, step = 50, value = 100),
                        sliderInput("n_is",
                                    "n:", min = 100, max = 1000, step = 50, value = 100),
                        sliderInput("m_is",
                                    "m:", min = 1, max = 10, step = 1, value = 5)
                    ),

                    ## Bootstrap Bias Controls
                    box(
                        title       = "Bootstrap Controls",
                        width       = 4,
                        background  = "black",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = TRUE,                        
                        sliderInput("sample_size",
                                    "Tamaño de muestra (sesgo):",
                                    step  = 100,
                                    max   = 1000,
                                    min   = 100,
                                    value = 500                                    
                                    )
                    ),
                    
                    ## LCG Controls
                    box(
                        title       = "LCG Controls",
                        width       = 4,
                        background  = "black",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed   = TRUE,
                        sliderInput("nsim",
                                    "Tamaño de muestra:", 1, 100, 50),
                        textInput("m",
                                    "M:", value = 2^32),
                        textInput("a",
                                  "a:", value = 22695477),
                        textInput("c",
                                    "c:", value = 1)
                    )
                )
            )
        )
    )
)
## Server
server <- function(input, output) {
    data1 = data.frame(
        x = c(
            0.01,
            0.48,
            0.71,
            0.95,
            1.19,
            0.01,
            0.48),
        y = c(
            127.6,
            124.0,
            110.8,
            103.9,
            101.5,
            130.1,
            122.0)
    )
    data2 = data.frame(
        x = c(
            1.44,
            0.71,
            1.96,
            0.01,
            1.44,
            1.96),
        y = c(
            92.3,
            113.1,
            83.7,
            128.0,
            91.4,
            86.2)
    )

    ## Bootstrap
    bootstrap_test <- function(d1, d2){
        ## Generate samples
        sample1 <- sample(nrow(d1), nrow(d1), replace = TRUE)
        sample2 <- sample(nrow(d2), nrow(d2), replace = TRUE)
        ## Choose new data
        d1      <- d1[sample1, ]
        d2      <- d2[sample2, ]
        ## Fit model
        fit1    <- lm(y~x, data = d1)
        fit2    <- lm(y~x, data = d2)
        ## Output statistic
        fit2$coefficients[2]/fit1$coefficients[2]
    }

    ## Bootstrap n times
    bootstrap_test_n <- function(d1, d2, n){
        replicate(n, bootstrap_test(d1, d2))
    }

    ## Bootstrap Graph
    output$boots_plot <- renderPlot({
        skwe <- data.frame(theta = bootstrap_test_n(data1,
                                                   data2,
                                                   input$sample_size)
                          )
        non_skwe <- data.frame(theta = skwe[,1] - mean(na.omit(skwe[,1])))
        ggplot(data = skwe, aes(x = theta)) +
            geom_histogram(fill = "#FAFAFA", col = "#FAFAFA") +
            geom_histogram(data = non_skwe, aes(x = theta), fill = "#311b92",
                           col = "#311b92") +
            theme(
                panel.background = element_rect(fill = "transparent", colour = NA),
                plot.background = element_rect(fill = "transparent", colour = NA),
                text = element_text(color = "#FAFAFA",
                                    face  = "bold"),
                axis.ticks = element_blank(),
                axis.text  = element_text(color = "#FAFAFA",
                                          face  = "bold"),
                axis.title.y = element_text( vjust = 1.5),
                axis.title.x = element_text( vjust = .1)
                  )
    }, bg = "transparent")

    ## LCG Graph
    LCG <- function(nsim, M = 2^32, a = 22695477, c = 1, seed  = 103596){
        x = c(seed,numeric(nsim - 1))
        for(i in 1:(nsim - 1)) x[ i + 1] <- ((a *x[i] + c) %% M) # Apply LCG rule
        return (x / M) # Apply transform
    }


    ## Summary
    output$lcg_summ <- renderTable({
        rseq <- LCG(extract_numeric(input$nsim),
                   M = extract_numeric(input$m),
                   a = extract_numeric(input$a),
                   c = extract_numeric(input$c))
        summ <- summary(rseq)
        data.frame("Min" = summ[1],
                   "1st Q." = summ[2],
                   "Median" = summ[3],
                   "Mean" = summ[4],
                   "3rd Q." = summ[5],
                   "Max" = summ[6],
                   "sd"  = sd(rseq)
                   )
    })

    ## Head
    output$lcg_head <- renderTable({
        rseq <- LCG(extract_numeric(input$nsim),
                   M = extract_numeric(input$m),
                   a = extract_numeric(input$a),
                   c = extract_numeric(input$c))
        data <- data.frame(rseq = rseq)
        head(data)
    })

    ## Plot
    output$lcg_plot <- renderPlot({
        rseq <- LCG(extract_numeric(input$nsim),
                   M = extract_numeric(input$m),
                   a = extract_numeric(input$a),
                   c = extract_numeric(input$c))
        data_plot <- data.frame(vec1 = rseq[-input$nsim],
                               vec2 = rseq[-1])
        ggplot(data = data_plot, aes(x = vec1, y = vec2)) +
            geom_point(col = "#FAFAFA") +
            theme(
                panel.background = element_rect(fill = "transparent", colour = NA),
                plot.background = element_rect(fill = "transparent", colour = NA),
                text = element_text(color = "#FAFAFA",
                                    face  = "bold"),
                axis.ticks = element_blank(),
                axis.text  = element_text(color = "#FAFAFA",
                                          face  = "bold"),
                axis.title.y = element_text( vjust = 1.5),
                axis.title.x = element_text( vjust = .1)
                  ) 
    }, bg = "transparent")

    ## is_plot
    output$is_plot <- renderPlot({

        n     <- input$n_is 
        N     <- input$N_is
        alpha <- input$alpha
        m     <- input$m_is

        ## Initialization
        real     <- 1 - exp(-2 * m)                

        ## Estimations
        estim_mc <- c()  ##rep(0,N)
        estim_is <- c()  ##rep(0,N)

        ## Error
        error_mc <- c()  ##rep(0,N)
        error_is <- c()  ##rep(0,N)

        ## Bounds
        lower_mc <- c()  ##rep(0,N)
        upper_mc <- c()  ##rep(0,N)

        ## Bounds
        lower_is <- c()  ##rep(0,N)
        upper_is <- c()  ##rep(0,N)

        
        for(i in 1:N){

            ## MC
            U           <- runif(n, 0, 2) # Ojo: de 0 a 2
            phi         <- function(x) 2 * m * exp(- m * x^2)
            estim_mc[i] <- mean(phi(U))
            s1          <- var(phi(U))
            quant       <- qnorm(alpha / 2, lower.tail = FALSE)
            upper_mc[i] <- estim_mc[i] + sqrt(s1 / n) * quant  #intervalos de confianza
            lower_mc[i] <- estim_mc[i] - sqrt(s1 / n) * quant
            error_mc[i] <- 1 - exp(-2 * m) - estim_mc[i]
              
            U <- runif(n, 0, 1) # Ojo: de 0 a 1
            X <- -log(1 - (1 - exp(-2))*U)

            ## IS
            fun         <- function(x) dexp(x) / (1 - exp(-2)) # Densidad de lambda truncada
            phi         <- function(x) m * exp(-m * x^2) / fun(x)  #corrección importance sampling
            estim_is[i] <- mean(phi(X))
            s1          <- var(phi(X))
            quant       <- qnorm(alpha / 2, lower.tail = FALSE)
            upper_is[i] <- estim_is[i] + sqrt(s1 / n) * quant  #intervalos de confianza
            lower_is[i] <- estim_is[i] - sqrt(s1 / n) * quant
            error_is[i] <- 1 - exp(-2 * m) - estim_is[i]            
        }
        
        ## Data MC estimates
        data_mc    <- data.frame(estim = estim_mc,
                                upper = upper_mc,
                                lower = lower_mc,
                                index = 1:N)

        ## Data IS estimates
        data_is    <- data.frame(estim = estim_is,
                                upper = upper_is,
                                lower = lower_is,
                                index = 1:N)
        
        ## Data MC & IS errors
        data_mc_is <- rbind(
            data.frame(id = 'mc', error = error_mc),
            data.frame(id = 'is', error = error_is)
        )

       # plot_density <- ggplot(data_mc_is) +
        #    geom_density(aes(errores)) +
         #   geom_vline(aes(xintercept = real)) +
          #  facet_wrap( ~ id)
        
        ## Final Plot
        ggplot(data = data_is,
               aes(x = index,
                   y = estim)) + geom_line(col = "#ffc107") +
            geom_line(data = data_is,
                      aes(x = index,
                          y = lower
                          ),
                      col = "#fafafa"
                      ) +
            geom_line(data = data_is,
                      aes(x = index,
                          y = upper
                          ),
                      col = "#fafafa"
                      ) +
                        theme(
                panel.background = element_rect(fill = "transparent", colour = NA),
                plot.background  = element_rect(fill = "transparent", colour = NA),
                text = element_text(color = "#FAFAFA",
                                    face  = "bold"),
                axis.ticks = element_blank(),
                axis.text  = element_text(color = "#FAFAFA",
                                          face  = "bold"),
                axis.title.y = element_text( vjust = 1.5),
                axis.title.x = element_text( vjust = .1)
                  )
    }, bg = "transparent")

    ## mc_plot
    output$mc_plot <- renderPlot({

        n     <- input$n_is 
        N     <- input$N_is
        alpha <- input$alpha
        m     <- input$m_is

        ## Initialization
        real     <- 1 - exp(-2 * m)                

        ## Estimations
        estim_mc <- c()  ##rep(0,N)
        estim_is <- c()  ##rep(0,N)

        ## Error
        error_mc <- c()  ##rep(0,N)
        error_is <- c()  ##rep(0,N)

        ## Bounds
        lower_mc <- c()  ##rep(0,N)
        upper_mc <- c()  ##rep(0,N)

        ## Bounds
        lower_is <- c()  ##rep(0,N)
        upper_is <- c()  ##rep(0,N)

        
        for(i in 1:N){

            ## MC
            U           <- runif(n, 0, 2) # Ojo: de 0 a 2
            phi         <- function(x) 2 * m * exp(- m * x^2)
            estim_mc[i] <- mean(phi(U))
            s1          <- var(phi(U))
            quant       <- qnorm(alpha / 2, lower.tail = FALSE)
            upper_mc[i] <- estim_mc[i] + sqrt(s1 / n) * quant  #intervalos de confianza
            lower_mc[i] <- estim_mc[i] - sqrt(s1 / n) * quant
            error_mc[i] <- 1 - exp(-2 * m) - estim_mc[i]
              
            U <- runif(n, 0, 1) # Ojo: de 0 a 1
            X <- -log(1 - (1 - exp(-2))*U)

            ## IS
            fun         <- function(x) dexp(x) / (1 - exp(-2)) # Densidad de lambda truncada
            phi         <- function(x) m * exp(-m * x^2) / fun(x)  #corrección importance sampling
            estim_is[i] <- mean(phi(X))
            s1          <- var(phi(X))
            quant       <- qnorm(alpha / 2, lower.tail = FALSE)
            upper_is[i] <- estim_is[i] + sqrt(s1 / n) * quant  #intervalos de confianza
            lower_is[i] <- estim_is[i] - sqrt(s1 / n) * quant
            error_is[i] <- 1 - exp(-2 * m) - estim_is[i]            
        }
        
        ## Data MC estimates
        data_mc    <- data.frame(estim = estim_mc,
                                upper = upper_mc,
                                lower = lower_mc,
                                index = 1:N)

        ## Data IS estimates
        data_is    <- data.frame(estim = estim_is,
                                upper = upper_is,
                                lower = lower_is,
                                index = 1:N)
        
        ## Data MC & IS errors
        data_mc_is <- rbind(
            data.frame(id = 'mc', error = error_mc),
            data.frame(id = 'is', error = error_is)
        )

       # plot_density <- ggplot(data_mc_is) +
        #    geom_density(aes(errores)) +
         #   geom_vline(aes(xintercept = real)) +
          #  facet_wrap( ~ id)
        
        ## Final Plot
        ggplot(data = data_mc,
               aes(x = index,
                   y = estim)) + geom_line(col = "#ffc107") +
            geom_line(data = data_mc,
                      aes(x = index,
                          y = lower
                          ),
                      col = "#fafafa"
                      ) +
            geom_line(data = data_mc,
                      aes(x = index,
                          y = upper
                          ),
                      col = "#fafafa"
                      ) +
                        theme(
                panel.background = element_rect(fill = "transparent", colour = NA),
                plot.background  = element_rect(fill = "transparent", colour = NA),
                text = element_text(color = "#FAFAFA",
                                    face  = "bold"),
                axis.ticks = element_blank(),
                axis.text  = element_text(color = "#FAFAFA",
                                          face  = "bold"),
                axis.title.y = element_text( vjust = 1.5),
                axis.title.x = element_text( vjust = .1)
                  )
    }, bg = "transparent")

    ## is_mc_plot
    output$is_mc_plot <- renderPlot({

        n     <- input$n_is 
        N     <- input$N_is
        alpha <- input$alpha
        m     <- input$m_is

        ## Initialization
        real     <- 1 - exp(-2 * m)                

        ## Estimations
        estim_mc <- c()  ##rep(0,N)
        estim_is <- c()  ##rep(0,N)

        ## Error
        error_mc <- c()  ##rep(0,N)
        error_is <- c()  ##rep(0,N)

        ## Bounds
        lower_mc <- c()  ##rep(0,N)
        upper_mc <- c()  ##rep(0,N)

        ## Bounds
        lower_is <- c()  ##rep(0,N)
        upper_is <- c()  ##rep(0,N)

        
        for(i in 1:N){

            ## MC
            U           <- runif(n, 0, 2) # Ojo: de 0 a 2
            phi         <- function(x) 2 * m * exp(- m * x^2)
            estim_mc[i] <- mean(phi(U))
            s1          <- var(phi(U))
            quant       <- qnorm(alpha / 2, lower.tail = FALSE)
            upper_mc[i] <- estim_mc[i] + sqrt(s1 / n) * quant  #intervalos de confianza
            lower_mc[i] <- estim_mc[i] - sqrt(s1 / n) * quant
            error_mc[i] <- 1 - exp(-2 * m) - estim_mc[i]
              
            U <- runif(n, 0, 1) # Ojo: de 0 a 1
            X <- -log(1 - (1 - exp(-2))*U)

            ## IS
            fun         <- function(x) dexp(x) / (1 - exp(-2)) # Densidad de lambda truncada
            phi         <- function(x) m * exp(-m * x^2) / fun(x)  #corrección importance sampling
            estim_is[i] <- mean(phi(X))
            s1          <- var(phi(X))
            quant       <- qnorm(alpha / 2, lower.tail = FALSE)
            upper_is[i] <- estim_is[i] + sqrt(s1 / n) * quant  #intervalos de confianza
            lower_is[i] <- estim_is[i] - sqrt(s1 / n) * quant
            error_is[i] <- 1 - exp(-2 * m) - estim_is[i]            
        }
        
        ## Data MC estimates
        data_mc    <- data.frame(estim = estim_mc,
                                upper = upper_mc,
                                lower = lower_mc,
                                index = 1:N)

        ## Data IS estimates
        data_is    <- data.frame(estim = estim_is,
                                upper = upper_is,
                                lower = lower_is,
                                index = 1:N)
        
        ## Data MC & IS errors
        data_mc_is <- rbind(
            data.frame(id = 'mc', error = error_mc),
            data.frame(id = 'is', error = error_is)
        )

       ggplot(data_mc_is) +
            geom_density(aes(error), col = "#FAFAFA") +
            facet_wrap( ~ id) +
                        theme(
                panel.background = element_rect(fill = "transparent", colour = NA),
                plot.background  = element_rect(fill = "transparent", colour = NA),
                text = element_text(color = "#FAFAFA",
                                    face  = "bold"),
                axis.ticks = element_blank(),
                axis.text  = element_text(color = "#FAFAFA",
                                          face  = "bold"),
                axis.title.y = element_text( vjust = 1.5),
                axis.title.x = element_text( vjust = .1)
                  )
    }, bg = "transparent")
}
shinyApp(ui, server)

