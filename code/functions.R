###################################################
## Las rutinas en este script, tienen como finalidad
## automatizar el proceso de validación de datos.
## Se revisan distintos tipos de datos como fechas,
## entidades federativas y coordenadas.
###################################################


###################################################
## Librerías utilizadas
###################################################
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(xlsx))
suppressPackageStartupMessages(library(stringdist))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(data.table))


###################################################

###################################################
##---------------------------------
## Funciones
##---------------------------------
###################################################

###################################################
##---------------------------------
## most_simil
##---------------------------------
###################################################
most_simil <- function(char1, char2){
    ## Identifica similaridad entre caracteres usando diversas métricas
    ## OSA (Restricted Damerau-Levenshtein): inserciones, eliminaciones,
    ## reemplazos y transposiciones pero cada subcadena sólo puede ser
    ## editada una vez.
    ## LV (Levenshtein): inserciones, eliminaciones y reemplazos.
    ## DL (FULL Damerau-Levenshtein): inserciones, eliminaciones,
    ## reemplazos y transposiciones (menor o igual a Levenshtein).
    ## LCS: subcadena de caracteres más larga en común
    max_str_length <- max(str_length(char1), str_length(char2))
    max_unique_length <- max(
        length(unique(str_split(char1, "")[[1]])),
        length(unique(str_split(char2, "")[[1]]))
    )
    ## Elección de una q razonable: en fechas 3
    ## PENDIENTE
    ## hagamos que todas las métricas estén entre 0,1
    res <-     list(
        osa       = 1 - stringdist(char1, char2, method = "osa") / max_str_length,
        lv        = 1 - stringdist(char1, char2, method = "lv")  / max_str_length,
        dl        = 1 - stringdist(char1, char2, method = "dl")  / max_str_length,
        lcs       = 1 - stringdist(char1, char2, method = "lcs") / max_str_length,
        ## Es necesario usar el máximo con 0
        ## ya que cuando alguna de las cadenas es
        ## vacía, se va a -Inf.
        cosine    = max(1 - stringdist(char1, char2, method = "cosine", q = 3),0),
        jaccard   = max(1 - stringdist(char1, char2, method = "jaccard", q = 3),0)
    )
    max(unlist(res))
}

###################################################
##---------------------------------
## most_simil_mult
##---------------------------------
###################################################
most_simil_mult <- function(char, bag){
    ##--------------------------------------------------------------
    ## Devuelve una lista con los siguientes elementos:
    ## 1.- Índice máximo de similitud entre bag y char.
    ## 2.- El caracter dentro de bag que alcanzó dicho índice.
    ## 3.- El índice de dicho caracter.
    ##--------------------------------------------------------------
    ## Primera Iteración, los que tienen los elementos más parecidos.
    simil   <- laply(bag, function(t)t <- (1 - stringdist(char,
                                                        t,
                                                        method = "jaccard",
                                                        q = 1)))
    indexes <- which(simil == max(simil))
    res_1   <-
    list(simil = max(simil),
         char  = unique(bag[indexes]))
    ## Segunda Iteración, combinación de distancias.
    simil <- laply(res_1[[2]],
                  function(t)t <- most_simil(char, t))
    index <- which(simil == max(simil))[1]
    char  <- res_1[[2]][index]
    index <- indexes[index]
    res   <-
        list(simil = max(simil),
             index = index,
             char  = char)
    ## Devolvemos resultados
    res
}

###################################################
############### Sección 1: FECHAS #################
###################################################
## Código que identifica, revisa y corrige fechas
###################################################
## Base de fechas correctas
date_base <- seq(as.Date("1980-01-01"),
                today(),
                "days")

###################################################
##---------------------------------
## transform_month
##---------------------------------
###################################################
transform_month <- function(month){
    bag_months <- list( "01" = c("Enero", "ene."),
                       "02" = c("Febrero", "feb."),
                       "03" = c("Marzo", "mar."),
                       "04" = c("Abril", "abr."),
                       "05" = c("Mayo", "may."),
                       "06" = c("Junio", "jun."),
                       "07" = c("Julio", "jul."),
                       "08" = c("Agosto", "ag."),
                       "09" = c("Septiembre", "sep."),
                       "10" = c("Octubre", "oct."),
                       "11" = c("Noviembre", "nov."),
                      "12"  = c("Diciembre","dic.")
                      )
    ## bolsa de meses
    off_months <- unlist(bag_months)
    ## Obtener la entidad que se parece más
    most_like <- most_simil_mult(month, off_months)$char
    ## Obtener índice
    clave <- names(bag_months)[laply(bag_months, function(t){ t <- most_like %in% t})]
    list("Mes" = clave, "Nombre" = bag_months[clave][[1]][1])
}

###################################################
##---------------------------------
## date_pre_proc
##---------------------------------
###################################################
date_pre_proc <- function(date){
    if(str_detect(date,"([a-z]|[A-Z])")){
        month <- transform_month(date)$Mes
        date  <- str_replace(date, "([a-z]|[A-Z])", month)
        date  <- str_replace_all(date, "([a-z]|[A-Z])", "")
    }
    date <- str_split(date, "([[:punct:]]|[[:space:]])")[[1]]
    date <- date[order(date, str_length(date), decreasing = TRUE)]
    date <- date[str_length(date) > 0]
    list("date1" = paste0(date,collapse = "-"),
         "date2" = paste0(date[c(1, 3, 2)], collapse = "-"))
}

###################################################
##---------------------------------
## transform.date
##---------------------------------
###################################################
transform.date <- function(date, dates = date_base){
    date <- date_pre_proc(date)
    list("possible_date1" = head(most_simil_mult(date$date1, dates)$char, 1),
         "possible_date2" = head(most_simil_mult(date$date2, dates)$char, 1)
         )
}


####################################################
################ PRUEBA SECCIÓN 1 ##################
transform.date("10 de enero 2015")
transform.date("12/dic/2014")
transform.date("12/15/2004")


####################################################
############## Sección 2: ENTIDADES ################
####################################################
## Código que identifica, revisa y corrige entidades
####################################################
####################################################
entities <- list(
    "01" = "Aguascalientes",
    "02" = "Baja California",
    "03" = "Baja California Sur",
    "04" = "Campeche",
    "05" = c("Coahuila de Zaragoza","Coahuila"),
    "06" = "Colima",
    "07" = "Chiapas",
    "08" = "Chihuahua",
    "09" = "Distrito Federal",
    "10" = "Durango",
    "11" = "Guanajuato",
    "12" = "Guerrero",
    "13" = "Hidalgo",
    "14" = "Jalisco",
    "15" = "México",
    "16" = c("Michoacán de Ocampo", "Michoacán"),
    "17" = "Morelons",
    "18" = "Nayarit",
    "19" = "Nuevo León",
    "20" = "Oaxaca",
    "21" = "Puebla",
    "22" = "Querétaro",
    "23" = "Quintana Roo",
    "24" = "San Luis Potosí",
    "25" = "Sinaloa",
    "26" = "Sonora",
    "27" = "Tabasco",
    "28" = "Tamaulipas",
    "29" = "Tlaxcala",
    "30" = c("Veracruz de Ignacio de la Llave","Veracruz"),
    "31" = "Yucatán",
    "32" = "Zacatecas"
)

full_ent <- read.csv(
    "https://raw.githubusercontent.com/lromang/IDMX/master/data/coords.csv",
    stringsAsFactors = FALSE,
    header = TRUE,
    colClasses = rep("character", 6),
    encoding = "UTF-8"
)

###################################################
##---------------------------------
## transform.all
##---------------------------------
###################################################
transform.all <- function(entity, mun = NA,
                         loc = NA, bag_entities = entities,
                         full_bag = full_ent){
    ##--------------------------------------------------------------
    ## Recibe una entidad federativa y opcionalment un municipio y una localidad
    ## los transforma a su clave INEGI  y nombre más probable, dada esa entidad federativa.
    ## IN
    ## entity: Entidad federativa la cual se quiere transformar.
    ## mun: Municipio el cual se quiere transformar.
    ## loc: Localidad la cual se quiere transformar.
    ## OUT
    ## data.frame con la clasificación correspondiente
    ##--------------------------------------------------------------
    mun_name <- NA
    mun_key  <- NA
    loc_name <- NA
    loc_key  <- NA
    off_entities  <- tolower(unlist(bag_entities))
    ## Obtener la entidad que se parece más.
    most_like     <- most_simil_mult(tolower(entity), off_entities)$char
    ## Obtener índice entidad.
    clave         <- names(bag_entities)[laply(bag_entities,
                                              function(t){t <- most_like %in% tolower(t)})]
    ## Obtener municipio al que se parece más, en caso de que haya.
    if(!is.na(mun)){
        filt_mun      <- dplyr::filter(full_bag, ENTIDAD == clave)
        most_like_mun <- most_simil_mult(tolower(mun), unique(filt_mun$NOM_MUN))$index
        ## Obtener clave y nombre municipio
        mun_name <- unique(filt_mun$NOM_MUN)[most_like_mun]
        mun_key  <- filt_mun$MUN[filt_mun$NOM_MUN == mun_name][1]
        ## Obtener localidad a la que se parece más, en caso de que haya.
        if(!is.na(loc)){
            filt_mun_loc  <- dplyr::filter(filt_mun, MUN == mun_key)
            most_like_loc <- most_simil_mult(tolower(loc), unique(filt_mun_loc$NOM_LOC))$index
            ## Obtener clave y nombre municipio
            loc_name <- unique(filt_mun_loc$NOM_LOC)[most_like_loc]
            loc_key  <- filt_mun_loc$LOC[filt_mun_loc$NOM_LOC == loc_name][1]
        }
    }
    ## Regresar resultados
    data.frame("entidad"         = bag_entities[clave][[1]][1],
               "clave_entidad"   = clave,
               "municipio"       = mun_name,
               "clave_municipio" = mun_key,
               "localidad"       = loc_name,
               "clave_localidad" = loc_key
               )
}

####################################################
################ PRUEBA SECCIÓN 2 ##################
transform.all("CAMPECHE")
transform.all("mchcan")
transform.all("oxc", "sn. agst. chayu")
transform.all("oxc", "sn. agst. chayu", "col. sn. flipe")
