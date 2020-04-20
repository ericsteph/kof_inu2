rm(list=ls())

# source("000_pacchetti.R", encoding = "UTF-8")
# source("00_FUNZIONI_dashboard.R", encoding = "UTF-8")
# source("00_FUNZIONI_x_figure_occupazione_attuale.R", encoding = "UTF-8")
# source("00_FUNZIONI_genera.R", encoding = "UTF-8")
source("01_inu_variabili.R", encoding = "UTF-8")
# source("02_FUNZ_buildup_Rmd_bau_x_regione.R", encoding = "UTF-8")
source("02_FUNZ_buildup_Rmd_inu.R", encoding = "UTF-8")

tmp <- "title: \"KOF, inchiesta congiunturale, INDUSTRIA (inu) | "
tmp2 <- "inu_flexdashboard."


# Creo un file .Rmd per il settore industriale, in Ticino
vars.settore.x <- vars.settore[1]
sotto.cap.x<- settori[1]
dove.x <- dove[1]
vars.dove.x <- vars.dove[1]

chunk02.x <- readLines(con = '03_inu_crea_Rmd.R', warn = FALSE)[14:20]

titolo_princ <- paste0(tmp, paste0(sotto.cap.x, ", ", dove.x), "\"")
nome_file <- paste0(tmp2, paste0(vars.dove.x, vars.settore.x), "Rmd")
crea.Rmd.inu(titolo_princ, chunk02.x, vars.settore.x, sotto.cap.x, dove.x, vars.dove.x, nome_file)


# Creo un file .Rmd per il settore industriale, industria interna (0-33%) in Ticino
vars.settore.x <- vars.settore[2]
sotto.cap.x<- settori[2]
dove.x <- dove[1]
vars.dove.x <- vars.dove[1]

chunk02.x <- readLines(con = '03_inu_crea_Rmd.R', warn = FALSE)[27:33]

titolo_princ <- paste0(tmp, paste0(sotto.cap.x, ", ", dove.x), "\"")
nome_file <- paste0(tmp2, paste0(vars.dove.x, vars.settore.x), "Rmd")
crea.Rmd.inu(titolo_princ, chunk02.x, vars.settore.x, sotto.cap.x, dove.x, vars.dove.x, nome_file)


# Creo un file .Rmd per il settore industriale, industria interna (66-100%) in Ticino
vars.settore.x <- vars.settore[3]
sotto.cap.x<- settori[3]
dove.x <- dove[1]
vars.dove.x <- vars.dove[1]

chunk02.x <- readLines(con = '03_inu_crea_Rmd.R', warn = FALSE)[40:46]

titolo_princ <- paste0(tmp, paste0(sotto.cap.x, ", ", dove.x), "\"")
nome_file <- paste0(tmp2, paste0(vars.dove.x, vars.settore.x), "Rmd")
crea.Rmd.inu(titolo_princ, chunk02.x, vars.settore.x, sotto.cap.x, dove.x, vars.dove.x, nome_file)


# Creo un file .Rmd per il settore industriale, aziende grandi, in Ticino
# vars.settore.x <- vars.settore[4]
# sotto.cap.x<- settori[4]
# dove.x <- dove[1]
# vars.dove.x <- vars.dove[1]
# 
# chunk02.x <- readLines(con = '03_inu_crea_Rmd.R', warn = FALSE)[53:59]
# 
# titolo_princ <- paste0(tmp, sotto.cap.x, "\"") 
# nome_file <- paste0(tmp2, vars.settore.x, "Rmd") 
# crea.Rmd.inu(titolo_princ, chunk02.x, vars.settore.x, sotto.cap.x, dove.x, vars.dove.x, nome_file)


# Creo un file .Rmd per il settore industriale, in Svizzera
vars.settore.x <- vars.settore[1]
sotto.cap.x<- settori[1]
dove.x <- dove[2]
vars.dove.x <- vars.dove[2]

chunk02.x <- readLines(con = '03_inu_crea_Rmd.R', warn = FALSE)[66:72]

titolo_princ <- paste0(tmp, paste0(sotto.cap.x, ", ", dove.x), "\"")
nome_file <- paste0(tmp2, paste0(vars.dove.x, vars.settore.x), "Rmd")
crea.Rmd.inu(titolo_princ, chunk02.x, vars.settore.x, sotto.cap.x, dove.x, vars.dove.x, nome_file)


# Creo un file .Rmd per il settore industriale, industria interna (0-33%) in Svizzera
vars.settore.x <- vars.settore[2]
sotto.cap.x<- settori[2]
dove.x <- dove[2]
vars.dove.x <- vars.dove[2]

chunk02.x <- readLines(con = '03_inu_crea_Rmd.R', warn = FALSE)[79:85]

titolo_princ <- paste0(tmp, paste0(sotto.cap.x, ", ", dove.x), "\"")
nome_file <- paste0(tmp2, paste0(vars.dove.x, vars.settore.x), "Rmd")
crea.Rmd.inu(titolo_princ, chunk02.x, vars.settore.x, sotto.cap.x, dove.x, vars.dove.x, nome_file)


# Creo un file .Rmd per il settore industriale, industria interna (66-100%) in Svizzera
vars.settore.x <- vars.settore[3]
sotto.cap.x<- settori[3]
dove.x <- dove[2]
vars.dove.x <- vars.dove[2]

chunk02.x <- readLines(con = '03_inu_crea_Rmd.R', warn = FALSE)[92:98]

titolo_princ <- paste0(tmp, paste0(sotto.cap.x, ", ", dove.x), "\"")
nome_file <- paste0(tmp2, paste0(vars.dove.x, vars.settore.x), "Rmd")
crea.Rmd.inu(titolo_princ, chunk02.x, vars.settore.x, sotto.cap.x, dove.x, vars.dove.x, nome_file)


# Creo un file .Rmd per il settore industriale, aziende grandi, in Svizzera
vars.settore.x <- vars.settore[4]
sotto.cap.x<- settori[4]
dove.x <- dove[2]
vars.dove.x <- vars.dove[2]

chunk02.x <- readLines(con = '03_inu_crea_Rmd.R', warn = FALSE)[105:111]

titolo_princ <- paste0(tmp, sotto.cap.x, "\"") 
nome_file <- paste0(tmp2, vars.settore.x, "Rmd") 
crea.Rmd.inu(titolo_princ, chunk02.x, vars.settore.x, sotto.cap.x, dove.x, vars.dove.x, nome_file)
#