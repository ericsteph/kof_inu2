



rm(list = ls())
source("000_pacchetti.R", encoding = "UTF-8")
source("00_FUNZIONI_dashboard.R", encoding = "UTF-8")
source("00_FUNZIONI_x_figure_occupazione_attuale.R", encoding = "UTF-8")
source("00_FUNZIONI_genera.R", encoding = "UTF-8")
source("01_inu_variabili.R", encoding = "UTF-8")



options(knitr.table.format = "html")

knitr::opts_chunk$set(dev="png", fig.width=6, fig.height=1.85, fig.align='left', fig.path='Figs/',
                      echo=FALSE, warning=FALSE, message=FALSE)


# tmp <- dati_kof(d, vars.x, vars.dove.x, vars.settore.x)
# 
# z <- genera_flex(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
#                  domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)


tabella <- z$t
sintesi <- z$table
p1 <- z$p[[1]]
p2 <- z$p[[2]]
p3 <- z$p[[3]]

