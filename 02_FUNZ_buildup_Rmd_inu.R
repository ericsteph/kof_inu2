
file.x <- "02_FUNZ_buildup_Rmd_inu.R"


# x1 <- readLines(con = '02_chunk_base.R')[20:25]
x2 <- readLines(con = '02_chunk_base.R')[25:31]


crea.blocco <- function(y, x2){
  # source("02_chunk_base.R", encoding = "UTF-8")
  
  chunk.y <- readLines(file.x)[y]
  out.chunk <- c(chunk.y, x2)
  out.chunk

}


crea.Rmd.inu <- function(titolo_princ, chunk02.x,
                          vars.settore.x, sotto.cap.x,
                         dove.x, vars.dove.x,
                          nome_file){
  
source("000_pacchetti.R", encoding = "UTF-8")
source("00_FUNZIONI_genera.R", encoding = "UTF-8")
source("00_FUNZIONI_dashboard.R", encoding = "UTF-8")
source("00_FUNZIONI_x_figure_occupazione_attuale.R", encoding = "UTF-8")
  
### 1
## Creazione dei titoli dello yaml
doc <- rmd_flex_kof(titolo_princ,
                    "subtitle:",
                    "date:")

### 2
## Base: Funzioni, pacchetti e opzioni generali
chunk00 <- readLines("02_chunk_base.R")[4:11]
chunk01 <- readLines("02_chunk_base.R")[13:18]
# chunk02.x <- chunk02

inizio <- flex_blocco00(doc, chunk00, chunk01, chunk02.x)


### 3.1
# Situazione degli affari, situazione attuale
vars.x = vars.q1[1]
capitolo.x <- capitoli[1]
domanda.x <- capitoli_[1]
riferimento.x <- riferimenti[1]
ris.x <- risposte00[1, ]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.01 <- crea.blocco(43:58, x2)

blocco.01 <- flex_rmd1(z, 2L, "01")
blocco.01 <- flex_blocchi(chunk02.01, blocco.01)

### 3.2
# Volume degli ordini, situazione attuale
vars.x = vars.q1[2]
capitolo.x <- capitoli[3]
ris.x <- risposte00[3, ]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.02 <- crea.blocco(63:76, x2)

blocco.02 <- flex_rmd1(z, 2L, "02")
blocco.02 <- flex_blocchi(chunk02.02, blocco.02)

### 3.3
# Ordini dall'estero, situazione attuale
vars.x = vars.q1[3]
capitolo.x <- capitoli[4]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex2(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.03 <- crea.blocco(81:93, x2)

blocco.03 <- flex_rmd1(z, 2L, "03")
blocco.03 <- flex_blocchi(chunk02.03, blocco.03)


### 3.4
# Occupazione, situazione attuale
vars.x = vars.q1[4]
capitolo.x <- capitoli[8]
domanda.x <- capitoli_[4]
ris.x <- risposte03[3, ]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex2(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.04 <- crea.blocco(99:113, x2)

blocco.04 <- flex_rmd1(z, 2L, "04")
blocco.04 <- flex_blocchi(chunk02.04, blocco.04)


### 3.5
# Acquisizione di ordini, situazione attuale su base annua
vars.x = vars.q1[5]
capitolo.x <- capitoli[2]
riferimento.x <- riferimenti[5]
domanda.x <- capitoli_[6]
ris.x <- risposte00[2, ]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                  domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.05 <- crea.blocco(119:134, x2)

blocco.05 <- flex_rmd1(z, 2L, "05")
blocco.05 <- flex_blocchi(chunk02.05, blocco.05)


### 3.6
# Produzione, situazione attuale su base annua
vars.x = vars.q1[6]
capitolo.x <- capitoli[5]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                  domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.06 <- crea.blocco(140:152, x2)

blocco.06 <- flex_rmd1(z, 2L, "06")
blocco.06 <- flex_blocchi(chunk02.06, blocco.06)


### 3.7
# Stock prodotti finiti, situazione attuale
vars.x = vars.q1[7]
capitolo.x <- capitoli[6]
riferimento.x <- riferimenti[1]
ris.x <- risposte00[6, ]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex2(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.07 <- crea.blocco(158:172, x2)

blocco.07 <- flex_rmd1(z, 2L, "07")
blocco.07 <- flex_blocchi(chunk02.07, blocco.07)


### 3.07b
# Stock prodotti intermedi, situazione attuale
vars.x = vars.q1[8]
capitolo.x <- capitoli[7]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex2(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.07b <- crea.blocco(178:190, x2)

blocco.07b <- flex_rmd1(z, 2L, "07b")
blocco.07b <- flex_blocchi(chunk02.07b, blocco.07b)


### 3.07c
# Capacità tecniche, situazione attuale
vars.x = vars.q1[9]
capitolo.x <- capitoli[9]
ris.x <- risposte03[1, ]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof_trim(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex2(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.07c <- crea.blocco(196:209, x2)

blocco.07c <- flex_rmd1(z, 2L, "07c")
blocco.07c <- flex_blocchi(chunk02.07c, blocco.07c)


### 3.8
# Situazione reddituale, ultimi tre mesi
vars.x = vars.q2[1]
capitolo.x <- capitoli[13]
riferimento.x <- riferimenti[2]
domanda.x <- capitoli_[2]
ris.x <- risposte01[2, ]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof_trim(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.08 <- crea.blocco(215:230, x2)

blocco.08 <- flex_rmd1(z, 1L, "08")
blocco.08 <- flex_blocchi(chunk02.08, blocco.08)


### 3.9
# Prezzi di vendita, ultimi tre mesi
vars.x = vars.q2[2]
capitolo.x <- capitoli[10]
ris.x <- risposte03[5, ]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof_trim(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.09 <- crea.blocco(236:249, x2)

blocco.09 <- flex_rmd1(z, 2L, "09")
blocco.09 <- flex_blocchi(chunk02.09, blocco.09)


### 3.10
# Posizione concorrenziale (in Svizzera), ultimi tre mesi
vars.x = vars.q2[3]
capitolo.x <- capitoli[14]
ris.x <- risposte01[2, ]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof_trim(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.10 <- crea.blocco(255:268, x2)

blocco.10 <- flex_rmd1(z, 2L, "10")
blocco.10 <- flex_blocchi(chunk02.10, blocco.10)


### 3.11
# Posizione concorrenziale (all'interno dell'UE), ultimi tre mesi
vars.x = vars.q2[4]
capitolo.x <- capitoli[15]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof_trim(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.11 <- crea.blocco(274:286, x2)

blocco.11 <- flex_rmd1(z, 2L, "11")
blocco.11 <- flex_blocchi(chunk02.11, blocco.11)


### 3.12
# Posizione concorrenziale (all'esterno dell'UE), ultimi tre mesi
vars.x = vars.q2[5]
capitolo.x <- capitoli[16]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof_trim(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.12 <- crea.blocco(292:304, x2)

blocco.12 <- flex_rmd1(z, 2L, "12")
blocco.12 <- flex_blocchi(chunk02.12, blocco.12)


### 3.14
# Capacità tecniche, ultimi tre mesi
vars.x = vars.q2[6]
capitolo.x <- capitoli[9]
ris.x <- risposte03[6, ]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof_trim(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.14 <- crea.blocco(310:323, x2)

blocco.14 <- flex_rmd1(z, 2L, "14")
blocco.14 <- flex_blocchi(chunk02.14, blocco.14)


### 3.15
# Situazione degli affari, prossimi sei mesi
vars.x = vars.q3[1]
capitolo.x <- capitoli[1]
domanda.x <- capitoli_[3]
riferimento.x <- riferimenti[4]
ris.x <- risposte01[6, ]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof_trim(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.15 <- crea.blocco(329:344, x2)

blocco.15 <- flex_rmd1(z, 1L, "15")
blocco.15 <- flex_blocchi(chunk02.15, blocco.15)


### 3.16
# Volume degli ordini, prossimi tre mesi
vars.x = vars.q3[2]
capitolo.x <- capitoli[3]
riferimento.x <- riferimenti[3]
ris.x <- risposte02[3, ]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.16 <- crea.blocco(350:364, x2)

blocco.16 <- flex_rmd1(z, 2L, "16")
blocco.16 <- flex_blocchi(chunk02.16, blocco.16)


### 3.17
# Volume degli ordini dall'estero, prossimi tre mesi
vars.x = vars.q3[3]
capitolo.x <- capitoli[4]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof_trim(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.17 <- crea.blocco(370:382, x2)

blocco.17 <- flex_rmd1(z, 2L, "17")
blocco.17 <- flex_blocchi(chunk02.17, blocco.17)


### 3.18
# Occupazione, prossimi tre mesi
vars.x = vars.q3[4]
capitolo.x <- capitoli[8]
domanda.x <- capitoli_[5]
ris.x <- risposte02[3, ]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.18 <- crea.blocco(388:402, x2)

blocco.18 <- flex_rmd1(z, 2L, "18")
blocco.18 <- flex_blocchi(chunk02.18, blocco.18)


### 3.19
# Produzione, prossimi tre mesi
vars.x = vars.q3[5]
capitolo.x <- capitoli[5]
domanda.x <- capitoli_[3]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.19 <- crea.blocco(408:421, x2)

blocco.19 <- flex_rmd1(z, 2L, "19")
blocco.19 <- flex_blocchi(chunk02.19, blocco.19)


### 3.20
# Acquisto di prodotti non finiti, prossimi tre mesi
vars.x = vars.q3[6]
capitolo.x <- capitoli[12]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.20 <- crea.blocco(427:439, x2)

blocco.20 <- flex_rmd1(z, 2L, "20")
blocco.20 <- flex_blocchi(chunk02.20, blocco.20)


### 3.21
# Prezzi di vendita, prossimi tre mesi
vars.x = vars.q3[7]
capitolo.x <- capitoli[10]
ris.x <- risposte03[8, ]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof_trim(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.21 <- crea.blocco(445:458, x2)

blocco.21 <- flex_rmd1(z, 2L, "21")
blocco.21 <- flex_blocchi(chunk02.21, blocco.21)


### 3.22
# Prezzi di acquisto, prossimi tre mesi
vars.x = vars.q3[8]
capitolo.x <- capitoli[11]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof_trim(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex2(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.22 <- crea.blocco(464:476, x2)

blocco.22 <- flex_rmd1(z, 2L, "22")
blocco.22 <- flex_blocchi(chunk02.22, blocco.22)


### 3.23
# Acqusizione di ordini, sit.attuale vs. mese precedente
vars.x = vars.q4[1]
riferimento.x <- riferimenti[6]
capitolo.x <- capitoli[2]
domanda.x <- capitoli_[2]
ris.x <- risposte00[2, ]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.23 <- crea.blocco(482:497, x2)

blocco.23 <- flex_rmd1(z, 1L, "23")
blocco.23 <- flex_blocchi(chunk02.23, blocco.23)


### 3.24
# Volume degli ordini, sit.attuale vs. mese precedente
vars.x = vars.q4[2]
capitolo.x <- capitoli[3]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.24 <- crea.blocco(503:515, x2)

blocco.24 <- flex_rmd1(z, 2L, "24")
blocco.24 <- flex_blocchi(chunk02.24, blocco.24)


### 3.25
# Produzione, sit.attuale  vs. mese precedente
vars.x = vars.q4[3]
capitolo.x <- capitoli[5]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.25 <- crea.blocco(521:533, x2)

blocco.25 <- flex_rmd1(z, 2L, "25")
blocco.25 <- flex_blocchi(chunk02.25, blocco.25)


### 3.26
# Stock prodotti finiti, sit.attuale  vs. mese precedente
vars.x = vars.q4[4]
capitolo.x <- capitoli[6]
ris.x <- risposte03[4, ]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex2(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.26 <- crea.blocco(539:552, x2)

blocco.26 <- flex_rmd1(z, 2L, "26")
blocco.26 <- flex_blocchi(chunk02.26, blocco.26)


### 3.27
# Stock prodotti intermedi, sit.attuale vs. mese precedente
vars.x = vars.q4[5]
capitolo.x <- capitoli[7]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex2(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.27 <- crea.blocco(558:570, x2)

blocco.27 <- flex_rmd1(z, 2L, "27")
blocco.27 <- flex_blocchi(chunk02.27, blocco.27)


### 3.0
# Indicatore sintetico, situazione attuale
vars.x = vars.q1[10]
riferimento.x <- riferimenti[1]
capitolo.x <- capitoli[17]
domanda.x <- capitoli_[7]
ris.x <- risposte00[1, ]

testi <- c(riferimento.x, paste0(sotto.cap.x, ", ", dove.x), capitolo.x, riferimento.x)

tmp <- dati_kof_SINT(d, vars.x, vars.dove.x, vars.settore.x)

z <- genera_flex_SINT(tmp, inchiesta, vars.x, dove.x, vars.dove.x, capitolo.x,
                 domanda.x, riferimento.x, sotto.cap.x, ris.x, testi)

chunk02.00 <- crea.blocco(576:591, x2)

blocco.00 <- flex_rmd1(z, 1L, "00")
blocco.00 <- flex_blocchi(chunk02.00, blocco.00)


writeLines(c(inizio,
             blocco.00,
             blocco.01,
             blocco.02,
             blocco.03,
             blocco.04,
             blocco.05,
             blocco.06,
             blocco.07,
             blocco.07b,
             blocco.07c,
             blocco.08,
             blocco.09,
             blocco.10,
             blocco.11,
             blocco.12,
             # blocco.13,
             blocco.14,
             blocco.15,
             blocco.16,
             blocco.17,
             blocco.18,
             blocco.19,
             blocco.20,
             blocco.21,
             blocco.22,
             blocco.23,
             blocco.24,
             blocco.25,
             blocco.26,
             blocco.27,
             linea.vuota
),
con = nome_file)

}

# rmarkdown::render('FVU_rgn.ti_dmnd.Rmd')
