# Funzione per creare il titolo
rmd_flex_kof <- function(titolo_princ, sottotitolo, data){
  
  titolo_princ
  sottotitolo
  data
  
  output <- c("output:", 
              "  flexdashboard::flex_dashboard:",
              "  social: menu",
              "  source: embed",
              "  storyboard: yes",
              "  theme: yeti",
              "  html_document: default"
  )
  
  doc <- c("---", titolo_princ, sottotitolo, data, output, "---")
  
  doc
}

# Funzione per creare la linea di inizio e di fine di un "chunk"
estremi.chunk <- function(x1, x2, y){
  line <- paste0("```", x1, y, x2)
  line
}

# Funzione per creare i titoli dello "storyboard"
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}


Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}


Titolo1 <- function(testo){
  testo <- Unaccent(testo)
  paste0(
    "# ", toupper(testo), " {.storyboard}"
  )
}


Titolo2 <- function(x1, x2, x3, x4){
  x1 <- Unaccent(x1)
  x2 <- Unaccent(x2)
  x3 <- Unaccent(x3)
  paste0(
    "### ", x1, "<br> **", toupper(x2), " | **", x3, " ... ... ... . ",
    "<br><br> *" , x4,"* {data-commentary-width=750}"
  )
}

# Funzione per creare "cambio sezione"
Cambio.s <- function(ultimo_mese, ultimo_anno, dove){
  lspec <- c("", "***", "", paste0("**KOF**, inchiesta congiunturale: ", ultimo_mese, " ", ultimo_anno, ", ", dove), "")
  lspec
}

# Linee vuote
linea.vuota <- ""
linea.vuota2 <- c("", "<br>", "")


genera_flex <- function(
  d, inchiesta,
  vars.x,
  dove.x, vars.dove.x , capitolo.x, 
  domanda.x, riferimento.x, 
  sotto.cap.x, ris.x, testi){
  
  dom.ris_f <- crea.dom.ris(sotto.cap.x, domanda.x, ris.x)
  
  titolo.x <- c(paste0(capitolo.x, " (", tolower(domanda.x),
                       ifelse(riferimento.x == "Situazione attuale", " della ",
                              ifelse(riferimento.x == "Ultimi tre mesi", " degli ", " ")),
                       tolower(riferimento.x),"), "))
  
  storyboard.Title1 <- Titolo1(testi[1])
  storyboard.Title2 <- Titolo2(testi[2], testi[3], testi[4], vars.x)
  storyboard.cambio <- Cambio.s(ultimo_mese, ultimo_anno, dove.x)
  
  # Tabella
  t <- out.tabella(d, titolo.x, dove.x, dom.ris_f)
  
  # Grafici
  p <- out.grafici(d, titolo.x, dove.x)
  
  table <- crea.tbl(capitolo.x, riferimento.x, sotto.cap.x, domanda.x, ris.x)
  
  output <- list(storyboard.Title1 = storyboard.Title1,
                 storyboard.Title2 = storyboard.Title2,
                 storyboard.cambio = storyboard.cambio,
                 t = t,
                 table = table,
                 p = p)
  output
}


genera_flex2 <- function(
  d, inchiesta,
  vars.x,
  dove.x, vars.dove.x , capitolo.x, 
  domanda.x, riferimento.x, 
  sotto.cap.x, ris.x, testi){
  
  dom.ris_f <- crea.dom.ris(sotto.cap.x, domanda.x, ris.x)
  
  titolo.x <- c(paste0(capitolo.x, " (", tolower(domanda.x),
                       ifelse(riferimento.x == "Situazione attuale", " della ",
                              ifelse(riferimento.x == "Ultimi tre mesi", " degli ", " per i ")),
                       tolower(riferimento.x),"), "))
  
  storyboard.Title1 <- Titolo1(testi[1])
  storyboard.Title2 <- Titolo2(testi[2], testi[3], testi[4], vars.x)
  storyboard.cambio <- Cambio.s(ultimo_mese, ultimo_anno, dove.x)
  
  # Tabella
  t <- out.tabella2(d, titolo.x, dove.x, dom.ris_f)
  
  # Grafici
  p <- out.grafici2(d, titolo.x, dove.x)
  
  table <- crea.tbl(capitolo.x, riferimento.x, sotto.cap.x, tolower(domanda.x), ris.x)
  
  output <- list(storyboard.Title1 = storyboard.Title1,
                 storyboard.Title2 = storyboard.Title2,
                 storyboard.cambio = storyboard.cambio,
                 t = t,
                 table = table,
                 p = p)
  output
}


genera_flex_SINT <- function(
  d, inchiesta,
  vars.x,
  dove.x, vars.dove.x , capitolo.x, 
  domanda.x, riferimento.x, 
  sotto.cap.x, ris.x, testi){
  
  dom.ris_f <- crea.dom.ris(sotto.cap.x, domanda.x, ris.x)
  
  titolo.x <- c(paste0(capitolo.x, " (", tolower(domanda.x),
                       ifelse(riferimento.x == "Situazione attuale", " della ",
                              ifelse(riferimento.x == "Ultimi tre mesi", " degli ", " ")),
                       tolower(riferimento.x),"), "))
  
  storyboard.Title1 <- Titolo1(testi[1])
  storyboard.Title2 <- Titolo2(testi[2], testi[3], testi[4], vars.x)
  storyboard.cambio <- Cambio.s(ultimo_mese, ultimo_anno, dove.x)
  
  # Tabella
  t <- out.tabella_SINT(d, titolo.x, dove.x, dom.ris_f)
  
  # Grafici
  p <- out.grafici_SINT(d, titolo.x, dove.x)
  
  table <- crea.tbl(capitolo.x, riferimento.x, sotto.cap.x, domanda.x, ris.x)
  
  output <- list(storyboard.Title1 = storyboard.Title1,
                 storyboard.Title2 = storyboard.Title2,
                 storyboard.cambio = storyboard.cambio,
                 t = t,
                 table = table,
                 p = p)
  output
}


# Assemblaggio del vettore character con il contenuto del file Rmd (flexdashboard)
flex_rmd1 <- function(z, livello, numero
                      # ,
                      # t, table, p1, p2, p3
                      ){

  source("00_FUNZIONI_genera.R", encoding = "UTF-8")
  linea.vuota <- ""
  linea.vuota2 <- c("", "<br>", "")
  
  
  if(livello == 1L){
    storyboard.Title1 <- z$storyboard.Title1
  }else{
    storyboard.Title1 <- NULL
  }
  
  storyboard.Title2 <- z$storyboard.Title2
  storyboard.cambio <- z$storyboard.cambio
  
  t_N <- paste0("t.", numero)
  table_N <- paste0("table.", numero)
  p_N1 <- paste0("p.", numero, ".", 1L)
  p_N2 <- paste0("p.", numero, ".", 2L)
  p_N3 <- paste0("p.", numero, ".", 3L)
  
  c(
    storyboard.Title1,
    linea.vuota,
    storyboard.Title2,
    linea.vuota,
    estremi.chunk("{r ", "}", t_N
    ),
    "tabella",
    estremi.chunk("", "", ""),
    linea.vuota,
    storyboard.cambio,
    linea.vuota,
    estremi.chunk("{r ", "}", table_N
    ),
    "sintesi",
    estremi.chunk("", "", ""),
    linea.vuota2,
    estremi.chunk("{r ", "}", p_N1
    ),
    "p1",
    estremi.chunk("", "", ""),
    linea.vuota2,
    estremi.chunk("{r ", "}", p_N2
    ),
    "p2",
    estremi.chunk("", "", ""),
    linea.vuota2,
    estremi.chunk("{r ", "}", p_N3
    ),
    "p3",
    estremi.chunk("", "", ""),
    linea.vuota
  )
}

flex_blocco00 <- function(x, y, z, q){
  blocco <- c(
    # Titolo yaml
    x,
    linea.vuota,
    # Pacchetti, funzioni,...
    estremi.chunk("{r ", "}", "setup, include = FALSE"),
    y,
    estremi.chunk("", "", ""),
    linea.vuota,
    estremi.chunk("{r ", "}", "global_options, include = FALSE"),
    z,
    estremi.chunk("", "", ""),
    linea.vuota,
    estremi.chunk("{r ", "}", "include = FALSE"),
    q,
    estremi.chunk("", "", ""),
    linea.vuota)
  
  blocco
}


flex_blocchi <- function(x, y){
  blocco <- c(
    estremi.chunk("{r ", "}", "include = FALSE"),
    x,
    estremi.chunk("", "", ""),
    linea.vuota,
    y,
    linea.vuota)
  
  blocco
}

#