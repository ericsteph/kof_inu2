
color1 <- brewer.pal(7, "YlOrRd")
color2 <- brewer.pal(7, "YlGnBu")
color3 <- brewer.pal(11, "RdYlGn")
grigi <- brewer.pal(9, "Greys")


# KOF, consumi

crea.dom.ris <- function(sotto.cap, domanda, risposta){
tmp <- c(paste0(sotto.cap, ", ", domanda, ":"),
         risposta[1],
         risposta[3]
)

tmp

}


sel.kof.vars <- function(a, x){
  
  kof <- paste0("ch.kof.", a, ".ng08.fx.")
  
  vars <- c(paste0(kof, x, ".share_pos"),
            paste0(kof, x, ".share_neg"),
            paste0(kof, x, ".balance")
  )
  
  vars
}


dati_kof <- function(d, vars.x, vars.dove.x, vars.settore.x
                     # , n
                     )
  {
  
  vars2 <- paste0(vars.dove.x, paste0(vars.settore.x, vars.x))
  
  vars2 <- sel.kof.vars(inchiesta, vars2)
  
  d$anno <- as.integer(d$anno)
  d1 <- d %>% dplyr::filter(anno >= primo_anno)
  # k <- d$anno >= primo_anno
  # d <- d[k, ]
  
  d1 <- d1 %>% dplyr::filter(series %in% vars2,
                           periodo %in% c("01", "04", "07", "10"))
  # d <- d[d$series %in% vars & d$periodo %in% c("01", "04", "07", "10"), ]

  d1$anno <- factor(d1$anno, levels = c(primo_anno:ultimo_anno))

  d1$periodo <- factor(d1$periodo, levels = c("01", "04", "07", "10"))
  levels(d1$periodo) <- c("gennaio", "aprile", "luglio", "ottobre")

  d1$value <- round(d1$value, digits = 1L)

  d_cast <- d1 %>%
    dcast( interaction(periodo, anno, sep = " ") ~ series , value.var = "value" )

  # d_cast <- d_cast[(nrow(d_cast) - n):nrow(d_cast),]

  colnames(d_cast) <- c("Inchiesta", "Balance", "Neg", "Pos")

  d_cast
  
}

dati_kof_SINT <- function(d, vars.x, vars.dove.x, vars.settore.x
                     # , n
)
{
  
  vars2 <- paste0(vars.dove.x, paste0(vars.settore.x, vars.x))
  
  vars2 <- sel.kof.vars(inchiesta, vars2)
  
  d$anno <- as.integer(d$anno)
  d1 <- d %>% dplyr::filter(anno >= primo_anno)
  # k <- d$anno >= primo_anno
  # d <- d[k, ]
  
  d1 <- d1 %>% dplyr::filter(series %in% vars2,
                             periodo %in% c("01", "04", "07", "10"))
  # d <- d[d$series %in% vars & d$periodo %in% c("01", "04", "07", "10"), ]
  
  d1$anno <- factor(d1$anno, levels = c(primo_anno:ultimo_anno))
  
  d1$periodo <- factor(d1$periodo, levels = c("01", "04", "07", "10"))
  levels(d1$periodo) <- c("gennaio", "aprile", "luglio", "ottobre")
  
  d1$value <- round(d1$value, digits = 1L)
  
  d_cast <- d1 %>%
    dcast( interaction(periodo, anno, sep = " ") ~ series , value.var = "value" )
  
  # d_cast <- d_cast[(nrow(d_cast) - n):nrow(d_cast),]
  
  colnames(d_cast) <- c("Inchiesta", "Balance")
  
  d_cast
  
}

dati_kof_trim <- function(d, vars.x, vars.dove.x, vars.settore.x
                     # , n
){
  
  vars2 <- paste0(vars.dove.x, paste0(vars.settore.x, vars.x))
  
  vars2 <- sel.kof.vars(inchiesta, vars2)
  
  d$anno <- as.integer(d$anno)
  d2 <- d %>% dplyr::filter(anno >= primo_anno)
  # k <- d$anno >= primo_anno
  # d <- d[k, ]
  
  d2 <- d2 %>% dplyr::filter(series %in% vars2,
                           periodo %in% c("Q1", "Q2", "Q3", "Q4"))
  # d <- d[d$series %in% vars & d$periodo %in% c("01", "04", "07", "10"), ]
  
  d2$anno <- factor(d2$anno, levels = c(primo_anno:ultimo_anno))
  
  d2$periodo <- factor(d2$periodo, levels = c("Q1", "Q2", "Q3", "Q4"))
  levels(d2$periodo) <- c("gennaio", "aprile", "luglio", "ottobre")
  
  d2$value <- round(d2$value, digits = 1L)
  
  d_cast <- d2 %>%
    dcast( interaction(periodo, anno, sep = " ") ~ series , value.var = "value" )
  
  # d_cast <- d_cast[(nrow(d_cast) - n):nrow(d_cast),]
  
  colnames(d_cast) <- c("Inchiesta", "Balance", "Neg", "Pos")
  
  d_cast
  
}



dati.tndz <- function(d1, d){
  d <- data.frame(d)
  d <- d %>% dplyr::mutate( tmp1 = lag(d[], n = 1L))
  d <- d %>% dplyr::mutate( tmp2 = lag(d[], n = 2L)) 
  d <- d %>% dplyr::mutate( tmp3 = lag(d[], n = 3L)) 
  d <- d %>% dplyr::mutate( tmp4 = lag(d[], n = 4L))
  
  d$var1 <- d[,1] - d$tmp1
  d$var1.x <- ifelse(d$var1 > 0, "TRUE", "FALSE")
  
  d$var2 <- d[,1] - d$tmp4
  d$var2.x <- ifelse(d$var2 > 0, "TRUE", "FALSE")
  
  
  d <- d[(5:nrow(d)),]
  
  for(i in(1:nrow(d))){
    d$media[i] <-  ( round( (mean(c(d[i,1], d$tmp1[i], d$tmp2[i], d$tmp3[i]))) / 10, digits = 1L) ) * 10
    d$tmp[i] <- d[i,1] - d$media[i]
    d$media.x[i] <- ifelse( d$tmp[i] > 0, "TRUE", "FALSE")
  }
  
  
  d[,1] <- NULL
  d$tmp <- NULL
  d$tmp1 <- NULL
  d$tmp2 <- NULL
  d$tmp3 <- NULL
  d$tmp4 <- NULL
  
  d1 <- d1[(5:nrow(d1)),]
  
  d1 <- bind_cols(d1, d)
  
  d1
}



unit.scale = function(x) (x - 1.5) / 100

crea.tabella <-   function(d, titolo, dom.ris){

  k <- nrow(d)
  
  colnames(d) <- c("Inchiesta", "Balance", "Neg", "Pos", "Var.pos.3m",
                   "var1.x", "Var.pos.12m", "var2.x", "Media.pos", "vs.t0")

  d <-   d %>% dplyr::select(Inchiesta, Balance, Neg, Pos,
                             Media.pos, vs.t0, Var.pos.3m, Var.pos.12m)
  
  d <- formattable::format_table(
    d,
    digits = 1L,
    align = c("l", "c", "r", "l", "c","l", rep("c", 2)),
    format = "html",
    caption = titolo,
    row_label_position = "l",
    formatters = list(

      Balance = color_tile("white", color3[5]
      ),
      Neg = color_bar(grigi[2], fun = unit.scale
      ),
      Pos = color_bar(color1[4], fun = unit.scale
      ),
      Media.pos = color_tile("white", grigi[5]
      ),
      vs.t0 = formatter(
        "span",
        style = x ~ style(color = ifelse(x,  "", grigi[5])),
        x ~ icontext(ifelse(x, "", "thumbs-down"), ifelse(x, "", ""))
      ),
      Var.pos.3m = formatter(
        "span", style = x ~ style(color = ifelse(rank(-x, ties.method = "max") <= 2, color3[11],
                                                 ifelse(rank(-x, ties.method = "max") > k -2, color3[1], grigi[4]))),
        x ~ sprintf("%.1f (rank: %02d)", x, rank(-x, ties.method = "max")),
        x ~ icontext(ifelse(x > 5, "arrow-up",
                            ifelse(x < -5, "arrow-down", "")))
      ),
      Var.pos.12m = formatter(
        "span", style = x ~ style(color = ifelse(rank(-x, ties.method = "max") <= 2, color3[11],
                                                 ifelse(rank(-x, ties.method = "max") > k -2, color3[1], grigi[4]))),
        x ~ sprintf("%.1f (rank: %02d)", x, rank(-x, ties.method = "max")),
        x ~ icontext(ifelse(x > 5, "arrow-up",
                            ifelse(x < -5, "arrow-down", "")))
      )
    )) %>%
  kable_styling("hover", full_width = F, position = "left") %>%
  row_spec(0, bold = F) %>%
  add_header_above(header = c("Indicatori classici" = 4, "Nuovi indicatori" = 4)) %>%
  # group_rows("2019", 12, 13) %>% # Come align : "links"
  column_spec(1, width = "4cm") %>%
  column_spec(2, bold = T, width = "2.5cm") %>%
  column_spec(3, width = "5cm")  %>%
  column_spec(4, width = "5cm")  %>%
  column_spec(5, width = "1.5cm") %>%
  column_spec(6, width = "0.5cm") %>%
  footnote(number = c(
                      "Balance: Differenza tra la quota di risposte positive meno quella di risposte negative",
                      paste0("Pos: ", dom.ris[[1]], " ... ", dom.ris[[2]]),
                      paste0("Neg: ", dom.ris[[1]], " ... ", dom.ris[[3]])
                        ),
           alphabet = c(
                     "Media.pos: Media delle risposte positive nelle ultime quattro inchieste",
                     "vs.t0: Risposte positive nell'ultima inchiesta rispetto alla media",
                     "Var.pos.3m: Tendenza delle risposte positive rispetto all'inchieste precedente",
                     "Var.pos.12m: Tendenza delle risposte positive rispetto all'inchiesta nello stesso periodo un anno prima"),
            number_title = "Indicatori classici",
            alphabet_title = "Nuovi indicatori"
           )
  d
  
}



out.tabella <- function(d1, domanda, dove, dom.ris){

  d2 <- dati.tndz(d1, d1[, 4])
  
  titolo <- paste0(domanda, dove, ", da ", d2[1, 1], " a ", d2[nrow(d2), 1])
  
  tmp <- crea.tabella(d2, titolo, dom.ris)
  
  tmp
  
}


crea.gauge.balance <-   function(d1, titolo, sottotitolo){

a1 <- quantile(d1[, 2], probs = 0.2, na.rm = TRUE)
a2 <- quantile(d1[, 2], probs = 0.4, na.rm = TRUE)
a3 <- quantile(d1[, 2], probs = 0.6, na.rm = TRUE)
a4 <- quantile(d1[, 2], probs = 0.8, na.rm = TRUE)

for(i in(1:nrow(d1))){
ifelse(d1[i, 2] < 0, d1$tmp1[i] <- d1[i, 2],
  d1$tmp1[i] <- 0)
}

for(i in(1:nrow(d1))){
  ifelse(d1[i, 2] < 0, d1$tmp2[i] <- 0,
         d1$tmp2[i] <- d1[i, 2])
}


d1 <- d1 %>% dplyr::mutate(gruppo = ifelse(d1[, 2] < a1, "male",
                                           ifelse(d1[, 2] >= a1 & d1[, 2] < a2, "rel. male",
                                             ifelse(d1[, 2] >= a2 & d1[, 2] < a3, "neutro",
                                               ifelse(d1[, 2] >= a3 & d1[, 2] < a4, "rel. bene", "bene")
                                               ))))

d1[, ncol(d1)] <- factor(d1[, ncol(d1)], levels = c("male", "rel. male", "neutro", "rel. bene", "bene"))

d1 <- d1[(nrow(d1)-3):nrow(d1), ]

p <- ggplot(d1, aes(fill = gruppo, ymin = tmp1, ymax = tmp2, xmax = 8, xmin = 3)) +
    geom_rect(aes(ymin = -100, ymax = tmp1, xmax = 8, xmin = 3), fill = grigi[2]) +
    geom_rect(aes(ymin = tmp2, ymax = 100, xmax = 8, xmin = 3), fill = grigi[2]) +
    geom_rect() +
    coord_polar(theta = "y", start = -pi/2) +
    xlim(c(-5, 8)) +
    ylim(c(-100, 300)) +
    geom_text(aes(x = -5, y = 200, label = d1[, 2], colour = gruppo), size = 5.5) +
    geom_text(aes(x = -1, y = 200, label = d1[, 1]), size = 2.8) +
    facet_wrap(~d1[, 1], nrow = 1) +
    scale_fill_manual(values = c(color3[1], color3[2], color3[4], color3[10], color3[11]), limits = levels(d1$gruppo),
                      labels = c("peggiore", "", "media", "", "migliore"),
                      name = "Posizione relativa\nrispetto alle ultime inchieste") +
    scale_colour_manual(values = c(color3[1], color3[2], color3[4], color3[10], color3[11]), limits = levels(d1$gruppo)) +
    labs(title = titolo, subtitle = sottotitolo) + guides(color=FALSE)

p

}

crea.gauge.pos <-   function(d1, titolo, sottotitolo){

  a1 <- quantile(d1[, 4], probs = 0.2, na.rm = TRUE)
  a2 <- quantile(d1[, 4], probs = 0.4, na.rm = TRUE)
  a3 <- quantile(d1[, 4], probs = 0.6, na.rm = TRUE)
  a4 <- quantile(d1[, 4], probs = 0.8, na.rm = TRUE)
  
  d1 <- d1 %>% dplyr::mutate(tmp = 100 - d1[, 4])
  
  d1 <- d1 %>% dplyr::mutate(gruppo = ifelse(d1[, 4] < a1, "male",
                                             ifelse(d1[, 4] >= a1 & d1[, 4] < a2, "rel. male",
                                                    ifelse(d1[, 4] >= a2 & d1[, 4] < a3, "neutro",
                                                           ifelse(d1[, 4] >= a3 & d1[, 4] < a4, "rel. bene", "bene")
                                                    ))))
  
  d1[, ncol(d1)] <- factor(d1[, ncol(d1)], levels = c("male", "rel. male", "neutro", "rel. bene", "bene"))
  
  d1 <- d1[(nrow(d1)-3):nrow(d1), ]
  
  p <- ggplot(d1, aes(fill = gruppo, ymax = d1[, 4], ymin = 0, xmax = 8, xmin = 3)) +
    geom_rect(aes(ymax = (d1[, 4] + tmp), ymin = 0, xmax = 8, xmin = 3), fill = grigi[2]) +
    geom_rect(aes(ymax = (d1[, 4] + tmp), ymin = 0, xmax = 8, xmin = 3), fill = grigi[2]) +
    geom_rect() +
    coord_polar(theta = "y", start = -pi/2) +
    xlim(c(-5, 8)) +
    ylim(c(0, 200)) +
    geom_text(aes(x = -5, y = 150, label = paste0(d1[, 4], "%"), colour = gruppo), size = 5.5) +
    geom_text(aes(x = -1, y = 150, label = d1[, 1]), size = 2.8) +
    facet_wrap(~d1[, 1], nrow = 1) +
    scale_fill_manual(values = c(color3[1], color3[2], color3[4], color3[10], color3[11]), limits = levels(d1$gruppo),
                      labels = c("peggiore", "", "media", "", "migliore"),
                      name = "Posizione relativa\nrispetto alle ultime inchieste") +
    scale_colour_manual(values = c(color3[1], color3[2], color3[4], color3[10], color3[11]), limits = levels(d1$gruppo)) +
    labs(title = titolo, subtitle = sottotitolo) + guides(color=FALSE)
  
  p
  
}



crea.gauge.neg <-   function(d1, titolo, sottotitolo){

  a1 <- quantile(d1[, 3], probs = 0.2, na.rm = TRUE)
  a2 <- quantile(d1[, 3], probs = 0.4, na.rm = TRUE)
  a3 <- quantile(d1[, 3], probs = 0.6, na.rm = TRUE)
  a4 <- quantile(d1[, 3], probs = 0.8, na.rm = TRUE)
  
  d1 <- d1 %>% dplyr::mutate(tmp = 100 - d1[, 3])
  
  d1 <- d1 %>% dplyr::mutate(gruppo = ifelse(d1[, 3] < a1, "bene",
                                             ifelse(d1[, 3] >= a1 & d1[, 3] < a2, "rel. bene",
                                                    ifelse(d1[, 3] >= a2 & d1[, 3] < a3, "neutro",
                                                           ifelse(d1[, 3] >= a3 & d1[, 3] < a4, "rel. male", "male")
                                                    ))))
  
  d1[, ncol(d1)] <- factor(d1[, ncol(d1)], levels = c("male", "rel. male", "neutro", "rel. bene", "bene"))
  
  d1 <- d1[(nrow(d1)-3):nrow(d1), ]
  
  p <- ggplot(d1, aes(fill = gruppo, ymax = 100, ymin = tmp, xmax = 8, xmin = 3)) +
    geom_rect(aes(ymax = tmp, ymin = 0, xmax = 8, xmin = 3), fill = grigi[2]) +
    geom_rect() +
    coord_polar(theta = "y", start = -pi/2) +
    xlim(c(-5, 8)) +
    ylim(c(0, 200)) +
    geom_text(aes(x = -5, y = 150, label = paste0(d1[, 3] * (-1), "%"), colour = gruppo), size = 5.5) +
    geom_text(aes(x = -1, y = 150, label = d1[, 1]), size = 2.8) +
    facet_wrap(~d1[, 1], nrow = 1) +
    scale_fill_manual(values = c(color3[1], color3[2], color3[4], color3[10], color3[11]), limits = levels(d1$gruppo),
                      labels = c("peggiore", "", "media", "", "migliore"),
                      name = "Posizione relativa\nrispetto alle ultime inchieste") +
    scale_colour_manual(values = c(color3[1], color3[2], color3[4], color3[10], color3[11]), limits = levels(d1$gruppo)) +
    labs(title = titolo, subtitle = sottotitolo) + guides(color=FALSE)
  
  p
  
}

tema1 <-  theme_void() + theme(
                legend.title = element_text(size = 7),
                legend.position = c(0.55, 0.15),
                legend.direction = "horizontal",
                legend.text = element_text(size = 6),
                legend.key.size = unit(0.3, "cm"),
                legend.key.width = unit(0.8, "cm"),
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                plot.background = element_rect(fill = "#fbfbfb", colour = NA),
                plot.title = element_text(size = 8, face = "bold"),
                plot.subtitle = element_text(size = 8)
                )

tema2 <- theme_void() + theme(
               legend.title = element_blank(),
               legend.position = "none",
               strip.background = element_blank(),
               strip.text.x = element_blank(),
               plot.background = element_rect(fill = "#fbfbfb", colour = NA),
               plot.title = element_text(size = 8, face = "bold"),
               plot.subtitle = element_text(size = 8)
               )

      
out.grafici <- function(d1, domanda, dove){
  titolo1 <- "Balance (Risposte positive meno Risposte negative)"
  titolo2 <- "Risposte positive, in %"
  titolo3 <- "Risposte negative, in %"
  
  sottotitolo <- paste0(domanda, dove)
  
  p1 <- crea.gauge.balance(d1, titolo1, sottotitolo)
  
  p1 <- p1 + tema1  + guides(fill = guide_legend(title.position = "left", label.position = "top"))
  
  p2 <- crea.gauge.pos(d1, titolo2, sottotitolo)
  
  p2 <- p2 + tema1  + guides(fill = guide_legend(title.position = "left", label.position = "top"))

  p3 <- crea.gauge.neg(d1, titolo3, sottotitolo)
  
  p3 <- p3 + tema1  + guides(fill = guide_legend(title.position = "left", label.position = "top"))
  
  p <- list(p1, p2, p3)
  
}


crea.tbl <- function(capitolo, riferimento, sotto.cap, domanda, ris){
text_tbl <- data.frame(
  tmp = c(
    paste0(sotto.cap, " ... ", tolower(domanda), " : "),
    paste0("--> ", ris[1,1], "  /  ", ris[1,2], "  /  ", ris[1,3])
    )
  )

colnames(text_tbl) <- capitolo

mini.tbl <- kable(text_tbl,
                  "html",
                  caption = riferimento
                  ) %>%
row_spec(1, italic = TRUE)  %>%
column_spec(1, background = "#fbfbfb")

mini.tbl
  
}

crea.tabella_SINT <-   function(d, titolo, dom.ris){
  
  k <- nrow(d)
  
  colnames(d) <- c("Inchiesta", "Balance", "Var.bal.3m",
                   "var1.x", "Var.bal.12m", "var2.x", "Media.bal", "vs.t0")
  
  d <-   d %>% dplyr::select(Inchiesta, Balance,
                             Media.bal, vs.t0, Var.bal.3m, Var.bal.12m)
  
  d <- formattable::format_table(
    d,
    digits = 1L,
    align = c("l", "c", "r", "l", "c","l", rep("c", 2)),
    format = "html",
    caption = titolo,
    row_label_position = "l",
    formatters = list(
      
      Balance = color_tile("white", color3[5]
      ),
      Media.bal = color_tile("white", grigi[5]
      ),
      vs.t0 = formatter(
        "span",
        style = x ~ style(color = ifelse(x,  "", grigi[5])),
        x ~ icontext(ifelse(x, "", "thumbs-down"), ifelse(x, "", ""))
      ),
      Var.bal.3m = formatter(
        "span", style = x ~ style(color = ifelse(rank(-x, ties.method = "max") <= 2, color3[11],
                                                 ifelse(rank(-x, ties.method = "max") > k -2, color3[1], grigi[4]))),
        x ~ sprintf("%.1f (rank: %02d)", x, rank(-x, ties.method = "max")),
        x ~ icontext(ifelse(x > 5, "arrow-up",
                            ifelse(x < -5, "arrow-down", "")))
      ),
      Var.bal.12m = formatter(
        "span", style = x ~ style(color = ifelse(rank(-x, ties.method = "max") <= 2, color3[11],
                                                 ifelse(rank(-x, ties.method = "max") > k -2, color3[1], grigi[4]))),
        x ~ sprintf("%.1f (rank: %02d)", x, rank(-x, ties.method = "max")),
        x ~ icontext(ifelse(x > 5, "arrow-up",
                            ifelse(x < -5, "arrow-down", "")))
      )
    )) %>%
    kable_styling("hover", full_width = F, position = "left") %>%
    row_spec(0, bold = F) %>%
    add_header_above(header = c("Indicatori classici" = 2, "Nuovi indicatori" = 4)) %>%
    # group_rows("2019", 12, 13) %>% # Come align : "links"
    column_spec(1, width = "4cm") %>%
    column_spec(2, bold = T, width = "2.5cm") %>%
    column_spec(3, width = "1.5cm") %>%
    column_spec(4, width = "0.5cm") %>%
    footnote(
    alphabet = c(
      "Media.bal: Media dell'INDICATORE SINTETICO nelle ultime quattro inchieste",
      "vs.t0: Valore dell'INDICATORE SINTETICO nell'ultima inchiesta rispetto alla media",
      "Var.bal.3m: Valore dell'INDICATORE SINTETICO rispetto all'inchieste precedente",
      "Var.bal.12m: Valore dell'INDICATORE SINTETICO rispetto all'inchiesta nello stesso periodo un anno prima"),
    number_title = "Indicatori classici",
    alphabet_title = "Nuovi indicatori"
    )
  d
  
}



out.tabella_SINT <- function(d1, domanda, dove, dom.ris){
  
  d2 <- dati.tndz(d1, d1[, 2])
  
  titolo <- paste0(domanda, dove, ", da ", d2[1, 1], " a ", d2[nrow(d2), 1])
  
  tmp <- crea.tabella_SINT(d2, titolo, dom.ris)
  
  tmp
  
}

out.grafici_SINT <- function(d1, domanda, dove){
  titolo1 <- "Indicatore SINTETICO"
  
  sottotitolo <- paste0(domanda, dove)
  
  p1 <- crea.gauge.balance(d1, titolo1, sottotitolo)
  
  p1 <- p1 + tema1  + guides(fill = guide_legend(title.position = "left", label.position = "top"))
  
  p2 <- ""
  
  p3 <- ""
  
  p <- list(p1, p2, p3)
  
}

#