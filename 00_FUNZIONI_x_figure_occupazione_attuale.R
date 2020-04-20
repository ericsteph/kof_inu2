# Occupazione
crea.tabella2 <- function(d, titolo, dom.ris){
  
  k <- nrow(d)
  
  colnames(d) <- c("Inchiesta", "Balance", "Ins.x", "Ecc.x", "Var.ins.3m",
                   "var1.x", "Var.ins.12m", "var2.x", "Media.ins", "vs.t0")
  
  d <-   d %>% dplyr::select(Inchiesta, Balance, Ins.x, Ecc.x, 
                             Media.ins, vs.t0, Var.ins.3m, Var.ins.12m)
  
  d <- formattable::format_table(
    d,
    digits = 1L,
    align = c("l", "c", "r", "l", "c","l", rep("c", 2)),
    format = "html",
    caption = titolo,
    row_label_position = "l",
    formatters = list(
      
      Balance = color_tile("white", color1[2]
      ),
      Ins.x = color_bar(color2[2], fun = unit.scale
      ),
      Ecc.x = color_bar(grigi[3], fun = unit.scale
      ),
      Media.ins = color_tile("white", grigi[5]
      ),
      vs.t0 = formatter(
        "span",
        style = x ~ style(color = ifelse(x,  "", grigi[5])),
        x ~ icontext(ifelse(x, "", "thumbs-down"), ifelse(x, "", ""))
      ),
      Var.ins.3m = formatter(
        "span", style = x ~ style(color = ifelse(rank(-x, ties.method = "max") <= 2, color1[7],
                                                 ifelse(rank(-x, ties.method = "max") > k -2, color2[7], grigi[4]))),
        x ~ sprintf("%.1f (rank: %02d)", x, rank(-x, ties.method = "max")),
        x ~ icontext(ifelse(x > 5, "arrow-up",
                            ifelse(x < -5, "arrow-down", "")))
      ),
      Var.ins.12m = formatter(
        "span", style = x ~ style(color = ifelse(rank(-x, ties.method = "max") <= 2, color1[7],
                                                 ifelse(rank(-x, ties.method = "max") > k -2, color2[7], grigi[4]))),
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
      "Balance: Differenza tra la quota di risposte ''Eccessivo'' meno quella di risposte ''Insufficienti''",
      paste0("Ecc.x: ", dom.ris[[1]], " ... ", dom.ris[[2]]),
      paste0("Ins.x: ", dom.ris[[1]], " ... ", dom.ris[[3]])
    ),
    alphabet = c(
      "Media.ins: Media delle risposte ''Insufficienti'' nelle ultime quattro inchieste",
      "vs.t0: Risposte ''Insufficienti'' nell'ultima inchiesta rispetto alla media",
      "Var.pos.3m: Tendenza delle risposte ''Insufficienti'' rispetto all'inchieste precedente",
      "Var.pos.12m: Tendenza delle risposte ''Insufficienti'' rispetto all'inchiesta nello stesso periodo un anno prima"),
    number_title = "Indicatori classici",
    alphabet_title = "Nuovi indicatori"
    )
  
  d
  
}


out.tabella2 <- function(d1, domanda, dove, dom.ris){
  
  d2 <- dati.tndz(d1, d1[, 3])
  
  titolo <- paste0(domanda, " ", dove, ", da ", d2[1, 1], " a ", d2[nrow(d2), 1])
  
  tmp <- crea.tabella2(d2, titolo, dom.ris)
  
  tmp
  
}


# crea.gauge.balance2 <- function(d1, titolo, sottotitolo){
#   
#   a1 <- quantile(d1[, 2], probs = 0.2)
#   a2 <- quantile(d1[, 2], probs = 0.4)
#   a3 <- quantile(d1[, 2], probs = 0.6)
#   a4 <- quantile(d1[, 2], probs = 0.8)
#   
#   for(i in(1:nrow(d1))){
#     ifelse(d1[i, 2] < 0, d1$tmp1[i] <- 100 + d1[i, 2],
#            d1$tmp1[i] <- 100)
#   }
# 
#   for(i in(1:nrow(d1))){
#     ifelse(d1[i, 2] >= 0, d1$tmp2[i] <- 100 - d1[i, 2],
#            d1$tmp2[i] <- 100)
#   }
# 
#   d1 <- d1 %>% dplyr::mutate(gruppo = ifelse(d1[, 2] < a1, "bene",
#                                              ifelse(d1[, 2] >= a1 & d1[, 2] < a2, "rel. bene",
#                                                     ifelse(d1[, 2] >= a2 & d1[, 2] < a3, "neutro",
#                                                            ifelse(d1[, 2] >= a3 & d1[, 2] < a4, "rel. male", "male")
#                                                     ))))
#   
#   d1[, ncol(d1)] <- factor(d1[, ncol(d1)], levels = c("male", "rel. male", "neutro", "rel. bene", "bene"))
#   
#   d1 <- d1[(nrow(d1)-3):nrow(d1), ]
#   
#   p <- ggplot(d1, aes(fill = gruppo, ymax = tmp1 + d1[, 2], ymin = tmp1, xmax = 8, xmin = 3)) +
#     geom_rect(aes(ymax = tmp1, ymin = -100, xmax = 8, xmin = 3), fill = grigi[2]) +
#     geom_rect(aes(ymax = 300, ymin = tmp1 + d1[, 2], xmax = 8, xmin = 3), fill = grigi[2]) +
#     geom_rect() +
#     coord_polar(theta = "y", start = -pi/2) +
#     xlim(c(-5, 8)) +
#     ylim(c(-100, 300)) +
#     geom_text(aes(x = -5, y = 300, label = d1[, 2], colour = gruppo), size = 5.5) +
#     geom_text(aes(x = -1, y = 300, label = d1[, 1]), size = 2.8) +
#     facet_wrap(~d1[, 1], nrow = 1) +
#     scale_fill_manual(values = c(color3[1], color3[2], color3[4], color3[10], color3[11]), limits = levels(d1$gruppo),
#                       labels = c("peggiore", "", "media", "", "migliore"),
#                       name = "Posizione relativa\nrispetto alle ultime inchieste") +
#     scale_colour_manual(values = c(color3[1], color3[2], color3[4], color3[10], color3[11]), limits = levels(d1$gruppo)) +
#     labs(title = titolo, subtitle = sottotitolo) + guides(color=FALSE)
#   
#   p
#   
# }

crea.gauge.balance2 <-   function(d1, titolo, sottotitolo){

a1 <- quantile(-d1[, 2], probs = 0.2, na.rm = TRUE)
a2 <- quantile(-d1[, 2], probs = 0.4, na.rm = TRUE)
a3 <- quantile(-d1[, 2], probs = 0.6, na.rm = TRUE)
a4 <- quantile(-d1[, 2], probs = 0.8, na.rm = TRUE)

for(i in(1:nrow(d1))){
ifelse(-d1[i, 2] < 0, d1$tmp1[i] <- -d1[i, 2],
  d1$tmp1[i] <- 0)
}

for(i in(1:nrow(d1))){
  ifelse(-d1[i, 2] < 0, d1$tmp2[i] <- 0,
         d1$tmp2[i] <- -d1[i, 2])
}


d1 <- d1 %>% dplyr::mutate(gruppo = ifelse(-d1[, 2] < a1, "male",
                                           ifelse(-d1[, 2] >= a1 & -d1[, 2] < a2, "rel. male",
                                             ifelse(-d1[, 2] >= a2 & -d1[, 2] < a3, "neutro",
                                               ifelse(-d1[, 2] >= a3 & -d1[, 2] < a4, "rel. bene", "bene")
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
    geom_text(aes(x = -5, y = 200, label = -d1[, 2], colour = gruppo), size = 5.5) +
    geom_text(aes(x = -1, y = 200, label = d1[, 1]), size = 2.8) +
    facet_wrap(~d1[, 1], nrow = 1) +
    scale_fill_manual(values = c(color3[1], color3[2], color3[4], color3[10], color3[11]), limits = levels(d1$gruppo),
                      labels = c("peggiore", "", "media", "", "migliore"),
                      name = "Posizione relativa\nrispetto alle ultime inchieste") +
    scale_colour_manual(values = c(color3[1], color3[2], color3[4], color3[10], color3[11]), limits = levels(d1$gruppo)) +
    labs(title = titolo, subtitle = sottotitolo) + guides(color=FALSE)

p

}


crea.gauge.pos2 <- function(d1, titolo, sottotitolo){
  
  a1 <- quantile(d1[, 4], probs = 0.2)
  a2 <- quantile(d1[, 4], probs = 0.4)
  a3 <- quantile(d1[, 4], probs = 0.6)
  a4 <- quantile(d1[, 4], probs = 0.8)
  
  d1 <- d1 %>% dplyr::mutate(tmp = 100 - d1[, 4])
  
  d1 <- d1 %>% dplyr::mutate(gruppo = ifelse(d1[, 2] < a1, "bene",
                                             ifelse(d1[, 2] >= a1 & d1[, 2] < a2, "rel. bene",
                                                    ifelse(d1[, 2] >= a2 & d1[, 2] < a3, "neutro",
                                                           ifelse(d1[, 2] >= a3 & d1[, 2] < a4, "rel. male", "male")
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
    geom_text(aes(x = -5, y = 150, label = paste0(-d1[, 4], "%"), colour = gruppo), size = 5.5) +
    geom_text(aes(x = -1, y = 150, label = d1[, 1]), size = 2.8) +
    facet_wrap(~d1[, 1], nrow = 1) +
    scale_fill_manual(values = c(color3[1], color3[2], color3[4], color3[10], color3[11]), limits = levels(d1$gruppo),
                      labels = c("peggiore", "", "media", "", "migliore"),
                      name = "Posizione relativa\nrispetto alle ultime inchieste") +
    scale_colour_manual(values = c(color3[1], color3[2], color3[4], color3[10], color3[11]), limits = levels(d1$gruppo)) +
    labs(title = titolo, subtitle = sottotitolo) + guides(color=FALSE)
  
  p
  
}



crea.gauge.neg2 <- function(d1, titolo, sottotitolo){
  
  a1 <- quantile(d1[, 3], probs = 0.2)
  a2 <- quantile(d1[, 3], probs = 0.4)
  a3 <- quantile(d1[, 3], probs = 0.6)
  a4 <- quantile(d1[, 3], probs = 0.8)
  
  d1 <- d1 %>% dplyr::mutate(tmp = 100 - d1[, 3])
  
  d1 <- d1 %>% dplyr::mutate(gruppo = ifelse(d1[, 3] < a1, "male",
                                             ifelse(d1[, 3] >= a1 & d1[, 3] < a2, "rel. male",
                                                    ifelse(d1[, 3] >= a2 & d1[, 3] < a3, "neutro",
                                                           ifelse(d1[, 3] >= a3 & d1[, 3] < a4, "rel. bene", "bene")
                                                    ))))
  
  d1[, ncol(d1)] <- factor(d1[, ncol(d1)], levels = c("male", "rel. male", "neutro", "rel. bene", "bene"))
  
  d1 <- d1[(nrow(d1)-3):nrow(d1), ]
  
  p <- ggplot(d1, aes(fill = gruppo, ymax = 100, ymin = tmp, xmax = 8, xmin = 3)) +
    geom_rect(aes(ymax = tmp, ymin = 0, xmax = 8, xmin = 3), fill = grigi[2]) +
    geom_rect() +
    coord_polar(theta = "y", start = -pi/2) +
    xlim(c(-5, 8)) +
    ylim(c(0, 200)) +
    geom_text(aes(x = -5, y = 150, label = paste0(d1[, 3], "%"), colour = gruppo), size = 5.5) +
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

out.grafici2 <- function(d, domanda, dove){
  titolo1 <- "Balance (Risposte ''Eccessivo'' meno Risposte ''Insufficiente'')"
  titolo2 <- "Risposte ''Eccessivo'', in %"
  titolo3 <- "Risposte ''Insufficiente'', in %"
  
  sottotitolo <- paste0(domanda, " ", dove)
  
  p1 <- crea.gauge.balance2(d, titolo1, sottotitolo)
  
  p1 <- p1 + tema1  + guides(fill = guide_legend(title.position = "left", label.position = "top"))
  
  p2 <- crea.gauge.pos2(d, titolo2, sottotitolo)
  
  p2 <- p2 + tema1  + guides(fill = guide_legend(title.position = "left", label.position = "top"))
  
  p3 <- crea.gauge.neg2(d, titolo3, sottotitolo)
  
  p3 <- p3 + tema1  + guides(fill = guide_legend(title.position = "left", label.position = "top"))
  
  p <- list(p1, p2, p3)
  
}