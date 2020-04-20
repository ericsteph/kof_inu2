
### 00.01 Scegliere "inchiesta" e "dove"
inchiesta <- "inu"


### 00.02 Impostazioni d'hoc per FIGURE
ultimo_anno <- 2020  # (VA MODIFICATO UNA VOLTA ALL'ANNO)
primo_anno <- ultimo_anno - 4


# 01 Ultima cartella
percorso <- "2020_04_20"
ultimo_mese <- "aprile"

percorso <- paste0("E:/KOF/", inchiesta, "/", percorso, "/", inchiesta, ".rda")
# percorso <- paste0("F:/Fust011/KOF/2018_nuova_piattaforma/file_scaricati2/", inchiesta, "/", percorso, "/", inchiesta, ".rda")
load(file = percorso)

d <- as.data.frame(inu)
rm(inu)


# 02 Variabili sul "dove"
dove <- c(
  "in Ticino",
  "in Svizzera"
)

vars.dove <- c(
  "rgn.ti.",
  ""
)


vars.settore <- c(
  "",
  "export.12.",
  "export.4.",
  "size_kof.l."
)
  

vars.q1 <- c(
  # 1, Situazione degli affari
  "q_ql_ass_bs",
  # 2-3, Volume degli ordini
  "q_ql_ass_order_blog",
  "q_ql_ass_order_blog_fgn",
  # 4, Occupazione
  "q_ql_ass_empl",
  
  # 5, Acqusizione di ordini, rispetto 
  # allo stesso mese di un anno prima
  "q_ql_chg_order_in_pmpym",
  # 6, Produzione, rispetto
  # allo stesso mese di un anno prima
  "q_ql_chg_prod_pmpym",
  
  # 7-8, Stock
  "q_ql_ass_stock_fin",
  "q_ql_ass_stock_intermed",
  # 9, Capacità tecniche
  "q_ql_ass_tech_cap",
  
  # 10, INDICATORE SINTETICO
  "course_bus"
)


vars.q2 <- c(
  # 1, Situazione reddituale
  "q_ql_chg_profit_p3m",
  # 2, Prezzi di vendita ultimi tre mesi
  "q_ql_chg_price_sales_p3m",
  # 3-5, Posizione concorrenziale
  "q_ql_chg_compet_ch_p3m",
  "q_ql_chg_compet_eu_p3m",
  "q_ql_chg_compet_non_eu_p3m",
  # 6, Capacità tecniche
  "q_ql_chg_tech_cap_p3m"
)


vars.q3 <- c(
  # 1, Situazione degli affari, prossimi 6 mesi
  "q_ql_exp_chg_bs_n6m",
  # 2-3, Volume degli ordini prossimi tre mesi
  "q_ql_exp_chg_order_in_n3m",
  "q_ql_exp_chg_export_n3m",
  # 4, Occupazione, prossimi tre mesi
  "q_ql_exp_chg_empl_n3m",
  # 5, Produzione prossimi tre mesi
  "q_ql_exp_chg_prod_n3m",
  # 6, Acquisto di prodotti non finiti, prossimi tre mesi
  "q_ql_exp_chg_pur_intermed_n3m",
  #7-8, prezzi, cambiamento atteso prossimi 3 mesi
  "q_ql_exp_chg_price_sales_n3m",
  "q_ql_exp_chg_price_pur_n3m"
)


vars.q4 <- c(
  # 1, Acqusizione di ordini
  "q_ql_chg_order_in_pmppm",
  # 2, Volume degli ordini
  "q_ql_chg_order_blog_pmppm",
  # 3, Produzione
  "q_ql_chg_prod_pmppm",
  # 4-5, Stock
  "q_ql_chg_stock_fin_pmppm",
  "q_ql_chg_stock_intermed_pmppm"
)


# 03 Variabili inerenti il questionario, per domanda
capitoli <- c(
  "Situazione degli affari",
  "Acquisizione di ordini",
  "Volume complessivo degli ordini",
  "Volume degli ordini dall'estero",
  "Produzione",
  "Stock di prodotti finiti",
  "Stock di prodotti intermedi",
  "Occupazione",
  "Capacità tecniche",
  "Prezzi di vendita",
  "Prezzi di acquisto",
  "Acquisto di prodotti non finiti",
  "Situazione reddituale",
  "Posizione concorrenziale, in CH",
  "Posizione concorrenziale, nell'UE",
  "Posizione concorrenziale, fuori dalla UE",
  "Indicatore sintetico"
)


capitoli_ <- c(
  "Valutazione generale",
  "Valutazione",
  "Previsione",
  "Valutazione del numero di occupati",
  "Variazione del numero d'occupati",
  "Valutazione su base annua",
  "Sintesi"
)


riferimenti <- c(
  "Situazione attuale",
  "Ultimi tre mesi",
  "Prossimi tre mesi",
  "Prossimi sei mesi",
  "Valutazione su base annua",
  "Val. rispetto al mese precedente"
)


settori <- c(
  "Industria",
  "Industria interna (0-33%)",
  "Industria esterna (> 66,6%)",
  "Industria, aziende grandi"
)

### POSSIBILI RISPOSTE
risposte00 <- data.frame(
  ris.pos = c("Buono", "Maggiore", "Buono*", "Buone*", "Maggiori", "Troppo grandi"),
  ris.0 = c("Soddisfacente", "Uguale", "Adeguato*", "Adeguate*", "Uguali", "Adeguate"),
  ris.neg = c("Cattivo", "Minore", "Insufficiente*", "Insufficienti*", "Minori", "Troppo piccole")
)

risposte01 <- data.frame(
  ris.pos = c("Positiva", "Migliorata", "Migliorati", "Migliorate", "Migliorerà", "Migliorerà", "Miglioreranno"),
  ris.0 = c("Neutra", "Rimasta invariata", "Rimasti invariate", "Rimaste invariate", "Rimarrà invariata", "Rimarrà invariato", "Rimarranno invariati"),
  ris.neg = c("Negativa", "Peggiorata", "Peggiorati", "Peggiorate", "Peggiorerà", "Peggiorerà", "Peggioreranno")
)

risposte02 <- data.frame(
  ris.pos = c("Aumentata", "Aumenterà", "Aumenterà", "Aumenteranno"),
  ris.0 = c("Rimasta invariata", "Rimarrà invariata", "Rimarrà invariato", "Resteranno uguali"),
  ris.neg = c("Diminuita", "Diminuirà", "Diminuirà", "Diminuiranno")
)

risposte03 <- data.frame(
  ris.pos = c("Troppo elevate", "Eccessivo", "Eccessive", "Aumentato", "Aumentati", "Aumentate", "Aumenterà", "Aumenteranno", "Aumenteranno"),
  ris.0 = c("Sufficienti", "Soddisfacente", "Normali", "Rimasto invariato", "Rimasti invariati", "Rimaste invariate", "Rimarrà invariato", "Rimarrano invariati", "Rimarrano invariate"),
  ris.neg = c("Troppo basse", "Insufficiente", "Insufficienti", "Diminuito", "Diminuiti", "Diminuite", "Diminuirà", "Diminuiranno", "Diminuiranno")
)

z <- list("", "", "", "", "", "", "")

#