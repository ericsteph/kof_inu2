

# packages <- c("ggplot2", "pxR", "reshape2", "plyr", "ggsubplot", "gridExtra",
#               "RGraphics", "openxlsx", "stringr", "readxl")
# installed <- as.data.frame(installed.packages())
# if(all(packages %nin% installed$Package)){
#   install.packages(packages)
# }else{
# 
# }

# source("00_FUNZIONI_x_MC.R", encoding = "UTF-8")

library(ggplot2)
library(reshape2)
# install.packages("pxR")
library(pxR)
library(plyr)
library(dplyr)
# library(ggsubplot)  # Tolto il 9.2.2018
# install.packages("gridExtra")
library(gridExtra)
library(RGraphics)
library(openxlsx)  # al posto di XLConnect
library(readxl)
library(stringr)
library(tibble)
library(distrr)
# install.packages("knitr")
library(knitr)
library(kableExtra)
library(formattable)
library(shiny)


# if("kitmelar" %nin% installed.packages()[, 1]){
#   install.packages("../../kitmelar_0.0.4.tar.gz", repos = NULL, type = "source")
# }
# library(kitmelar)

# install.packages("wesanderson")
# library(wesanderson)
library(RColorBrewer)

# Rtools (necessari per openxlsx)
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "F:/USTAT/AA/Programmi/Rtools/bin/",
                        sep = .Platform$path.sep))

