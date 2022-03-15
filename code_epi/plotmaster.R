library(tidyverse)
library(readxl)
library(scales)
library(viridis)
library(lubridate)
library(ggpubr)
library(cowplot)

# times and dates 
today <- Sys.Date()

# FUNCTIONS ==============================================================
# moving average
mav <- function(x,n=3){stats::filter(x,rep(1/n,n), sides=2)}  #n=3 denotes the number of weeks to move the average

startweek=15
source("lab-rsv-plots.R")
source("icd-rsv-plots.R")

lab <- 
ggarrange(nsw.lab.plot+theme(axis.text.x = element_blank()),
          wa.lab.plot+theme(axis.text.x = element_blank()),
          vic.lab.plot+xlab("Week"),
          align = "v", ncol = 1,
          common.legend = T,
          labels = c("NSW","WA","Vic"),label.x = 0.1,
          font.label = list(size = 10, color = "black", face = "bold", family = NULL, hjust=0))
lab
icd <- 
  ggarrange(nsw.icd.plot+theme(axis.text.x = element_blank()),
            wa.icd.plot+theme(axis.text.x = element_blank()),
            vic.icd.plot+xlab("Month"),
            align = "v", ncol = 1,
            common.legend = T)
icd
ggarrange(lab, icd, ncol=2)
ggsave("Fig1.Lab-ICD-Plot.pdf", height=8, width=15)
ggsave("Fig1.Lab-ICD-Plot.png", height=8, width=15)

lab.long <- 
  ggarrange(nsw.lab.plot.long+theme(axis.text.x = element_blank()),
            wa.lab.plot.long+theme(axis.text.x = element_blank()),
            vic.lab.plot.long+xlab("Week"),
            labels = c("NSW","WA","Vic"),hjust=0,vjust=1,
            align = "v", ncol = 1,
            common.legend = T,
            font.label = list(size = 10, color = "black", face = "bold", family = NULL, hjust=0))
lab.long
icd.long <- 
  ggarrange(nsw.icd.plot.long+theme(axis.text.x = element_blank()),
            wa.icd.plot.long+theme(axis.text.x = element_blank()),
            vic.icd.plot.long+xlab("Month"),
            align = "v", ncol = 1,
            common.legend = T)
icd.long
ggarrange(lab.long, icd.long, ncol=2)
ggsave("Fig1.Lab-ICD-long-Plot.pdf", height=8, width=20)

