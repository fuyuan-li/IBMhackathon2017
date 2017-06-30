rm(list = ls())
library(data.table)

grant_2014 <- fread("~/Desktop/GWU/hackthon/grants2010-2014/2014_ED_Grants_Full_20150715.csv")
summary(grant_2014)
View(grant_2014)

dim(grant_2014)
head(grant_2014[cfda_program_num == "84.184",cfda_program_title])
unique(grant_2014[cfda_program_num == "84.361",cfda_program_title])
class(grant_2014$cfda_program_num)

library(dplyr)
OECE_CFDA = c("84.287", "84.330","84.356","84.149", "84.283", "84.304", "84.299","84.349","84.196","84.365",
              "84.318","84.368","84.369","84.141","84.360","84.041","84.367","84.060","84.850","84.206",
              "84.366","84.144","84.011","84.362","84.013","84.358","84.186","84.184","84.377","84.415",
              "84.371","84.256","84.010","84.004")
OII_CFDA = c("84.351","84.282","84.354","84.370","84.215","84.411","84.165","84.416","84.295",
             "84.363","84.374","84.336","84.350")
OECE_OII_CFDA = c(OECE_CFDA, OII_CFDA)

OECE_OII_filter<-grant_2014%>%
  filter(cfda_program_num %in% OECE_OII_CFDA)
View(OECE_OII_filter)
dim(OECE_OII_filter)