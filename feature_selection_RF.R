library(data.table)

aggrDat <- read.csv("~/Desktop/GWU/hackthon/merge_data/aggrDat.csv")
head(aggrDat)
dim(aggrDat)
aggrDat_new <- aggrDat[,c(-1,-38)]
head(aggrDat_new)

library(caret)
library(mlbench)

control <-trainControl( method="repeatedcv",number=10, repeats=3)
model <-train(gr~., data=aggrDat_new, method="rf", importance=TRUE, na.action=na.exclude, preProcess="scale", trControl=control)

importance <-varImp(model)
print(importance)
plot(importance)















