library(readxl)
library(ggplot2)


BP <- read_excel("/Users/valeriaduran/Downloads/analysis93_BP.xlsx")
MP <-  read_excel("/Users/valeriaduran/Downloads/analysis93_MF.xlsx")                  
CP <-  read_excel("/Users/valeriaduran/Downloads/analysis93_CC.xlsx")                  
GO_28 <- rbind(BP[-122,], MP, CP)

colnames(BP$`GO biological process complete`) <- GO
names(BP)[1] <- "GO"
BP$TYPE <- "Biological Process"
names(CP)[1] <- "GO"
CP$TYPE <- "Cellular Component"
names(MP)[1] <- "GO"
MP$TYPE <- "Molecular Function"

GO <- rbind(BP, MP, CP)
names(GO)[9] <- "Process"
#### ALL 258 Proteins

go258 <- read_excel("258_proteins_goanalysis.xlsx")
go258 <- data.frame(GO[,c(1,8,9)])
go258$upload_1..FDR. <- -log10(go258$upload_1..FDR.)

q <- ggplot(go258, aes(x = reorder(go258$GO, go258$upload_1..FDR.), y = go258$upload_1..FDR., fill=Process)) + 
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = c("#fc0303", "#03b1fc", "#03fc6f"))
q
q1 <- q + theme_minimal() +theme(legend.title = element_text(size = 20),
                                 legend.text = element_text(size = 18), legend.key.size = unit(1.2, 'cm'), axis.text.y = element_text(size = 15), axis.title.y = element_blank(),axis.text.x = element_text(size = 15),axis.title.x = element_text(size = 20)) + coord_flip()
q2 <- q1 + ylab("-log(p-value)")
q2
q3 <- q2 + facet_grid(rows = vars(Process),scales = "free", space = "free")
q4<- q3 + 
  theme(
    strip.background = element_blank(),
    strip.text  = element_blank()
  )
dev.off()
jpeg(file="GO_analysisDec24.jpeg",
     height = 30, width = 40, units='cm', res = 700)   
q4
dev.off()


######## RF

library(readxl)
library(readr)
library("RColorBrewer")
library(ggplot2)
library(gplots)
library(data.table)
library(naniar)
library(randomForest)
library(rsample)
library(tidyverse)



############### Random forest top 10 DMD vs HC ########################
dat <- read_excel("/Users/valeriaduran/Downloads/BC Top 93.xlsx", sheet = "Sheet3")



dat2 <- sapply(dat[,2:94], as.numeric )

dat2 <- as.data.frame(dat2)
names(dat2) <- make.names(names(dat2))


dat2 <- dat2[,1:20]
dat2$cells <- as.factor(dat$Target) 
#Create data for training
set.seed(150)
dat_split <- initial_split(dat2, prop = .75)
dat_train <- training(dat_split)
dat_test  <- testing(dat_split)

# for reproduciblity
set.seed(1234)

# default RF model
rf <- randomForest(
  formula = cells ~ .,
  data    = dat_train,
  mtry = 7,
  ntree = 500,
  importance=TRUE
)
rf

features <- setdiff(names(dat_train), "cells")
set.seed(100)

m3 <- tuneRF(x = dat_train[features],y= dat_train$cells, ntreeTry   = 500,
             mtryStart  = 4,
             stepFactor = .50,
             improve = 1.0,
             trace = FALSE)      # to not show real-time progress 
m3
set.seed(100)
tune.rf <- tuneRF(x = dat_train[features],y= dat_train$cells, stepFactor=0.5)
print(tune.rf)
rf.predict <- predict(rf, dat_test)
cm = table(dat_test$cells, rf.predict)
cm
varImp(rf)
imp1 <- varImpPlot(rf) # let's save the varImp object

# this part just creates the data.frame for the plot part
library(dplyr)
imp1 <- as.data.frame(imp1)
imp1$varnames <- rownames(imp1) # row names to column
rownames(imp) <- NULL  
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
jpeg(file="BC_Top20_RF_Dec24.jpeg",
     height = 30, width = 40, units='cm', res = 700)   
ggplot(imp1, aes(x = reorder(varnames, MeanDecreaseGini),
                 y = MeanDecreaseGini, weight=MeanDecreaseGini,
                 fill = MeanDecreaseGini)) +
  geom_bar(stat='identity', width = 0.7) +
  scale_fill_continuous(type = "viridis") + 
  coord_flip() +
  theme_classic() + theme(text= element_text(size = 15)) +
  labs(
    x     = "Biomarker",
    y     = "Importance",
    title = "BC Top 20") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.y = element_text(size = 15))
dev.off()

