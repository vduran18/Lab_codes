library("qvalue")
library(readxl)
library(stats)

fig1 <- read_excel("", sheet = 2) 
fig <- tibble(fig1)
pval <- fig$u.pval.UC.HC
qobj <- qvalue(p = pval)
names(fig)
# initialize a list to store the p_values for UC vs HC 
p_valuesUC_HC <- vector("list", nrow(fig))

for(i in seq_along(1: nrow(fig))){
  p_valuesUC_HC[i] = wilcox.test(as.matrix(fig[i,21:25]),as.matrix(fig[i,2:10]),alternative = "two.sided", exact = FALSE)$p.value
  
}


fig$p_valuesUC_HC <- as.numeric(p_valuesUC_HC)

fig$FC_UC_HC <- abs(rowMeans(fig[,21:25])/rowMeans(fig[,2:10]))
fig$FC_CD_HC <- abs(rowMeans(fig[,11:20])/rowMeans(fig[,2:10]))

p_valuesCD_HC <- vector("list", nrow(fig))

for(i in seq_along(1: nrow(fig))){
  p_valuesCD_HC[i] = wilcox.test(as.matrix(fig[i,11:20]),as.matrix(fig[i,2:10]),alternative = "two.sided", exact = FALSE)$p.value
  
}
fig$p_valuesCD_HC <- as.numeric(p_valuesCD_HC)

### UC vs HC 

pval_UC.HC <- as.numeric(fig$p_valuesUC_HC)
# pi0 = 1 is same as BH in p.adjust
#obj_pval_UC.HC <- qvalue(pval_UC.HC)
obj_pval_UC.HC <- qvalue(pval_UC.HC, pi0 = 1)


qvalues_UCvsHC <- obj_pval_UC.HC$qvalues
qvalues_UCvsHC
fig$q.val.UC.HC <- qvalues_UCvsHC
#summary(qobj)
#soma$FDR <- qobj$lfdr
#write.csv(soma, "VD_Somascan-HL-0203.csv")


###use p.adjust function
q_val <- p.adjust(pval_UC.HC, method = "BH", n = length(pval_UC.HC))
View(q_val)
write.csv(soma, "VD_Somascan-HL-0203.csv")



## CD vs HC
p_val_CD.HC <- as.numeric(fig$p_valuesCD_HC)
p.adjust(p_val_CD.HC, method = "BH")

obj_qval_CD.HC <- qvalue(p_val_CD.HC, pi0=1)
qval_CDvsHC <- obj_qval_CD.HC$qvalues
fig$qval.CD.HC <- qval_CDvsHC


fig$FC.UC.HC <- NULL
fig$FC.CD.HC <- NULL
fig$mean.IBD <- NULL
fig$FC.IBD.HC <- NULL
fig$u.pval.IBD.HC <- NULL
fig$u.pval.CD.HC <- NULL
fig$u.pval.UC.HC <- NULL
fig$FC.CD.UC <- NULL
fig$u.pval.CD.UC <- NULL
fig$mean.CD <- NULL
fig$mean.UC <- NULL
fig$mean.HC <- NULL

fig$mean_UC <- rowMeans(fig[,21:25])
abs(fig$mean_CD/fig$mean_HC)

names(fig)
fig <- fig[,c(1:25,33,34,32,27,28,26,29,30,31)]
fig <- data.frame(fig)
write.csv(fig, "fig1_table.csv")


library("qvalue")
library(readxl)
library(stats)

fig <- read_excel("", sheet = 2) 

pval_UC.HC <- fig$u.pval.UC.HC

obj_UC <- qvalue(pval_UC.HC, pi0 = 1)
q.val.UC.HC <- obj_UC$qvalues
fig$q.val.UC.HC <- q.val.UC.HC


p_val_CD.HC <- fig$u.pval.CD.HC
pbj_CD <- qvalue(p_val_CD.HC, pi0=1)
q.val.CD.HC <- pbj_CD$qvalues
fig$q.val.CD.HC <- q.val.CD.HC
write.csv(fig, "fig1_orig_table.csv")
q_val <- p.adjust(pval_UC.HC, method = "BH", n = length(pval_UC.HC))
View(q_val)
