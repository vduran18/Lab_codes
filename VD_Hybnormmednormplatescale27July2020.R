library(data.table)
library(readr)
hybnormmednormplateNCle <- read_csv("/Users/valeriaduran/Downloads/NPSLE-hybnorm -N3 10august2020.csv")
###Use sweep to divide by rows
norm <- sweep(data.matrix(df[,-1]), 2, data.matrix(df[36,-1]), '/')
names <- df$Target
row.names(norm) <- names

#read data
proteins<-read.csv("/Users/valeriaduran/Desktop/NPSLE_albumin_norm_10AUG20.csv",header=FALSE,skip=1,row.names=1)
groups<-read.csv("/Users/valeriaduran/Desktop/NPSLE_albumin_norm_10AUG20.csv",header=FALSE,row.names=1,nrows=1)
#assign groups
Control<-proteins[,groups=="HC"]
NPSLE<-proteins[,groups=="NPSLE"]
NC<-proteins[,groups=="NC"]

#means
mean.Control<-apply(Control,1,function(x)mean(x,na.rm = TRUE))
mean.NPSLE<-apply(NPSLE,1,function(x)mean(x,na.rm = TRUE))
mean.NC<-apply(NC,1,function(x)mean(x,na.rm = TRUE))


#NPSLEvsControl
FC.NPSLE.Control<-mean.NPSLE/mean.Control
t.result.NPSLE.Control<-apply(cbind(NPSLE,Control),1,function(x)t.test(x[1:ncol(NPSLE)],
                                                         x[ncol(NPSLE)+1:ncol(cbind(NPSLE,Control))]))
t.pval.NPSLE.Control<-unlist(lapply(t.result.NPSLE.Control,function(x)x$p.value)) 
t.qval.NPSLE.Control<-p.adjust(t.pval.NPSLE.Control,"fdr",n=length(t.pval.NPSLE.Control))
t.eq.result.NPSLE.Control<- apply(cbind(NPSLE,Control),1,function(x)t.test(x[1:ncol(NPSLE)],
                                                             x[ncol(NPSLE)+1:ncol(cbind(NPSLE,Control))],var.equal=TRUE))                         
t.eq.pval.NPSLE.Control<-unlist(lapply(t.eq.result.NPSLE.Control,function(x)x$p.value)) 
t.eq.qval.NPSLE.Control<-p.adjust(t.eq.pval.NPSLE.Control,"fdr",n=length(t.eq.pval.NPSLE.Control))

u.result.NPSLE.Control<-apply(cbind(NPSLE,Control),1,function(x)wilcox.test(x[1:ncol(NPSLE)],
                                                              x[ncol(NPSLE)+1:ncol(cbind(NPSLE,Control))]))
u.pval.NPSLE.Control<-unlist(lapply(u.result.NPSLE.Control,function(x)x$p.value)) 
u.qval.NPSLE.Control<-p.adjust(u.pval.NPSLE.Control,"fdr",n=length(u.pval.NPSLE.Control))

#NCvsControl
FC.NC.Control<-mean.NC/mean.Control
t.result.NC.Control<-apply(cbind(NC,Control),1,function(x)t.test(x[1:ncol(NC)],
                                                       x[ncol(NC)+1:ncol(cbind(NC,Control))]))
t.pval.NC.Control<-unlist(lapply(t.result.NC.Control,function(x)x$p.value)) 
t.qval.NC.Control<-p.adjust(t.pval.NC.Control,"fdr",n=length(t.pval.NC.Control))
t.eq.result.NC.Control<- apply(cbind(NC,Control),1,function(x)t.test(x[1:ncol(NC)],
                                                           x[ncol(NC)+1:ncol(cbind(NC,Control))],var.equal=TRUE))                         
t.eq.pval.NC.Control<-unlist(lapply(t.eq.result.NC.Control,function(x)x$p.value)) 
t.eq.qval.NC.Control<-p.adjust(t.eq.pval.NC.Control,"fdr",n=length(t.eq.pval.NC.Control))

u.result.NC.Control<-apply(cbind(NC,Control),1,function(x)wilcox.test(x[1:ncol(NC)],
                                                            x[ncol(NC)+1:ncol(cbind(NC,Control))]))
u.pval.NC.Control<-unlist(lapply(u.result.NC.Control,function(x)x$p.value)) 
u.qval.NC.Control<-p.adjust(u.pval.NC.Control,"fdr",n=length(u.pval.NC.Control))



#NPSLEvsNC
FC.NPSLE.NC<-mean.NPSLE/mean.NC
t.result.NPSLE.NC<-apply(cbind(NPSLE,NC),1,function(x)t.test(x[1:ncol(NPSLE)],
                                                         x[ncol(NPSLE)+1:ncol(cbind(NPSLE,NC))]))

t.pval.NPSLE.NC<-unlist(lapply(t.result.NPSLE.NC,function(x)x$p.value)) 
t.qval.NPSLE.NC<-p.adjust(t.pval.NPSLE.NC,"fdr",n=length(t.pval.NPSLE.NC))
t.eq.result.NPSLE.NC<- apply(cbind(NPSLE,NC),1,function(x)t.test(x[1:ncol(NPSLE)],
                                                             x[ncol(NPSLE)+1:ncol(cbind(NPSLE,NC))],var.equal=TRUE))                         
t.eq.pval.NPSLE.NC<-unlist(lapply(t.eq.result.NPSLE.NC,function(x)x$p.value)) 
t.eq.qval.NPSLE.NC<-p.adjust(t.eq.pval.NPSLE.NC,"fdr",n=length(t.eq.pval.NPSLE.NC))

u.result.NPSLE.NC<-apply(cbind(NPSLE,NC),1,function(x)wilcox.test(x[1:ncol(NPSLE)],
                                                              x[ncol(NPSLE)+1:ncol(cbind(NPSLE,NC))]))
u.pval.NPSLE.NC<-unlist(lapply(u.result.NPSLE.NC,function(x)x$p.value)) 
u.qval.NPSLE.NC<-p.adjust(u.pval.NPSLE.NC,"fdr",n=length(u.pval.NPSLE.NC))


#append stats

stats_analysis<-data.frame(proteins)
colnames(stats_analysis)<-as.matrix(groups)
stats_analysis$mean.Control<-mean.Control
stats_analysis$mean.NPSLE<-mean.NPSLE
stats_analysis$mean.NC<-mean.NC


stats_analysis$FC.NPSLE.Control<-FC.NPSLE.Control
stats_analysis$t.pval.NPSLE.Control<-t.pval.NPSLE.Control
stats_analysis$t.qval.NPSLE.Control<-t.qval.NPSLE.Control
stats_analysis$t.eq.pval.NPSLE.Control<-t.eq.pval.NPSLE.Control
stats_analysis$t.eq.qval.NPSLE.Control<-t.eq.qval.NPSLE.Control
stats_analysis$u.pval.NPSLE.Control<-u.pval.NPSLE.Control
stats_analysis$u.qval.NPSLE.Control<-u.qval.NPSLE.Control

stats_analysis$FC.NC.Control<-FC.NC.Control
stats_analysis$t.pval.NC.Control<-t.pval.NC.Control
stats_analysis$t.qval.NC.Control<-t.qval.NC.Control
stats_analysis$t.eq.pval.NC.Control<-t.eq.pval.NC.Control
stats_analysis$t.eq.qval.NC.Control<-t.eq.qval.NC.Control
stats_analysis$u.pval.NC.Control<-u.pval.NC.Control
stats_analysis$u.qval.NC.Control<-u.qval.NC.Control


stats_analysis$FC.NPSLE.NC<-FC.NPSLE.NC
stats_analysis$t.pval.NPSLE.NC<-t.pval.NPSLE.NC
stats_analysis$t.qval.NPSLE.NC<-t.qval.NPSLE.NC
stats_analysis$t.eq.pval.NPSLE.NC<-t.eq.pval.NPSLE.NC
stats_analysis$t.eq.qval.NPSLE.NC<-t.eq.qval.NPSLE.NC
stats_analysis$u.pval.NPSLE.NC<-u.pval.NPSLE.NC
stats_analysis$u.qval.NPSLE.NC<-u.qval.NPSLE.NC



write.csv(stats_analysis,"NPSLE-hybnorm -N3_stats_10AUG2020.csv")





library("ggplot2") 
library("ggrepel") 
library(tidyverse)

val <- stats_analysis

#Volcano plot for NPSLE vs HC

#set the color schemes 
color_key<-val$FC.NPSLE.Control*0
color_key[val$FC.NPSLE.Control>2 & val$u.pval.NPSLE.Control<0.05] = "FC>2,p<0.05"
color_key[val$FC.NPSLE.Control>3 & val$u.pval.NPSLE.Control<0.05] = "FC>3,p<0.05"
color_key[val$FC.NPSLE.Control<=2] = "FC<2"
color_key[val$FC.NPSLE.Control<0.5 & val$u.pval.NPSLE.Control<0.05] = "FC<0.5,p<0.05"
color_key[val$u.pval.NPSLE.Control>0.05] = "p>0.05"
View(color_key)

#create a new dataset for proteins wanted

newdata <- val[ which((val$FC.NPSLE.Control > 3 & val$u.pval.NPSLE.Control < 0.05) | 
                        (val$FC.NPSLE.Control < 0.5 & val$u.pval.NPSLE.Control < 0.05)),]
names(newdata)
newdata <- newdata[,c(1,29,34)]
newdata %>% arrange(desc(FC.NPSLE.Control), u.pval.NPSLE.Control) -> newdata
newdata <- newdata[c(1:5, 17:21),]

png('NPSLEvsCTRL_volc.png', height = 20, width = 30, units='cm', res = 900)
ggplot(val,aes(x=log2(val$FC.NPSLE.Control),
               y=-log10(val$u.pval.NPSLE.Control)),
       xlim=c(0,00),ylim=c(0,5))+geom_point(aes(colour= color_key),size = 1.5)+
  NCle_color_manual(values=c("FC>2,p<0.05"="blue","FC>3,p<0.05"="red","FC<0.5,p<0.05"="red","FC<2"="black", "p>0.05" = "black"))+ 
  geom_hline(yintercept = 1.3, linetype = 2, alpha = 0.5, color='black') + 
  geom_vline(xintercept = log2(3), linetype = 2, alpha = 0.5, color='red') +
  #geom_vline(xintercept = log2(2), linetype = 2, alpha = 0.5,color='black') +
  #geom_hline(yintercept = -1, linetype = 2, alpha = 0.5, color='red') +
  geom_vline(xintercept = log2(0.5), linetype = 2, alpha = 0.5, color='red') +
  geom_text_repel(data = newdata, aes(log2(newdata$FC.NPSLE.Control), -log10(newdata$u.pval.NPSLE.Control), label = newdata$X1), size = 3)+
  #geom_text_repel(data= val %>% mutate(label=ifelse(hia1$fc >= 5 & hia1$p.value >= 3 |, Target, "")), aes(label=label), size = 2.5)+ #only label proteins that follow threshold requirement
  xlab("log2 fold change") + ylab("-log10 p-value") + ggtitle("NPSLE vs CTRL") + theme_classic() + theme(plot.title = element_text(hjust = 0.5))# Relabel the axes
dev.off()

##Heatmap for NPSLE vs CTRL
val %>% arrange(desc(FC.NPSLE.Control), u.pval.NPSLE.Control) %>% filter(u.pval.NPSLE.Control < 0.05) %>% top_n(50, wt= FC.NPSLE.Control)-> select 



names(select)
heat <- select[,c(1:9,18:25,10:17)]
heat_values<-data.matrix(heat[,-1])
row.names(heat_values)<-as.matrix(heat[,1])

heat2 <- heat_values
heat2 <- log2(heat2)

#change colors to yellow (for red) and blue (for green)
colors <- colorRampPalette(c("blue","grey27", "yellow"))(n=20)


distance = dist(heat2, method = "euclidean")
cluster = hclust(distance, method = "median")
jpeg(file="VD_NPSLEvsCTRLtop50_29July20_median.jpeg",
     height = 20, width = 30, units='cm', res = 600)
heatmap.2(heat2,
          NCle="row",col = colors, trace='none', Colv=NA,cexRow = 0.3, cexCol = 0.5, density.info = "none",
          Rowv = as.dendrogram(cluster), dendrogram = "row")
dev.off()

#Volcano plot for NC vs HC

#set the color schemes 
color_key<-val$FC.NC.Control*0
color_key[val$FC.NC.Control>2 & val$u.pval.NC.Control<0.05] = "FC>2,p<0.05"
color_key[val$FC.NC.Control>3 & val$u.pval.NC.Control<0.05] = "FC>3,p<0.05"
color_key[val$FC.NC.Control<=2] = "FC<2"
color_key[val$FC.NC.Control<0.5 & val$u.pval.NC.Control<0.05] = "FC<0.5,p<0.05"
color_key[val$u.pval.NC.Control>0.05] = "p>0.05"
View(color_key)
#create a new dataset for proteins wanted

newdata <- val[ which((val$FC.NC.Control > 2 & val$u.pval.NC.Control < 0.05) | 
                        (val$FC.NC.Control < 0.5 & val$u.pval.NC.Control < 0.05)),]
names(newdata)
newdata <- newdata[,c(1,36,41)]
newdata %>% arrange(desc(FC.NC.Control), u.pval.NC.Control) -> newdata
newdata <- newdata[c(1:5, 33:37),]
png('NCvsCTRL_volc.png', height = 20, width = 30, units='cm', res = 900)
ggplot(val,aes(x=log2(val$FC.NC.Control),
               y=-log10(val$u.pval.NC.Control)),
       xlim=c(0,00),ylim=c(0,5))+geom_point(aes(colour= color_key),size = 1.5)+
  NCle_color_manual(values=c("FC>2,p<0.05"="blue","FC>3,p<0.05"="red","FC<0.5,p<0.05"="red","FC<2"="black", "p>0.05" = "black"))+ 
  geom_hline(yintercept = 1.3, linetype = 2, alpha = 0.5, color='black') + 
  geom_vline(xintercept = log2(3), linetype = 2, alpha = 0.5, color='red') +
  #geom_vline(xintercept = log2(2), linetype = 2, alpha = 0.5,color='black') +
  #geom_hline(yintercept = -1, linetype = 2, alpha = 0.5, color='red') +
  geom_vline(xintercept = log2(0.5), linetype = 2, alpha = 0.5, color='red') +
  geom_text_repel(data = newdata, aes(log2(newdata$FC.NC.Control), -log10(newdata$u.pval.NC.Control), label = newdata$X1), size = 3)+
  #geom_text_repel(data= val %>% mutate(label=ifelse(hia1$fc >= 5 & hia1$p.value >= 3 |, Target, "")), aes(label=label), size = 2.5)+ #only label proteins that follow threshold requirement
  xlab("log2 fold change") + ylab("-log10 p-value") + ggtitle("NC vs CTRL") + theme_classic() + theme(plot.title = element_text(hjust = 0.5))# Relabel the axes
dev.off()

##Heatmap for NC vs CTRL
val %>% arrange(desc(FC.NC.Control), u.pval.NC.Control) %>% filter(u.pval.NC.Control < 0.05) %>% top_n(50, wt= FC.NC.Control)-> select 



names(select)
heat <- select[,c(1:9,18:25,10:17)]
heat_values<-data.matrix(heat[,-1])
row.names(heat_values)<-as.matrix(heat[,1])

heat2 <- heat_values
heat2 <- log2(heat2)

#change colors to yellow (for red) and blue (for green)
colors <- colorRampPalette(c("blue","grey27", "yellow"))(n=20)


distance = dist(heat2, method = "euclidean")
cluster = hclust(distance, method = "median")
jpeg(file="VD_NCvsCTRLtop50_29July20_median.jpeg",
     height = 20, width = 30, units='cm', res = 600)
heatmap.2(heat2,
          NCle="row",col = colors, trace='none', Colv=NA,cexRow = 0.3, cexCol = 0.5, density.info = "none",
          Rowv = as.dendrogram(cluster), dendrogram = "row")
dev.off()

