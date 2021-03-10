library(readxl)
library(corrplot)

#set working directory
setwd("/")

#read in data

val <- read_csv("")


############# NEW VOLC DEC 2020
val <- val[,c(1,36,41)]
val$FC.SCA.Control <- log2(val$FC.SCA.Control)
#set the color schemes 
color_key<-val$FC.SCA.Control*0
#color_key[val$FC.NPSLE.Control<=1] = "FC<2"
color_key[val$FC.SCA.Control > 0.5 & val$FC.SCA.Control < 3 &
            +                       val$u.pval.SCA.Control<0.05] = "FC < 3, FC > 0.5"
#color_key[val[which(val$FC.NPSLE.Control> -1 & val$t.pval.NPSLE.Control<0.05] 
color_key[val$FC.SCA.Control>3 & val$u.pval.SCA.Control<0.05] = "FC>3,p<0.05"
#color_key[val$FC.NPSLE.Control>1 & val$t.pval.NPSLE.Control<0.05] = "FC>2,p<0.05"
color_key[val$FC.SCA.Control< 0.5 & val$u.pval.SCA.Control<0.05] = "FC<0.5,p<0.05"
color_key[val$u.pval.SCA.Control>0.05] = "p>0.05"

View(color_key)





#create a new dataset for proteins wanted

newdata <- val[ which((val$FC.SCA.Control > 2 & val$u.pval.SCA.Control < 0.05) | 
                        (val$FC.SCA.Control < 0.5 & val$u.pval.SCA.Control < 0.05)),]
names(newdata)
newdata <- newdata[,c(1,36,41)]
newdata %>% arrange(desc(FC.SCA.Control), u.pval.SCA.Control) -> newdata
newdata <- newdata[c(1:5, 33:37),]
png('SCAvsCTRL_volcDec9.png', height = 20, width = 30, units='cm', res = 900)
ggplot(val,aes(x=log2(val$FC.SCA.Control),
               y=-log10(val$u.pval.SCA.Control)),
       xlim=c(0,00),ylim=c(0,5))+geom_point(aes(colour= color_key),size = 1.5)+
  scale_color_manual(values=c("FC<0.5,p<0.05"="green","FC < 3, FC > 0.5"="orange","FC>3,p<0.05"="red","p>0.05" = "blue"))+ 
  geom_hline(yintercept = -log10(0.05), linetype = 2, alpha = 0.5, color='black') + 
  geom_vline(xintercept = log2(3), linetype = 2, alpha = 0.5, color='black') +
  #geom_vline(xintercept = log2(2), linetype = 2, alpha = 0.5,color='black') +
  #geom_hline(yintercept = -1, linetype = 2, alpha = 0.5, color='red') +
  geom_vline(xintercept = log2(0.5), linetype = 2, alpha = 0.5, color='black') +
  geom_text_repel(data = newdata, aes(log2(newdata$FC.SCA.Control), -log10(newdata$u.pval.SCA.Control), label = newdata$X1), size = 3)+
  #geom_text_repel(data= val %>% mutate(label=ifelse(hia1$fc >= 5 & hia1$p.value >= 3 |, Target, "")), aes(label=label), size = 2.5)+ #only label proteins that follow threshold requirement
  xlab("log2 fold change") + ylab("-log10 p-value") + ggtitle("SCA vs CTRL") + theme_classic() + theme(plot.title = element_text(hjust = 0.5)) +labs(col="Color Key")# Relabel the axes 
dev.off()


val <- read_excel("", sheet = "")


str(dat)
str(val)

names(dat)

dat <- val[,-1]
matrix_cor <- cor(dat)
#Generate the heatmap
jpeg(file="VD_.jpeg",
     height = 12, width = 12, units='cm', res = 600)
corrplot(matrix_cor, type = "upper",tl.col="black", tl.cex = 0.4)
dev.off()



### PCA ANALYSIS

res.pca <- prcomp(val[,-1], scale = TRUE)
jpeg(file="VD_2DPCA_.jpeg",
     height = 20, width = 20, units='cm', res = 600)
fviz_pca_ind(res.pca,
             label = "none", # hide individual labels
             habillage = val$Target, # color by groups
             pointsize = 3,
             palette = c("#fc0303", "#03b1fc", "#03fc6f"),
             addEllipses = FALSE,
             legend.title = "Disease Groups"# Concentration ellipses
)+  labs(title = "PCA Analysis Top 20") +
  theme(plot.title = element_text(hjust = 0.5)) + xlab("PC 1 (66.3%)") + ylab("PC 2 (11.1%)")
dev.off()
