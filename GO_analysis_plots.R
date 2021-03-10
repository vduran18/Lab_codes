library(readxl)
library(ggplot2)


BP <- read_excel("BP.xlsx")
MP <-  read_excel("MP.xlsx")                  
CP <-  read_excel("CP.xlsx")                  
GO_28 <- rbind(BP[-122,], MP, CP)

jpeg(file="GO_28.jpeg",
     height = 50, width = 20, units='cm', res = 600)
ggplot(theTable, aes(x=GO, y=theTable$`p-value (FDR)`, fill= Process)) +
  geom_bar(stat='identity') + coord_flip()
dev.off()

ggplot(BP, aes(x=GO, y=BP$`p-value (FDR)`), fill = Process)) +
  geom_bar(stat='identity') #+
  coord_flip()

theTable <- within(BP, 
                   Process <- factor(Process, 
                                      levels=names(sort(table(Process), 
                                                        decreasing=TRUE))))

ggplot(theTable,aes(x=Proce))+geom_bar(binwidth=1)
BP$`p-value (FDR)` <- -log10(BP$`p-value (FDR)`)
data2  <- BP[order(BP[,2],decreasing=F),]

jpeg(file="GO_28.jpeg",
     height = 50, width = 20, units='cm', res = 600)
ggplot(BP, aes(x = GO, y = BP$`p-value (FDR)`, fill = Process)) + geom_bar(stat = "identity") + theme(axis.title.y = element_text(size = 5)) + coord_flip()
dev.off()

BP1 <- BP[1:]

jpeg(file="GO_28.jpeg",
     height = 50, width = 20, units='cm', res = 600)
p <- ggplot(BP, aes(x = GO, y = -log10(BP$`p-value (FDR)`), fill = Process)) +
  geom_bar(
    stat = "identity", position = position_fill())#,
    #stat = "identity", position = "dodge",
    #width = 0.7
  ) +
  scale_color_manual(values = c("#0073C2FF", "#EFC000FF", '#03fc6f'))+
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF", "#03fc6f")) + coord_flip()
p
dev.off()


str(BP1)
BP <- data.frame(BP)
BP1 <- BP
BP1$Process <- as.factor(BP1$Process)

ggplot(BP, aes(x= GO, y= BP$`p-value (FDR)`, fill = factor(Process))) + geom_bar()

levels(BP1$Process)
ggplot(BP1, aes(GO, -log10(BP1$p.value..FDR.), fill = Process)) + 
  geom_bar(position = "dodge", width = 0.5, stat = "identity") 
sizes <- factor(BP1$Process, levels = c("BP", "MP", "CP"))
barplot(BP, legend = TRUE, beside = TRUE)



theTable <- within(BP, 
                   GO <- factor(GO, 
                                      levels=names(sort(table(GO), 
                                                        decreasing=TRUE))))

ggplot(theTable,aes(x=GO, y = -log10(theTable$p.value..FDR.), fill=Process))+geom_bar(stat = "identity",position = "dodge")
   

jpeg(file="GO_28.jpeg",
     height = 50, width = 20, units='cm', res = 600)    
ggcharts::bar_chart(theTable, x= GO, y = -log10(theTable$p.value..FDR.), group = Process)      
dev.off()


barplot(GO,                                       # Add labels to barplot
        names.arg = group)


BP2 <- BP
BP2$Process <- factor(BP$Process,levels = c("BP", "MP", "CP"))
ggplot(BP2, aes(x = GO, y = BP2$p.value..FDR., fill=Process)) + geom_bar(stat = "identity")



barplot(BP2$p.value..FDR.,
        names.arg = BP2$GO,
        beside = TRUE,
       main = "Survival of Each Class",
       xlab = "GO",
       col = c("red","green", "yellow")
)

GO <- c("T cell chemotaxis (GO:0010818)", "T cell chemotaxis (GO:0010818)",
        "eosinophil chemotaxis (GO:0048245)", "CXCR3 chemokine receptor binding (GO:0048248)",
        'hemokine activity (GO:0008009)', 'CXCR chemokine receptor binding (GO:0045236)',"cytoplasmic vesicle lumen (GO:0060205)",
        "vesicle lumen (GO:0031983)", "collagen-containing extracellular matrix (GO:0062023)
")

pvalue <- c(2.49e-02, 3.07e-08, 1.90e-06, 2.22e-08, 1.72e-08, 1.63e-08, 6.18e-03, 3.87e-08
, 3.19e-07)
process <- as.factor(rep(c("BP", "CP", "MP"),c(3,3,3)))
data <- data.frame(GO, pvalue,process)
data$pvalue <- -log10(pvalue)
data$GO <- as.factor(data$GO)
ggplot(data, aes(GO, pvalue, fill=factor(process))) +                                    # Manually ordered barchart
  geom_bar(stat = "identity")

BP4 <- BP[,-1]

names(B4)<- BP[,1]
barplot(height=as.matrix(BP$p.value..FDR.),main="Analysis-1",ylab="Vaccine", beside=TRUE,col=rainbow (5))
legend ("topleft",c("Week1","Week2","Week3","Week4","Week5"),cex=2.0,bty="n",fill=rainbow (5))

BP_sep <- BP[1:121,1:2]
BP_sep$p.value..FDR. <- -log10(BP_sep$p.value..FDR.)
data2  <- BP_sep[order(BP_sep[,2],decreasing=F),]

b= barplot(data2$`p-value (FDR)`,col="green", names.arg = data2$GO, horiz = T,
        cex.names=0.23, las=1)

BP1 <- BP
BP1$p.value..FDR. <- -log10(BP1$p.value..FDR.)
BP_sep <- BP[1:121,1:2]
BP_sep$`p-value (FDR)` <- -log10(BP_sep$`p-value (FDR)`)
data2  <- BP_sep[order(BP_sep[,2],decreasing=F),]

b= barplot(data2$`p-value (FDR)`,col="yellow", names.arg = data2$GO, horiz = T,
           cex.names=0.25, las=1)

MP_sep <- BP[122:138,1:2]
MP_sep$`p-value (FDR)` <- -log10(MP_sep$`p-value (FDR)`)
data3  <- data.frame(MP_sep[order(MP_sep[,2],decreasing=F),])

m= barplot(data3$`p-value (FDR)`,col="red", names.arg = data3$GO, horiz = T,
           cex.names=0.3, las=1)

CP_sep <- BP[139:148,1:2]
CP_sep$`p-value (FDR)` <- -log10(CP_sep$`p-value (FDR)`)
data4  <- data.frame(CP_sep[order(CP_sep[,2],decreasing=F),])

c= barplot(data4$`p-value (FDR)`,col="blue", names.arg = data4$GO, horiz = T,
           cex.names=0.3, las=1)


  
test <- cbind(BP_sep,CP_sep,MP_sep)

barplot(test,beside=T)
data2 <- data.frame(data2)
ggplot(MP, aes())


################################### DO THIS ###########################
jpeg(file="GO_28.jpeg",
     height = 50, width = 20, units='cm', res = 600)   
BP1 <- data.frame(BP)
BP1$p.value..FDR. <- -log10(BP1$p.value..FDR.)
b=ggplot(data = data2, aes(x = reorder(data2$GO, data2$p.value..FDR.), y=data2$p.value..FDR.)) + geom_bar(stat = "identity", fill="red")+ theme_classic() +theme(axis.text.y = element_text(size = 3), axis.title = element_blank()) + coord_flip()
m=ggplot(data = data3, aes(x = reorder(data3$GO, data3$p.value..FDR.), y=data3$p.value..FDR.)) + geom_bar(stat = "identity", fill="blue")+ theme_classic() +theme(axis.text.y = element_text(size = 10), axis.title = element_blank()) + coord_flip()
c=ggplot(data = data4, aes(x = reorder(data4$GO, data4$p.value..FDR.), y=data4$p.value..FDR.)) + geom_bar(stat = "identity", fill="darkgreen")+ theme_classic() +theme(axis.text.y = element_text(size = 10), axis.title = element_blank()) + coord_flip()
#####################################
p <- ggplot(BP1, aes(x = reorder(BP1$GO, BP1$p.value..FDR.), y = BP1$p.value..FDR., fill=Process)) + 
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = c("#fc0303", "#03b1fc", "#03fc6f"))
p1 <- p + theme_classic() +theme(legend.key.size = unit(1.5, 'cm'), axis.text.y = element_text(size = 7), axis.title.y = element_blank(),axis.text.x = element_text(size = 15),axis.title.x = element_text(size = 15)) + coord_flip()
p2 <- p1 + ylab("-log(p-value)")
p2
p3 <- p2 + facet_grid(rows = vars(Process),scales = "free", space = "free")
p4<- p3 + 
  theme(
    strip.background = element_blank(),
    strip.text  = element_blank()
  )

jpeg(file="GO_28.jpeg",
     height = 30, width = 50, units='cm', res = 700)   
p4
dev.off()

#######################################################################3

#### ALL 258 Proteins

go258 <- read_excel("258_proteins_goanalysis.xlsx")
go258 <- data.frame(go258[,c(1,8,9)])
go258$upload_1..FDR. <- -log10(go258$upload_1..FDR.)

q <- ggplot(go258, aes(x = reorder(go258$GO.biological.process.complete, go258$upload_1..FDR.), y = go258$upload_1..FDR., fill=Process)) + 
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
jpeg(file="GO_258.jpeg",
     height = 30, width = 40, units='cm', res = 700)   
q4
dev.off()

#### ALL 28 Proteins

go28 <- read_excel("28_go.xlsx")
go28 <- data.frame(go28[,c(1,8,9)])
go28$upload_1..FDR. <- -log10(go28$upload_1..FDR.)

q <- ggplot(go28, aes(x = reorder(go28$GO.biological.process.complete, go28$upload_1..FDR.), y = go28$upload_1..FDR., fill=Process)) + 
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
jpeg(file="GO_28.jpeg",
     height = 30, width = 40, units='cm', res = 700)   
q4
dev.off()

