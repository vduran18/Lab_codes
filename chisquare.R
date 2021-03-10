dat <- data.frame(HC=c(0,19),BC=c(19,41), row.names = c('Positive', 'Negative'))
ct<- table(dat$HC, dat$BC)
chisq.test(dat)


dat_all <- data.frame(HC=c(0,19), Ta=c(0,35), Tis=c(2,5), T1=c(2,5), T2=c(2,9),row.names = c('Positive', 'Negative'))
chisq.test(table(dat_all$HC,dat_all$Ta, dat_all$Tis))



il8 <- data.frame(HC=c(1,18),BC=c(31,29), row.names = c('Positive', 'Negative'))
il<- table(il8$HC, il8$BC) 
chisq.test(il8, correct=T)#p-value = 0.0008915
View(il)
ctbl = cbind(il[,3], il[,"Var1"] + il[,"Var2"]) 
il[,3]
il
chisq
round(chisq$expected,2)


sdf <- data.frame(HC=c(0,19),BC=c(6,54), row.names = c('Positive', 'Negative'))
chisq.test(sdf, correct=F) #p-value = 0.3487

il1 <- data.frame(HC=c(0,19),BC=c(19,41), row.names = c('Positive', 'Negative'))
chisq.test(il1, correct = F) # p-value = 0.01219

il_alpha <- data.frame(HC=c(1,18),BC=c(25,35), row.names = c('Positive', 'Negative'))
chisq.test(il_alpha, correct = F) # p-value = 0.007748



