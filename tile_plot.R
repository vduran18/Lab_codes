library(tidyverse)

dat <- read_excel("", sheet = "matrix")


breaks <- c(10e-21,10e-10,10e-5,10e-2,1e-1)
library("RColorBrewer")
ggplot(dat %>% mutate(significance = .5 - dat$`p-value_p`)) + geom_tile(aes(x = dat$`FC (log2)`,y = dat$FC_P,  fill = dat$`p-value_g`, height=0.50, width=0.3))+ 
  scale_fill_gradientn(limits=c(1e-20,1e-3),
                       colours=c("#010915", "#3c3f47", "#797a7f", "#bababd", "#ffffff")) +
  #geom_text(aes(x = dat$`FC (log2)`,y = dat$FC_P,label=Protein), check_overlap = TRUE, size=3.2, colour="blue", nudge_x = 0.75)+
  geom_point(aes(x=dat$`FC (log2)`,y = dat$FC_P, size= significance), colour = rgb(245, 127, 32, maxColorValue = 255))+ 
  theme_classic() +  theme(legend.position='none') + 
  xlab("Gene FC (log2)") + ylab("Protein FC  ")
dev.off()

