#Ting RF 
library(readxl)
library("RColorBrewer")
library(ggplot2)
library(gplots)
library(data.table)
library(naniar)
library(randomForest)
library(rsample)
library(tidyverse)
library(dplyr)

dat <- read_excel("")
dat_CI <- read_excel("")
names(dat_CI) <- make.names(names(dat_CI))

dat_AI <- read_excel("")
names(dat_AI) <- make.names(names(dat_AI))

#Get values with FC >2 and p-value < 0.05
CI <- dat_CI %>% filter(FC._CI..4.CI.0.3 >=2 & p.U.test.CI..4.CI.0.3 <= 0.05) 

AI <- dat_AI %>% filter(FC...AI..7.AI.0.6 >=2 & p.u.test..AI..7.AI.0.6 <= 0.05) 

#Get values with corr > 0.5 and p-value < 0.05
CI_corr <- CI %>% filter(CI.corr..r > 0.5 & CI.corr..p < 0.05) %>% top_n(50)

AI_corr <- AI %>% filter(AI.corr..r > 0.5 & AI.corr..p < 0.05) %>% top_n(50)

write.csv(CI_corr, "CItop50.csv")
write.csv(AI_corr, "AItop50.csv")


###Random Forest TOP 50 CI

dat <- read_excel("")


names(dat) <- make.names(names(dat))
dat$Target <- as.factor(dat$Target) 
str(dat)

##convert columns to numeric
ix <- 1:11 
data2[ix] <- lapply(data2[ix], as.numeric) 
str(data2)

colSums(is.na(dat))
#Create data for training
set.seed(100)
dat_split <- initial_split(dat, prop = .8)
dat_train <- training(dat_split)
dat_test  <- testing(dat_split)

# for reproduciblity
set.seed(100)

# default RF model
rf <- randomForest(
  formula = Target ~ .,
  data    = dat_train,
  mtry = 7,
  ntree = 500,
  importance=TRUE
)
rf
set.seed(100)
features <- setdiff(names(dat_train), "Target")
m3 <- tuneRF(x = dat_train[features],y= as.factor(dat_train$Target), ntreeTry   = 500,
             mtryStart  = 7,
             stepFactor = 1.5,
             improve = 0.05,
             trace = FALSE)      # to not show real-time progress 
m3
set.seed(100)
tune.rf <- tuneRF(x = dat_train[features],y= dat_train$Target, stepFactor=1.5, doBest=TRUE)
print(tune.rf)
summary(dat_train)

#tuned RF
set.seed(100)

# default RF model
rf <- randomForest(
  formula = Target ~ .,
  data    = dat_train,
  mtry = 10,
  importance=TRUE
)
rf


imp <- varImpPlot(tune.rf) # let's save the varImp object

# this part just creates the data.frame for the plot part
library(dplyr)
imp <- as.data.frame(imp)
imp$varnames <- rownames(imp) # row names to column
rownames(imp) <- NULL  

tiff("Kiloplex_VariableImportance_11.tiff",
     height = 20, width = 30, units='cm', 
     compression = "lzw", res = 300)
ggplot(imp, aes(x = reorder(varnames, MeanDecreaseGini),
                y = MeanDecreaseGini, weight=MeanDecreaseGini,
                fill = MeanDecreaseGini)) +
  geom_bar(stat='identity', width = 0.9) +
  #scale_fill_continuous(type = "viridis") + 
  coord_flip() +
  theme_classic() + #theme(text= element_text(size = 15)) +
  labs(
    x     = "Biomarker",
    y     = "Importance",
    title = "CI Top 50") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_fill_viridis_b()
dev.off()



#Unordered
p = ggplot(imp, aes(x=varnames, y=MeanDecreaseGini, fill=MeanDecreaseGini)) +
  geom_bar(width=1, stat="identity") +theme_light() + labs(x = "Biomarker",y = "Importance",title = "CI Top 50")+
  scale_fill_viridis_b() +
  theme(axis.title.y=element_text(angle=0))
p +  coord_polar()+ theme(axis.text.x = element_text(angle=-20)) +
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.y = element_text(angle=0, vjust = 0.5)) +
  theme(text= element_text(size = 17))

#Ordered
p = ggplot(imp, aes(x = reorder(varnames, MeanDecreaseGini), y=MeanDecreaseGini, fill=MeanDecreaseGini)) +
  geom_bar(width=1, stat="identity") +theme_light() + labs(x = "Biomarker",y = "Importance",title = "CI Top 50")+
  scale_fill_viridis_b() +
  theme(axis.title.y=element_text(angle=0))
p +  coord_polar() +theme(axis.text.x = element_text(angle=-20)) +
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.y = element_text(angle=0, vjust = 0.5))+
  theme(text= element_text(size = 15))



#Dont use this
se <- function(x) sqrt(var(x)/length(x)) 
plot <- ggplot(imp, aes(varnames, MeanDecreaseGini, fill = varnames)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_errorbar(aes(ymin = MeanDecreaseGini - se(imp$MeanDecreaseGini), 
                    ymax = MeanDecreaseGini + se(imp$MeanDecreaseGini), 
                    color = varnames), 
                width = .2) + 
  scale_y_continuous(breaks = 0:nlevels(imp$varnames)) +
  theme_gray() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank())
plot 


plot + coord_polar() 


#### RF for Top AI

dat1 <- read_excel("")


names(dat1) <- make.names(names(dat1))
dat1$Target <- as.factor(dat1$Target) 
str(dat1)


colSums(is.na(dat1))
#Create data for training
set.seed(1)
dat_split <- initial_split(dat1, prop = .8)
dat_train <- training(dat_split)
dat_test  <- testing(dat_split)

# for reproduciblity
set.seed(100)

# default RF model
rf <- randomForest(
  formula = Target ~ .,
  data    = dat_train,
  mtry = 10,
  ntree = 500,
  importance=TRUE
)
rf
set.seed(100)
features <- setdiff(names(dat_train), "Target")
m3 <- tuneRF(x = dat_train[features],y= as.factor(dat_train$Target), ntreeTry   = 500,
             mtryStart  = 7,
             stepFactor = 1.5,
             improve = 1,
             trace = FALSE, doBest=TRUE)      # to not show real-time progress 
m3
set.seed(200)
tune.rf <- tuneRF(x = dat_train[features],y= dat_train$Target, stepFactor=1.5, doBest=TRUE)
print(tune.rf)
summary(dat_train)

#tuned RF
set.seed(100)

# default RF model
rf <- randomForest(
  formula = Target ~ .,
  data    = dat_train,
  mtry = 10,
  importance=TRUE
)
rf


imp <- varImpPlot(tune.rf) # let's save the varImp object

# this part just creates the data.frame for the plot part
library(dplyr)
imp <- as.data.frame(imp)
imp$varnames <- rownames(imp) # row names to column
rownames(imp) <- NULL  

tiff("Kiloplex_VariableImportance_11.tiff",
     height = 20, width = 30, units='cm', 
     compression = "lzw", res = 300)
ggplot(imp, aes(x = reorder(varnames, MeanDecreaseGini),
                y = MeanDecreaseGini, weight=MeanDecreaseGini,
                fill = MeanDecreaseGini)) +
  geom_bar(stat='identity', width = 0.9) +
  #scale_fill_continuous(type = "viridis") + 
  coord_flip() +
  theme_classic() + #theme(text= element_text(size = 15)) +
  labs(
    x     = "Biomarker",
    y     = "Importance",
    title = "AI Top 50") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_fill_viridis_b()
dev.off()



#Unordered
p = ggplot(imp, aes(x=varnames, y=MeanDecreaseGini, fill=MeanDecreaseGini)) +
  geom_bar(width=1, stat="identity") +theme_light() + labs(x = "Biomarker",y = "Importance",title = "AI Top 50")+
  scale_fill_viridis_b() +
  theme(axis.title.y=element_text(angle=0))
p +  coord_polar()+ theme(axis.text.x = element_text(angle=-20)) +
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.y = element_text(angle=0, vjust = 0.5)) +
  theme(text= element_text(size = 17))

#Ordered
p = ggplot(imp, aes(x = reorder(varnames, MeanDecreaseGini), y=MeanDecreaseGini, fill=MeanDecreaseGini)) +
  geom_bar(width=1, stat="identity") +theme_light() + labs(x = "Biomarker",y = "Importance",title = "AI Top 50")+
  scale_fill_viridis_b() +
  theme(axis.title.y=element_text(angle=0))
p +  coord_polar() +theme(axis.text.x = element_text(angle=-20)) +
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.y = element_text(angle=0, vjust = 0.5))+
  theme(text= element_text(size = 15))





library(plotrix)
radial.plot(imp$MeanDecreaseGini,labels = imp$varnames, main = "AI Top 50", rp.type = "s", radlab = TRUE)

# ----- This section prepare a dataframe for labels ---- #
# Get the name and the y position of each label
label_data <- imp

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$varnames-0.5) /number_of_bar     
# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)
# ----- ------------------------------------------- ---- #


# Start the plot
p <- ggplot(imp, aes(x=varnames, y=MeanDecreaseGini)) +       
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-100,120) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
  ) +
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +
  
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=label_data, aes(x=varnames, y=MeanDEcreaseGini+10, label=varnames, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

p
