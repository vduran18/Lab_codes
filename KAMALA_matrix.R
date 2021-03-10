# Lattice Correlation Plot
library(ggplot2)
library(reshape)
library(reshape2)
library(dplyr)
library("ggpubr")
library("Hmisc")
library(readxl)

##ALCAM Brightness ratio

alcam <- read_excel("/Users/valeriaduran/Desktop/matrix data_VD.xlsx", sheet = 2)
# 24th column has renal involvement, did not include for this figure
#rawdata2 <- rawdata[(rawdata$Disease.Status == 3) | (rawdata$Disease.Status ==2),]
alcam_matrix <- as.matrix(alcam)


#Plot Correlation 
ggplot( alcam_matrix ) + geom_raster( aes( x = Immobilized, y = Functionalized, fill = SNR ) ) + scale_fill_gradient2( low = "black", high = "white") +geom_point(aes( x = Immobilized, y = Functionalized, size= Brightness Ratio), colour = rgb(245, 127, 32, maxColorValue = 255)) + theme(text = element_text(size = 20), axis.text.x = element_text(hjust = 1, angle = 90, colour = "black"), axis.text.y = element_text(colour = "black")) +ggtitle("ALCAM") +theme(plot.title = element_text(hjust = 0.5)) 

p3 <- ggplot(alcam) +
  geom_raster(aes(x=Immobilized, y= Functionalized, fill=SNR)) +
  scale_fill_gradient(low = "white", high = "black") + 
  geom_point(aes(x=Immobilized, y=Functionalized, size=Brightness_Ratio), colour = rgb(245,127,32, maxColorValue = 255)) +
  scale_x_discrete(position = "top") +
  theme(legend.position = "none", text = element_text(size = 12), axis.text.x = element_text(hjust = 1, angle = 90, colour = "black"), axis.text.y = element_text(colour = "black"), plot.title = element_text(hjust = 0.5, size=20)) +
  ggtitle("ALCAM") 

  
GPLOT



ZPLOT <- ggplot(alcam) +
  geom_raster(aes(x=Immobilized, y= Functionalized, fill=SNR)) +
  scale_fill_gradient(low = "black", high = "white") + 
  theme(legend.position='bottom', legend.direction='horizontal')



ZPLOT

library(gridExtra)
library(gtable)

### Step 1
# Draw a plot with the colour legend
p1 <- ggplot(alcam) +
  geom_raster(aes(x=Immobilized, y= Functionalized, fill=SNR)) +
  scale_fill_gradient(low = "white", high = "black") + 
  theme(legend.position='bottom', legend.direction='horizontal')
  
# Extract the colour legend - leg1
leg1 <- gtable_filter(ggplot_gtable(ggplot_build(p1)), "guide-box") 

### Step 2
# Draw a plot with the size legend
p2 <- ggplot(alcam) + geom_point(aes(x=Immobilized, y=Functionalized, size=Brightness_Ratio), colour = rgb(245,127,32, maxColorValue = 255)) +
    theme(legend.position='right', legend.direction='vertical') + 
    scale_x_discrete(position = "top") + labs(size = "Brightness Ratio")

#Extract the size legend - leg2
leg2 <- gtable_filter(ggplot_gtable(ggplot_build(p2)), "guide-box") 

# Step 3
# Draw a plot with no legends - plot
GPLOT <- ggplot(alcam) +
  geom_raster(aes(x=Immobilized, y= Functionalized, fill=SNR)) +
  scale_fill_gradient(low = "white", high = "black") + 
  geom_point(aes(x=Immobilized, y=Functionalized, size=Brightness_Ratio), colour = rgb(245,127,32, maxColorValue = 255)) +
  theme(legend.position = "none")) +
  scale_x_discrete(position = "top") +
  theme(text = element_text(size = 12), axis.text.x = element_text(hjust = 1, angle = 90, colour = "black"), axis.text.y = element_text(colour = "black"), plot.title = element_text(hjust = 0.5)) +
  ggtitle("ALCAM")
p3
### Step 4
# Arrange the three components (plot, leg1, leg2)
# The two legends are positioned outside the plot: 
# one at the top and the other to the side.
plotNew <- arrangeGrob(p3, leg1, 
                       heights = unit.c(unit(1, "npc") - leg1$height, leg1$height), ncol = 1)

plotNew <- arrangeGrob(plotNew, leg2,
                       widths = unit.c(unit(1, "npc") - leg2$width, leg2$width), nrow = 1)

grid.newpage()
grid.draw(plotNew)


##hvem Brightness ratio

hvem <- read_excel("/Users/valeriaduran/Desktop/matrix data_VD.xlsx", sheet = 4)

plot_hvem <- ggplot(hvem) +
  geom_raster(aes(x=Immobilized, y= Functionalized, fill=SNR)) +
  scale_fill_gradient(low = "white", high = "black", na.value = "white") + 
  geom_point(aes(x=Immobilized, y=Functionalized, size=Brightness_Ratio), colour = rgb(245,127,32, maxColorValue = 255)) +
  scale_x_discrete(position = "top") +
  theme(legend.position = "none", text = element_text(size = 12), axis.text.x = element_text(hjust = 1, angle = 90, colour = "black"), axis.text.y = element_text(colour = "black"), plot.title = element_text(hjust = 0.5, size=20)) +
  ggtitle("HVEM") 


plot_hvem



ZPLOT <- ggplot(alcam) +
  geom_raster(aes(x=Immobilized, y= Functionalized, fill=SNR)) +
  scale_fill_gradient(low = "black", high = "white") + 
  theme(legend.position='bottom', legend.direction='horizontal')



ZPLOT

library(gridExtra)
library(gtable)

### Step 1
# Draw a plot with the colour legend
p1 <- ggplot(hvem) +
  geom_raster(aes(x=Immobilized, y= Functionalized, fill=SNR)) +
  scale_fill_gradient(low = "white", high = "black") + 
  theme(legend.position='bottom', legend.direction='horizontal')

# Extract the colour legend - leg1
leg1 <- gtable_filter(ggplot_gtable(ggplot_build(p1)), "guide-box") 

### Step 2
# Draw a plot with the size legend
p2 <- ggplot(hvem) + geom_point(aes(x=Immobilized, y=Functionalized, size=Brightness_Ratio), colour = rgb(245,127,32, maxColorValue = 255)) +
  theme(legend.position='right', legend.direction='vertical') + 
  scale_x_discrete(position = "top") + labs(size = "Brightness Ratio")

#Extract the size legend - leg2
leg2 <- gtable_filter(ggplot_gtable(ggplot_build(p2)), "guide-box") 

# Step 3
# Draw a plot with no legends - plot
GPLOT <- ggplot(alcam) +
  geom_raster(aes(x=Immobilized, y= Functionalized, fill=SNR)) +
  scale_fill_gradient(low = "white", high = "black") + 
  geom_point(aes(x=Immobilized, y=Functionalized, size=Brightness_Ratio), colour = rgb(245,127,32, maxColorValue = 255)) +
  theme(legend.position = "none")) +
  scale_x_discrete(position = "top") +
  theme(text = element_text(size = 12), axis.text.x = element_text(hjust = 1, angle = 90, colour = "black"), axis.text.y = element_text(colour = "black"), plot.title = element_text(hjust = 0.5)) +
  ggtitle("ALCAM")
p3
### Step 4
# Arrange the three components (plot, leg1, leg2)
# The two legends are positioned outside the plot: 
# one at the top and the other to the side.
plotNew <- arrangeGrob(plot_hvem, leg1, 
                       heights = unit.c(unit(1, "npc") - leg1$height, leg1$height), ncol = 1)

plotNew <- arrangeGrob(plotNew, leg2,
                       widths = unit.c(unit(1, "npc") - leg2$width, leg2$width), nrow = 1)

grid.newpage()
grid.draw(plotNew)

