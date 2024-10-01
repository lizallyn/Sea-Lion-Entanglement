##################################################
### PACKING BAND ANALYSIS
### CREATED FEB 4, 2020
### LAST EDITED FEB 4, 2020
### liz.allyn@allyn.org
##################################################

# PACKAGES
library(ggplot2)
library(reshape2)

# READ IN DATA
setwd("~/Projects/Sea Lion Entanglement Project/Upload to Box when edited")
PB <- read.csv("Packing band analysis.csv")

# DATA MANIPULATION

PB.long <- melt(data = PB, id.vars = c("Year"), variable.name = "Source")

# LINE WITHOUT 2018

PB2017 <- data.frame(PB[1:6,])
PB.2017long <- melt(data = PB2017, id.vars = c("Year"), variable.name = "Source")
PB.2017long$value <- as.numeric(PB.2017long$value)

# set the theme

theme.pb2017.line <- theme_classic() +
  theme(plot.margin = margin(t=20,r=10,b=10,l=10),
        axis.title = element_text(size = 30), 
        axis.title.x = element_text(color = "black", hjust = 0.5, vjust = 0), 
        axis.title.y = element_text(hjust = 0.5, vjust = 0, color = "black"), 
        plot.title = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 26, colour = "black"), 
        axis.text.y = element_text(size = 26, colour = "black"), 
        legend.position = "top", 
        legend.title = element_text(size = 34, colour = "black"), 
        legend.text = element_text(size = 30, colour = "black"), 
        legend.key.size = unit(6, "line")) # size of color boxes

# build the plot

plot.PB2017.line <- 
  ggplot(data = PB.2017long) +
  geom_line(aes(x = Year, y = value, group = Source, col = Source), size = 3) + 
  geom_point(aes(x = Year, y = value, col = Source), size = 10) +
  geom_point(aes(x = Year, y = value), col = "white", show.legend = T, size = 5) +
  labs(y = "Proportion of Entanglements \n") +
  theme.pb2017.line + 
  scale_color_manual(name = "Source:", 
                     labels = c("Entanglement", "Debris"), 
                     values = c("Entanglement" = "darksalmon", 
                                "Debris" = "darkseagreen")) +
  scale_y_continuous(sec.axis = dup_axis(name = "Packing Bands per Survey \n"))

plot(plot.PB2017.line)

# PLOTTING - NOT USED!

# SCATTER

# set the theme
theme.PB.scatter <- theme_classic() +
  theme(axis.title = element_text(size = 15), 
        axis.title.x = element_text(color = "black", hjust = 0.5, vjust = 1), 
        axis.title.y = element_text(hjust = 0.4, vjust = 1, color = "black"), 
        plot.title = element_blank(), 
        axis.text.x = element_text(size = 13, colour = "black"), 
        axis.text.y = element_text(size = 13, colour = "black"), 
        legend.position = "right", 
        legend.title = element_text(size = 15, colour = "black"), 
        legend.text = element_text(size = 13, colour = "black"))

# build the plot
scatter.pb <- 
  ggplot(data = PB, aes(x = Debris, y = Entanglement)) +
  theme.PB.scatter +
  geom_point(aes(col = Year), size = 4) +
  scale_color_brewer(palette = "Set2", direction = -1) +
  geom_smooth(method = "lm", se = F, size = 2, col = "darkgrey") 

# plot it
plot(scatter.pb)


# LINE PLOT

# set the theme

theme.PB.line <- theme_classic() +
  theme(axis.title = element_text(size = 15), 
        axis.title.x = element_text(color = "black", hjust = 0.5, vjust = 1), 
        axis.title.y = element_text(hjust = 0.5, vjust = 1, color = "black"), 
        plot.title = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 13, colour = "black"), 
        axis.text.y = element_text(size = 13, colour = "black"), 
        legend.position = "right", 
        legend.title = element_text(size = 15, colour = "black"), 
        legend.text = element_text(size = 13, colour = "black"))

# build the plot

plot.PB.line <- 
  ggplot(data = PB) +
  geom_line(aes(x = Year, y = Entanglement), col = "darksalmon", size = 1.5) +
  geom_line(aes(x = Year, y = Debris), col = "darkseagreen", size = 1.5) + 
  geom_point(aes(x = Year, y = Entanglement), col = "darksalmon", size = 3) +
  geom_point(aes(x = Year, y = Debris), col = "darkseagreen", size = 3) +
  labs(y = "Packing Band Occurrence") +
  theme.PB.line

# plotting

plot(plot.PB.line)

