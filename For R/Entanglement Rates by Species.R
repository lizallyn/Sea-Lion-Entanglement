##################################################
### ENTANGLEMENT RATES BY SPECIES
### CREATED FEB 4, 2020
### LAST EDITED FEB 6, 2020
### liz.allyn@allyn.org
##################################################

# PACKAGES
library(ggplot2)
library(psych)
library(png) # add pic to plots
library(grid)
library(extrafont)

# READ IN DATA

setwd("~/Projects/Sea Lion Entanglement Project/Upload to Box when edited")
ZcRates <- read.csv("Zc Rate for R.csv")
EjRates <- read.csv("Ej Rate for R.csv")

# read in pics

img.ej <- readPNG("~/Projects/Sea Lion Entanglement Project/Upload to Box when edited/4_Packing band_20160810_02Ej1_2.png")
ej_pic <- rasterGrob(img.ej, interpolate = T)

img.zc <- readPNG("~/Projects/Sea Lion Entanglement Project/Upload to Box when edited/2_Packing Band_20100623_12Zc1_1.png")
zc_pic <- rasterGrob(img.zc, interpolate = T)

# DATA MANIPULATION

# year summaries
summary.ZcER <- describeBy(ZcRates$Rate, group = ZcRates$Year, digits = 4, mat = T, na.rm = F)
summary.EjER <- describeBy(EjRates$Rate, group = EjRates$Year, digits = 4, mat = T, na.rm = F)

# month summary
summary.EjM <- describeBy(EjRates$Rate, group = EjRates$Month, mat = T, digits = 4, na.rm = F)
summary.ZcM <- describeBy(ZcRates$Rate, group = ZcRates$Month, mat = T, digits = 4, na.rm = F)
# reorder months
summary.EjM$group1 <- as.factor(summary.EjM$group1)
summary.EjM$group1 <- factor(summary.EjM$group1, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))

summary.ZcM$group1 <- as.factor(summary.ZcM$group1)
summary.ZcM$group1 <- factor(summary.ZcM$group1, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))

# PLOTTING

# YEAR RATES FOR MS

# limits
limits <- aes(ymax = mean + (se), ymin = mean - (se), width = 0.5)
dodge <- position_dodge(width=0.9)

# set the theme
theme.ZcER <- theme_classic() +
  theme(plot.margin = margin(c(10, 10, 10, 10)),
        axis.title = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        plot.title = element_blank(), 
        axis.text.x = element_text(size = 25, colour = "black"), 
        axis.text.y = element_text(size = 25, colour = "black"), 
        axis.ticks.x = element_blank())

# plot building Zc Year
plot.ZcER <- 
  ggplot(data = summary.ZcER, aes(x = group1, y = mean)) + 
  geom_bar(stat = "identity", position = dodge, fill = "burlywood4") + 
  geom_errorbar(limits, position = dodge, color = "black") + 
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme.ZcER

# plot
plot(plot.ZcER)

# set the theme
theme.EjER <- theme_classic() +
  theme(plot.margin = margin(c(10, 10, 10, 10)),
        axis.title = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        plot.title = element_blank(), 
        axis.text.x = element_text(size = 25, colour = "black"), 
        axis.text.y = element_text(size = 25, colour = "black"), 
        axis.ticks.x = element_blank())
       
# plot building Ej Year
plot.EjER <- 
  ggplot(data = summary.EjER, aes(x = group1, y = mean)) + 
  geom_bar(stat = "identity", position = dodge, fill = "darkgoldenrod") + 
  geom_errorbar(limits, position = dodge, color = "black") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme.EjER

plot(plot.EjER)

#Month Ej Rate for MS

# add pic?
img.ej <- readPNG("~/Projects/Sea Lion Entanglement Project/Upload to Box when edited/4_Packing band_20160810_02Ej1_2.png")
ej_pic <- rasterGrob(img.ej, interpolate = T)

# set the theme

theme.Month <- theme_classic() +
  theme(plot.margin = margin(c(10, 10, 10, 10)),
        axis.title = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.ticks.x = element_blank(),
        plot.title = element_blank(), 
        axis.text.x = element_text(size = 25, colour = "black"), 
        axis.text.y = element_text(size = 25, colour = "black"))

# plot building Ej Month
plot.EjM <- 
  ggplot(data = summary.EjM, aes(x = group1, y = mean)) + 
  geom_bar(stat = "identity", position = dodge, fill = "darkgoldenrod") + 
  geom_errorbar(limits, position = dodge, color = "black") + 
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme.Month 


plot(plot.EjM)

# Zc Months plot

# set the theme

theme.ZcMonth <- theme_classic() +
  theme(plot.margin = margin(c(10, 10, 10, 10)),
        axis.title = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.ticks.x = element_blank(),
        plot.title = element_blank(), 
        axis.text.x = element_text(size = 25, colour = "black"), 
        axis.text.y = element_text(size = 25, colour = "black"))

# plot building Zc Month
plot.ZcM <- 
  ggplot(data = summary.ZcM, aes(x = group1, y = mean)) + 
  geom_bar(stat = "identity", position = dodge, fill = "burlywood4") + 
  geom_errorbar(limits, position = dodge, color = "black") + 
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme.ZcMonth 


plot(plot.ZcM)
