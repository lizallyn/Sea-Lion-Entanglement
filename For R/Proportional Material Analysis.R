##################################################
### PROPORTIONAL MATERIAL ANALYSIS
### CREATED FEB 4, 2020
### LAST EDITED FEB 6, 2020
### liz.allyn@allyn.org
##################################################

# PACKAGES
library(ggplot2)
library(reshape2) # melt
library(plyr) # rename

# READ IN DATA

# at work
setwd("~/Projects/Sea Lion Entanglement Project/Upload to Box when edited")
# read in data
EjYear <- read.csv("Ej Year_Proportional Material Analysis.csv")
ZcYear <- read.csv("Zc Year_Proportional Material Analysis.csv")
BothYear <- read.csv("Both Year_Proportional Material Analysis.csv")
BothMonth <- read.csv("Both Month_Proportional Material Analysis.csv")
EjMonth <- read.csv("Ej Month_Proportional Material Analysis.csv")
ZcMonth <- read.csv("Zc Month_Proportional Material Analysis.csv")

# DATA MANIPULATION

# Make year a factor
EjYear$Year <- as.factor(EjYear$Year)
ZcYear$Year <- as.factor(ZcYear$Year)
BothYear$Year <- as.factor(BothYear$Year)
BothMonth$Month <- as.factor(BothMonth$Month)
EjMonth$Month <- as.factor(EjMonth$Month)
ZcMonth$Month <- as.factor(ZcMonth$Month)

# rename materials without underscores
EjYear <- rename(EjYear, c("Packing.band" = "Packing band", "Hook.and.Line" = "Hook and Line", "Rubber.band" = "Rubber band"))
ZcYear <- rename(ZcYear, c("Packing.band" = "Packing band"))
BothYear <- rename(BothYear, c("Packing.band" = "Packing band", "Hook.and.Line" = "Hook and Line", "Rubber.band" = "Rubber band"))
BothMonth <- rename(BothMonth, c("Packing.band" = "Packing band", "Hook.and.Line" = "Hook and Line", "Rubber.band" = "Rubber band"))
EjMonth <- rename(EjMonth, c("Packing.band" = "Packing band", "Hook.and.Line" = "Hook and Line", "Rubber.band" = "Rubber band"))
ZcMonth <- rename(ZcMonth, c("Packing.band" = "Packing band"))

# switch to long format
EjYear.long <- melt(data = EjYear, id.vars = c("Year"), variable.name = "Material")
ZcYear.long <- melt(data = ZcYear, id.vars = c("Year"), variable.name = "Material")
BothYear.long <- melt(data = BothYear, id.vars = c("Year"), variable.name = "Material")
BothMonth.long <- melt(data = BothMonth, id.vars = c("Month"), variable.name = "Material")
EjMonth.long <- melt(data = EjMonth, id.vars = c("Month"), variable.name = "Material")
ZcMonth.long <- melt(data = ZcMonth, id.vars = c("Month"), variable.name = "Material")

dodge <- position_dodge(width=0.9)

# PLOT FOR MS - YEAR

# Plot Ej Year

# set the theme
theme.Year <- theme_classic() +
  theme(plot.margin = margin(t=10,r=10,b=10,l=10),
        axis.title = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        plot.title = element_blank(), 
        axis.text.x = element_text(size = 25, colour = "black"), 
        axis.text.y = element_text(size = 25, colour = "black"),
        axis.ticks.x = element_blank(),
        legend.position = "right", 
        legend.title = element_text(size = 30, colour = "black"), 
        legend.text = element_text(size = 25, colour = "black"), 
        legend.key.size = unit(2.5, "line")) # size of color boxes

# plot building
plot.EjYear <- 
  ggplot(data = EjYear.long, aes(x = Year, y = value, fill = Material)) + 
  geom_col(position = "stack") + 
  labs(x = "Year", y = "Proportion of Entanglements") +
  theme.Year +
  guides(color = guide_legend("Material")) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(name = "Material", 
                    labels = c("Packing band", "Flasher", "Rubber band", "Monofilament", "Hook and Line","Netting", "Rope", "Unknown", "Scar"), 
                    values = c("Scar" = "bisque3",
                               "Unknown" = "darkgrey",
                               "Rope" = "darkseagreen4",
                               "Netting" = "darkslategrey",
                               "Hook and Line" = "skyblue4",
                               "Monofilament" = "darkseagreen2",
                               "Rubber band" = "skyblue2",
                               "Flasher" = "darksalmon",
                               "Packing band" = "khaki3"))

plot(plot.EjYear)

# Plot Zc Year

# set the theme
theme.YearZc <- theme_classic() +
  theme(plot.margin = margin(c(10, 10, 10, 10)),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        plot.title = element_blank(), 
        axis.text.x = element_text(size = 25, colour = "black"), 
        axis.text.y = element_text(size = 25, colour = "black"), 
        axis.ticks.x = element_blank(),
        legend.position = "none")
        

plot.YearZc <- 
  ggplot(data = ZcYear.long, aes(x = Year, y = value, fill = Material)) + 
  geom_col(position = "stack") + 
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme.YearZc +
  guides(color = guide_legend("Material")) +
  scale_fill_manual(name = "Material", 
                    labels = c("Packing band", "Monofilament", "Rope", "Flasher", "Unknown", "Scar"), 
                    values = c("Scar" = "bisque3",
                               "Unknown" = "darkgrey",
                               "Rope" = "darkseagreen4",
                               "Monofilament" = "darkseagreen2",
                               "Flasher" = "darksalmon",
                               "Packing band" = "khaki3"))

# plot

plot(plot.YearZc)

# MONTH EJ MA PLOT FOR MS

EjMonth.long$Month <- as.factor(EjMonth.long$Month)

# plotting months
theme.Month <- theme_classic() +
  theme(plot.margin = margin(c(10, 10, 10, 10)),
        axis.title = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.ticks.x = element_blank(),
        plot.title = element_blank(), 
        axis.text.x = element_text(size = 25, colour = "black"), 
        axis.text.y = element_text(size = 25, colour = "black"), 
        legend.position = "right", 
        legend.title = element_text(size = 30, colour = "black"), 
        legend.text = element_text(size = 25, colour = "black"), 
        legend.key.size = unit(2.5, "line"))# size of color boxes 

plot.EjMonth <- 
  ggplot(data = EjMonth.long, aes(x = Month, y = value, fill = Material)) + 
  geom_col(position = "stack") + 
  theme.Month +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  guides(color = guide_legend("Material")) +
  scale_fill_manual(name = "Material", 
                    labels = c("Packing band", "Flasher", "Monofilament", "Rubber band", "Hook and Line","Netting", "Rope", "Unknown", "Scar"), 
                    values = c("Scar" = "bisque3",
                               "Unknown" = "darkgrey",
                               "Netting" = "darkslategrey",
                               "Hook and Line" = "skyblue4",
                               "Rope" = "darkseagreen4",
                               "Monofilament" = "darkseagreen2",
                               "Rubber band" = "skyblue2",
                               "Flasher" = "darksalmon",
                               "Packing band" = "khaki3"))
plot(plot.EjMonth)

# Zc Months

# plotting months
theme.ZcMonth <- theme_classic() +
  theme(plot.margin = margin(c(10, 10, 10, 10)),
        axis.title = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.ticks.x = element_blank(),
        plot.title = element_blank(), 
        axis.text.x = element_text(size = 25, colour = "black"), 
        axis.text.y = element_text(size = 25, colour = "black"), 
        legend.position = "none")

plot.ZcMonth <- 
  ggplot(data = ZcMonth.long, aes(x = Month, y = value, fill = Material)) + 
  geom_col(position = "stack") + 
  theme.ZcMonth +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  guides(color = guide_legend("Material")) +
  scale_fill_manual(name = "Material", 
                    labels = c("Packing band", "Flasher", "Monofilament", "Rubber band", "Hook and Line","Netting", "Rope", "Unknown", "Scar"), 
                    values = c("Scar" = "bisque3",
                               "Unknown" = "darkgrey",
                               "Netting" = "darkslategrey",
                               "Hook and Line" = "skyblue4",
                               "Rope" = "darkseagreen4",
                               "Monofilament" = "darkseagreen2",
                               "Rubber band" = "skyblue2",
                               "Flasher" = "darksalmon",
                               "Packing band" = "khaki3"))
plot(plot.ZcMonth)
