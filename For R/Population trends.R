##################################################
### POPULATION TRENDS
### CREATED FEB 12, 2020
### LAST EDITED FEB 12, 2020
### liz.allyn@allyn.org
##################################################

# PACKAGES
library(ggplot2)

# READ IN DATA
setwd("~/Projects/Sea Lion Entanglement Project/Upload to Box when edited")
trends <- read.csv("Population trends.csv")
Zc <- read.csv("Population trends by month.csv")
Ej <- read.csv("Population trends by month Ej.csv")


# DATA MANIPULATION

trends$Year <- as.factor(trends$Year)

summary.Zc <- describeBy(Zc$Average, group = Zc$Month, digits = 4, mat = T, na.rm = F)
summary.Ej <- describeBy(Ej$Average, group = Ej$Month, digits = 4, mat = T, na.rm = F)

# reorder months
summary.Ej$group1 <- as.factor(summary.Ej$group1)
summary.Ej$group1 <- factor(summary.Ej$group1, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))

summary.Zc$group1 <- as.factor(summary.Zc$group1)
summary.Zc$group1 <- factor(summary.Zc$group1, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))

# PLOTTING

# set the theme

dodge <- position_dodge(0.2)

theme.poptrends <- theme_classic() +
  theme(plot.margin = margin(c(10, 10, 10, 10)),
        axis.title = element_text(size = 30), 
        axis.title.x = element_text(color = "black", hjust = 0.5, vjust = 0), 
        axis.title.y = element_text(hjust = 0.45, vjust = 2, color = "black"), 
        plot.title = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 26, colour = "black"), 
        axis.text.y = element_text(size = 26, colour = "black"), 
        legend.position = "right", 
        legend.title = element_text(size = 34, colour = "black"), 
        legend.text = element_text(size = 30, colour = "black"), 
        legend.key.size = unit(6, "line")) # size of color boxes

# build the plot

plot.poptrends <- 
  ggplot(data = trends) +
  geom_line(aes(x = Year, y = Count, col = Species, group = Species), size = 3) + 
  geom_errorbar(aes(x = Year, ymin = Count-CI, ymax = Count+CI, col = Species), 
                width = 0.2, 
                size = 2, 
                position = dodge) +
  geom_point(aes(x = Year, y = Count, col = Species), size = 10) +
  geom_point(aes(x = Year, y = Count), col = "white", size = 5, show.legend = T) +
  labs(y = "Average Count") +
  theme.poptrends + 
  scale_color_manual(name = "Species", 
                    labels = c("California", "Steller"), 
                    values = c("Steller" = "darkgoldenrod", "California" = "burlywood4"))

# plotting

plot(plot.poptrends)

# PLOTTING

# Month Counts FOR MS

# limits
limits <- aes(ymax = mean + (se), ymin = mean - (se), width = 0.5)
dodge <- position_dodge(width=0.9)

# set the theme
theme.Zc <- theme_classic() +
  theme(plot.margin = margin(c(10, 10, 10, 10)),
        axis.title = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        plot.title = element_blank(), 
        axis.text.x = element_text(size = 25, colour = "black"), 
        axis.text.y = element_text(size = 25, colour = "black"), 
        axis.ticks.x = element_blank())

# plot building Zc Year
plot.Zc <- 
  ggplot(data = summary.Zc, aes(x = group1, y = mean)) + 
  geom_bar(stat = "identity", position = dodge, fill = "burlywood4") + 
  geom_errorbar(limits, position = dodge, color = "black") + 
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme.Zc

# plot
plot(plot.Zc)

# Ej Month Counts

# set the theme
theme.Ej <- theme_classic() +
  theme(plot.margin = margin(c(10, 10, 10, 10)),
        axis.title = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        plot.title = element_blank(), 
        axis.text.x = element_text(size = 25, colour = "black"), 
        axis.text.y = element_text(size = 25, colour = "black"), 
        axis.ticks.x = element_blank())

# plot building Ej Year
plot.Ej <- 
  ggplot(data = summary.Ej, aes(x = group1, y = mean)) + 
  geom_bar(stat = "identity", position = dodge, fill = "darkgoldenrod") + 
  geom_errorbar(limits, position = dodge, color = "black") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme.Ej

plot(plot.Ej)
