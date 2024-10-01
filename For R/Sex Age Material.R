##################################################
### SEX AGE MATERIAL
### CREATED FEB 7, 2020
### LAST EDITED FEB 7, 2020
### liz.allyn@allyn.org
##################################################

# PACKAGES
library(ggplot2)
library(plyr) # rename
library(psych) # describeBy

# READ IN DATA
setwd("~/Projects/Sea Lion Entanglement Project/Upload to Box when edited")
sexage <- read.csv("Sex Age Material.csv")

# DATA MANIPULATION

AF <- data.frame(sexage[sexage$Sex_Age == "Adult_Female", 4:6])
AM <- data.frame(sexage[sexage$Sex_Age == "Adult_Male", 4:6])

AM[6,2] <- 0

Material <- c( "Flasher", "Hook and Line", "Monofilament", "Netting", "Packing band", "Rope", "Rubber band", "Scar", "Unknown")
Entanglements <- c(10, 1, 1, 0, 18, 0, 7, 14, 12)
Total <- c(rep(63, 9))

J <- data.frame(cbind(Material, Entanglements, Total))

AF$Material <- as.factor(AF$Material)
AM$Material <- as.factor(AM$Material)
J$Material <- as.factor(J$Material)

J$ID <- c(rep("Juvenile", 9))
AF$ID <- c(rep("Female", 9))
AM$ID <- c(rep("Male", 9))

Focus <- data.frame(rbind(AM, AF, J))

Focus$Total <- as.numeric(Focus$Total)
Focus$Entanglements <- as.numeric(Focus$Entanglements)

Focus$Proportion <- Focus$Entanglements/Focus$Total

Focus$ID <- factor(Focus$ID, levels = c("Juvenile", "Female", "Male"))

# PLOTTING

# Just AF, AM, Juv

# set the theme
theme.Focus <- theme_classic() +
  theme(axis.title = element_text(size = 17), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(hjust = 0.5, vjust = 2, color = "black"), 
        plot.title = element_blank(), 
        axis.text.x = element_text(size = 15, colour = "black"), 
        axis.text.y = element_text(size = 15, colour = "black"),
        axis.ticks.x = element_blank(),
        legend.position = "right", 
        legend.title = element_text(size = 17, colour = "black"), 
        legend.text = element_text(size = 15, colour = "black"), 
        legend.key.size = unit(3, "line")) # size of color boxes

# plot building
plot.Focus <- 
  ggplot(data = Focus, aes(x = ID, y = Proportion, fill = Material)) + 
  geom_col(position = "stack") + 
  labs(y = "Proportion of Entanglements") +
  theme.Focus +
  guides(color = guide_legend("Material")) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(name = "Material", 
                    labels = c("Flasher", 
                               "Hook and Line", 
                               "Monofilament", 
                               "Netting", 
                               "Packing band", 
                               "Rope",
                               "Rubber band", 
                               "Scar",
                               "Unknown"), 
                    values = c("Scar" = "bisque3",
                               "Unknown" = "darkgrey",
                               "Netting" = "darkslategrey",
                               "Hook and Line" = "skyblue4",
                               "Rope" = "darkseagreen4",
                               "Monofilament" = "darkseagreen2",
                               "Rubber band" = "skyblue2",
                               "Flasher" = "darksalmon",
                               "Packing band" = "khaki3"))

plot(plot.Focus)


# ANALYSIS

# read in data

SAM <- read.csv("Sex Age Material contingency.csv")

# set up matrix

SAM.matrix <- data.matrix(SAM, rownames.force = T)
SAM.wo.Unknown <- SAM.matrix[,1:9]
SAM.wo.FU <- SAM.wo.Unknown[,1:8]

colnames <- c("Hook", "Mono", "Net", "PB", "Rope", "RB", "Scar", "Unk")
rownames <- c("Female", "Male", "Juvenile")
SAM.wo.Flasher <- matrix(data = c(1,5,2,62,1,2,41,57,0,9,1,52,5,2,50,102,1,1,0,17,0,7,13,11),nrow = 3, ncol = 8, byrow = T, dimnames = list(rownames,colnames))
SAM.wo.PB <- matrix(data = c(1,5,2,1,2,41,3,57,0,9,1,5,2,50,10,102,1,1,0,0,7,13,9,11),nrow = 3, ncol = 8, byrow = T, dimnames = list(rownames,colnames))
SAM.wo.Mono <- matrix(data = c(1,2,62,1,2,41,3,57,0,1,52,5,2,50,10,102,1,0,17,0,7,13,9,11),nrow = 3, ncol = 8, byrow = T, dimnames = list(rownames,colnames))

# run fishers test

fisher.test(SAM.matrix, simulate.p.value = T)
fisher.test(SAM.wo.Unknown, simulate.p.value = T)
fisher.test(SAM.wo.Flasher, simulate.p.value = T)
fisher.test(SAM.wo.PB, simulate.p.value = T)
fisher.test(SAM.wo.Mono, simulate.p.value = T)
