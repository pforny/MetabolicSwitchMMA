### METABOLIC SWITCH IN A MOUSE MODEL OF METHYLMALONIC ACIDURIA
# Supp Fig. 5

# author: Patrick Forny

# run R one level above the Code directory. Create folders Data and Figs to start with.
## folders:
# Code: scripts
# Data: data for analyses
# Figs: output of analyses


# libraries
require(data.table)
require(ggplot2)
require(tidyverse)
require(readxl)
require(ggpubr)
library(ggplotify)

# color pallette (manual): order:
# female-KI/WT; female-KO/KI
# male_KI/WT; male-KO/KI
mypal = c("#ffa64c", "#b25a00", "#66b2b2", "#004c4c")
mypal2 = c("#856cb7", "#340a88")
mylvls = c("f_Mmut-ki/wt", "f_Mmut-ko/ki", "m_Mmut-ki/wt", "m_Mmut-ko/ki")
mylvls2 = c("Mmut-ki/wt", "Mmut-ko/ki")

# create figures path
system("mkdir Figs")
system("mkdir Figs/v19")
fig_path <- c("Figs/v19/")

system("mkdir Figs")
system("mkdir Figs/tablesv19")
fig_tbl_path <- c("Figs/tablesv19/")



####################################################################
####################################################################
# SUPP. FIGURE 5
####################################################################
####################################################################

# theme & info
# glucose over time

####################################################################
# DATA IMPORT AND TIDY
####################################################################




# glucose over time

tbl7e <- data.table(read_excel("Data/Fig7/PF_Fig7E_glcOverTime.xlsx"))
colnames(tbl7e)

fkoki7e00 <- tbl7e[, c(2:9)]
fkoki7e0 <- fkoki7e00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
fkoki7e <- cbind(data.table(fkoki7e0[, c("mean", "sd")]), 
	type = rep(mylvls[2], length(fkoki7e0$mean)),
	time = tbl7e$time)

fkiwt7e00 <- tbl7e[, c(10:17)]
fkiwt7e0 <- fkiwt7e00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
fkiwt7e <- cbind(data.table(fkiwt7e0[, c("mean", "sd")]), 
	type = rep(mylvls[1], length(fkiwt7e0$mean)),
	time = tbl7e$time)

f7e <- rbind(fkiwt7e, fkoki7e)





####################################################################
# PLOT
####################################################################




# glucose over time

plt_glcTime <- 
ggplot(f7e, aes(x = time, y = mean, color = type)) +
	geom_line() +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6, width = 2) +
	geom_point(size = 2) +
	ylab("Glucose [mmol/L]") +
	xlab("Time [min]") +
	theme_pubr() +
	scale_color_manual(values = mypal[c(1,2)]) +
	theme(legend.title = element_blank(), legend.position = c(0.8, 0.8))






pltsupp3 <- 
ggarrange(plt_glcTime, labels = "b", ncol = 1, nrow = 1)


ggsave(paste(fig_path, "SuppFig5.png", sep = ""), pltsupp3, device = png(), width = 4, height = 3, bg = "white")
dev.off()

ggsave(paste(fig_path, "SuppFig5.pdf", sep = ""), pltsupp3, device = "pdf", width = 4, height = 3, bg = "white")
dev.off()

