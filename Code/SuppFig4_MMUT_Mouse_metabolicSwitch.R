### METABOLIC SWITCH IN A MOUSE MODEL OF METHYLMALONIC ACIDURIA
# Supp Fig. 4

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
require(cowplot)

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
# SUPP. FIGURE 4
####################################################################
####################################################################

# theme & info:
# insulin and glucose
# a. glucose course
# b. islets area in pancreas

####################################################################
# DATA IMPORT AND TIDY
####################################################################



# glucose course

tbl4a_supp0 <- data.table(read_excel("Data/SuppFig4/PF_SuppFig4a_Glucose.xlsx"))
colnames(tbl4a_supp0)
setnames(tbl4a_supp0, c("time", rep("test", 69)))

tbl4a_supp <- rbind(tbl4a_supp0[, c(2:18)], tbl4a_supp0[, c(19:35)], tbl4a_supp0[, c(36:52)], tbl4a_supp0[, c(53:69)])
ncol(tbl4a_supp)
setnames(tbl4a_supp, paste("test", seq(1,17), sep = "_"))

tbl4a_supp <- tbl4a_supp %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric)), na.rm = TRUE), sd = sd(c_across(where(is.numeric)), na.rm = TRUE))
tbl4a_supp <- cbind(data.table(tbl4a_supp[, c("mean", "sd")]))
tbl4a_supp[, type := c(rep(mylvls[3],6), rep(mylvls[4],6), rep(mylvls[1],6), rep(mylvls[2],6))]
tbl4a_supp[, time := c(rep(tbl4a_supp0$time,4))]





# islets area in pancreas

tbl4e_supp <- data.table(read_excel("Data/SuppFig4/PF_SuppFig4E_pancreasislets.xlsx"))
setnames(tbl4e_supp, c("type", "area", "gender", "type2"))
tbl4e_supp[type == mylvls2[1], type3 := "control"]
tbl4e_supp[type == mylvls2[2], type3 := "mutant"]






####################################################################
# PLOTS AND ARRANGE AND SAVE
####################################################################

margs = c(0.5, 0.5, 0.5, 0.5)
margin = theme(plot.margin = unit(margs, "cm"))



# a. glucose course
plt4a_supp <- 
ggplot(tbl4a_supp, aes(x = time, y = mean, color = type)) +
	geom_line() +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6, width = 2) +
	geom_point(size = 2) +
	ylab("Glucose [mmol/L]") +
	xlab("Time [min]") +
	guides(color = guide_legend(nrow = 2)) +
	theme_pubr() +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none") +
	margin




# b. islets area in pancreas

compare <- list(c("control", "mutant"))

max_val <- max(tbl4e_supp$area)
min_val <- min(tbl4e_supp$area)

plt4e_supp <- 
ggplot(tbl4e_supp, aes(x = type3, y = area)) +
	geom_boxplot(alpha = 0.1, outlier.shape = NA, color = "black", fill = "black") +
	geom_jitter(aes(color = type2), width = 0.1) +
	stat_compare_means(comparisons = compare, size = 3) +
	ylab("Islet/pancreatic area [%]") +
	ylim(0.9*min_val,1.1*max_val) +
	scale_color_manual(values = mypal) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	theme(legend.title = element_blank(), legend.position = "right", axis.title.x = element_blank()) +
	margin






plt4_supp <-
plot_grid(plt4a_supp, plt4e_supp, align = "vh", nrow = 1, labels = "auto", rel_widths = c(1, 1.1))

ggsave(paste(fig_path, "SuppFig4.png", sep = ""), plt4_supp, device = png(), width = 6, height = 3)
dev.off()

ggsave(paste(fig_path, "SuppFig4.pdf", sep = ""), plt4_supp, device = "pdf", width = 6, height = 3)
dev.off()
















