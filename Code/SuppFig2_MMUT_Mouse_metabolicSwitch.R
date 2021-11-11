# wd
Dropbox/PF/Post_PhD/Zurich_2019_2022/MMUT_Mouse_metabolicSwitch

# libraries
require(data.table)
require(ggplot2)
require(tidyverse)
require(readxl)
require(ggpubr)
library(cowplot)

# color pallette (manual): order:
# female-KI/WT; female-KO/KI
# male_KI/WT; male-KO/KI
# middle color female: #FF8100
# middle color male: #008080
mypal = c("#ffa64c", "#b25a00", "#66b2b2", "#004c4c")
mypal2 = c("#856cb7", "#340a88")
mylvls = c("f_Mmut-ki/wt", "f_Mmut-ko/ki", "m_Mmut-ki/wt", "m_Mmut-ko/ki")
mylvls2 = c("Mmut-ki/wt", "Mmut-ko/ki")

# create figures path
system("mkdir Figs")
system("mkdir Figs/v3")
fig_path <- c("Figs/v3/")



####################################################################
####################################################################
# SUPP. FIGURE 4
####################################################################
####################################################################

# theme & info:
# insulin and glucose
# a. glucose course
# b. plasma insulin fed
# c. histo
# d. islets area in pancreas

####################################################################
# DATA IMPORT AND TIDY
####################################################################



# a. glucose course

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




# b. plasma insulin fed

tbl4b_supp <- data.table(read_excel("Data/SuppFig4/PF_SuppFig4b_insulinFED.xlsx"))
tbl4b_supp <- melt.data.table(tbl4b_supp)
setnames(tbl4b_supp, c("type", "value"))
tbl4b_supp <- tbl4b_supp[!is.na(value), ]



# c. islets area in pancreas

tbl4e_supp <- data.table(read_excel("Data/SuppFig4/PF_SuppFig4E_pancreasislets.xlsx"))
tbl4e_supp <- t(tbl4e_supp)
tbl4e_supp <- as.data.table(tbl4e_supp)
tbl4e_supp <- tbl4e_supp[-1, ]
tbl4e_supp[, type := c(rep(mylvls2[2], 6), rep(mylvls2[1], 6))]
tbl4e_supp$type <- factor(tbl4e_supp$type, levels = c(mylvls2[1], mylvls2[2]))
setnames(tbl4e_supp, c("value", "gender", "type"))
tbl4e_supp$value <- as.numeric(tbl4e_supp$value)
tbl4e_supp[type == mylvls2[1]]$type <- "control"
tbl4e_supp[type == mylvls2[2]]$type <- "mutant"



# further variables for plotting

margs = c(0.5, 0.5, 0.5, 0.5)
margin = theme(plot.margin = unit(margs, "cm"))



####################################################################
# PLOTS AND ARRANGE AND SAVE
####################################################################


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



# b. plasma insulin fed

# compare <- list(mylvls[c(1,2)], mylvls[c(3,4)])

# max_val <- max(tbl4b_supp$value)
# min_val <- min(tbl4b_supp$value)

# plt4b_supp <- 
# ggplot(tbl4b_supp, aes(x = type, y = value, color = type, fill = type)) +
# 	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
# 	geom_jitter(width = 0.1) +
# 	stat_compare_means(comparisons = compare) +
# 	ylab("Plasma insulin [pg/mL]") +
# 	ylim(0.9*min_val,1.2*max_val) +
# 	theme_pubr() +
# 	rotate_x_text(angle = 45) +
# 	scale_fill_manual(values = mypal) +
# 	scale_color_manual(values = mypal) +
# 	theme(legend.title = element_blank(), legend.position = "right", axis.title.x = element_blank(), axis.text.x = element_blank()) +
# 	margin



# c. islets area in pancreas

compare <- list(c("control", "mutant"))

max_val <- max(tbl4e_supp$value)
min_val <- min(tbl4e_supp$value)

plt4e_supp <- 
ggplot(tbl4e_supp, aes(x = type, y = value)) +
	geom_boxplot(alpha = 0.1, outlier.shape = NA, color = "black", fill = "black") +
	geom_jitter(data = tbl4e_supp[gender == "female", ], width = 0.1, color = mypal[2], alpha = 1) +
	geom_jitter(data = tbl4e_supp[gender == "male", ], width = 0.1, color = mypal[4], alpha = 1) +
	stat_compare_means(comparisons = compare, size = 3) +
	ylab("Islet/pancreatic area [%]") +
	ylim(0.9*min_val,1.1*max_val) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	theme(legend.title = element_blank(), legend.position = "right", axis.title.x = element_blank()) +
	margin






plt4_supp <-
plot_grid(plt4a_supp, plt4e_supp, align = "vh", nrow = 1, labels = "auto", rel_widths = c(1, 0.5))

ggsave(paste(fig_path, "SuppFig2.png", sep = ""), plt4_supp, device = png(), width = 6, height = 3)
dev.off()

ggsave(paste(fig_path, "SuppFig2.pdf", sep = ""), plt4_supp, device = "pdf", width = 6, height = 3)
dev.off()
















