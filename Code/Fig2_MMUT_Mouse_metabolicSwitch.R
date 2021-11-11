# wd
Dropbox/PF/Post_PhD/Zurich_2019_2022/MMUT_Mouse_metabolicSwitch

# libraries
require(data.table)
require(ggplot2)
require(tidyverse)
require(readxl)
require(ggpubr)

# color pallette (manual): order:
# female-KI/WT; female-KO/KI
# male_KI/WT; male-KO/KI
# middle color female: #FF8100
# middle color male: #008080
mypal = c("#ffa64c", "#b25a00", "#66b2b2", "#004c4c")
mylvls = c("f_Mmut-ki/wt", "f_Mmut-ko/ki", "m_Mmut-ki/wt", "m_Mmut-ko/ki")

# create figures path
system("mkdir Figs")
system("mkdir Figs/v3")
fig_path <- c("Figs/v3/")



####################################################################
####################################################################
# FIGURE 2
####################################################################
####################################################################

# theme & info:
# avg. body surface temp
# Fig2A is histology

####################################################################
# DATA IMPORT AND TIDY
####################################################################



# a. body surface temperature

tbl2b <- data.table(read_excel("Data/Fig2/PF_Fig2B.xlsx"))
colnames(tbl2b) <- mylvls
tbl2b_melt <- melt.data.table(tbl2b)
tbl2b_melt <- tbl2b_melt[!is.na(value), ]

compare2b <- list(c(mylvls[1], mylvls[2]), c(mylvls[3], mylvls[4]))





# b. leptin

tbl1b_supp <- data.table(read_excel("Data/SuppFig1/PF_SuppFig1_leptin.xlsx"))
setnames(tbl1b_supp, mylvls)
tbl1b_supp <- melt.data.table(tbl1b_supp, measure.vars = mylvls)
tbl1b_supp$value <- as.numeric(tbl1b_supp$value)
tbl1b_supp <- tbl1b_supp[!is.na(tbl1b_supp$value), ]
setnames(tbl1b_supp, c("type", "leptin"))



# c. leptin / fat mass

tbl1c_supp <- data.table(read_excel("Data/SuppFig1/PF_SuppFig1_fatVSleptin.xlsx"))
setnames(tbl1c_supp, c("fat", mylvls))
tbl1c_supp <- as.data.table(sapply(tbl1c_supp, as.numeric))
tbl1c_supp <- melt.data.table(tbl1c_supp, id.vars = "fat")
tbl1c_supp[!is.na(value), ]
setnames(tbl1c_supp, c("fat", "type", "value"))





####################################################################
# PLOTS AND ARRANGE AND SAVE
####################################################################

max_val <- max(tbl2b_melt$value)
min_val <- min(tbl2b_melt$value)

plt_bodyTemp <- 
ggplot(tbl2b_melt, aes(x = variable, y = value, color = variable, fill = variable)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(width = 0.1) +
	stat_compare_means(comparisons = compare2b, size = 3, label.y = max_val*1.005) +
	ylab("Mean body surf. temp. [°C]") +
	theme_pubr() +
	ylim(min_val, max_val*1.02) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	guides(fill = guide_legend(nrow = 2)) +
	theme(legend.position = "none", legend.title = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank())




# b. leptin

compare <- list(c(mylvls[c(1,2)]), c(mylvls[c(3,4)]))

max_leptin <- max(tbl1b_supp$leptin)

plt_leptin <- 
ggplot(tbl1b_supp, aes(x = type, y = leptin, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(width = 0.1) +
	stat_compare_means(comparisons = compare, size = 3) +
	ylab("Plasma leptin [pg/mL]") +
	theme_pubr() +
	ylim(0, max_leptin*1.3) +
 	guides(fill = guide_legend(nrow = 2)) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank())



# c. leptin / fat mass

plot_leptin_fat <- 
ggplot(tbl1c_supp, aes(x = fat, y = value, color = type)) +
	geom_point() +
	geom_smooth(method = "lm", se = FALSE) +
 	# stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, size = 3, aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")))+
	ylab("Plasma leptin [pg/mL]") +
	xlab("Fat mass [g]") +
	theme_pubr() +
 	guides(color = guide_legend(nrow = 2)) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.position = "none", legend.title = element_blank())




plt2 <-
ggarrange(plt_bodyTemp, plt_leptin, plot_leptin_fat, align = "v", ncol = 1, labels = c("c", "d", "e"))

ggsave(paste(fig_path, "Fig2.png", sep = ""), plt2, device = png(), width = 2.5, height = 8, bg = "white")
dev.off()

ggsave(paste(fig_path, "Fig2.pdf", sep = ""), plt2, device = "pdf", width = 2.5, height = 8, bg = "white")
dev.off()
























