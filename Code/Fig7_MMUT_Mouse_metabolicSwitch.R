### METABOLIC SWITCH IN A MOUSE MODEL OF METHYLMALONIC ACIDURIA
# FIGURE 7

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
require(patchwork)


# color pallette (manual): order:
# female-KI/WT; female-KO/KI
# male_KI/WT; male-KO/KI
mypal = c("#ffa64c", "#b25a00", "#66b2b2", "#004c4c")
mylvls = c("f_Mmut-ki/wt", "f_Mmut-ko/ki", "m_Mmut-ki/wt", "m_Mmut-ko/ki")

# create figures path
system("mkdir Figs")
system("mkdir Figs/v19")
fig_path <- c("Figs/v19/")

system("mkdir Figs")
system("mkdir Figs/tablesv19")
fig_tbl_path <- c("Figs/tablesv19/")




####################################################################
####################################################################
# FIGURE 7
####################################################################
####################################################################

# theme & info:
# liver damage
# a. liver weight
# b. transaminases (ALT, ASAT) and ALP
# c. hallmark metabolites
# d. MMA in liver


####################################################################
# DATA IMPORT AND TIDY
####################################################################



# a. liver weight

tbl7a <- data.table(read_excel("Data/Fig7/PF_Fig7A_liverweightBW.xlsx"))
tbl7a <- melt.data.table(tbl7a)
setnames(tbl7a, c("type", "value"))
tbl7a <- tbl7a[!is.na(value), ]



# b. transaminases (ALT, ASAT) and ALP

tbl7b <- data.table(read_excel("Data/Fig7/PF_Fig7B_liverenzymes.xlsx"))
tbl7b <- melt.data.table(tbl7b)
setnames(tbl7b, c("test", "type", "value"))
tbl7b <- tbl7b[!is.na(value), ]
tbl7b$test <- factor(tbl7b$test, levels = c("ALAT", "ASAT", "ALP"))


# c. hallmark metabolites
tbl7c <- data.table(read_excel("Data/Fig7/PF_Fig7C_normMetabolites.xlsx"))
tbl7c$type <- factor(tbl7c$type, levels = mylvls)
tbl7c$metabolite <- factor(tbl7c$metabolite)
levels(tbl7c$metabolite) <- list("Propionyl-CoA"="norm_propcoa", "Methylmalonyl-CoA"="norm_mmacoa", "Methylmalonate"="norm_mma", "2-Methylcitrate"="norm_mca")



# d. MMA in liver
tbl7d <- data.table(read_excel("Data/Fig7/PF_Fig7D_mmaInLiver.xlsx"))
tbl7d$type <- factor(tbl7d$type, levels = mylvls)











####################################################################
# PLOTS AND ARRANGE AND SAVE
####################################################################


margs = c(0.5, 0.5, 0.5, 0.5)
margin = theme(plot.margin = unit(margs, "cm"))



# a. liver weight

compare <- list(mylvls[c(1,2)], mylvls[c(3,4)])
compare_f <- list(mylvls[c(1,2)])

max_val <- max(tbl7a$value)
min_val <- min(tbl7a$value)

plt7a <- 
ggplot(tbl7a, aes(x = type, y = value, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(width = 0.1) +
	stat_compare_means(comparisons = compare, size =3) +
	ylab("Liver/body weight [mg/g]") +
	ylim(0.9*min_val,1.1*max_val) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank()) +
	margin




# b. transaminases (ALT, ASAT) and ALP

max_val <- max(tbl7b$value)
min_val <- min(tbl7b$value)

plt7b <- 
ggplot(tbl7b, aes(x = type, y = value, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(width = 0.1) +
	stat_compare_means(comparisons = compare, size = 3) +
	ylab("U/L") +
	ylim(0,410) +
	facet_grid(.~test, scales = "free") +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white")) +
	margin



# c. hallmark metabolites

plt7c <- 
ggplot(tbl7c, aes(x = type, y = value)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA, aes(color = type, fill = type)) +
	geom_jitter(width = 0.1, aes(color = type, fill = type)) +
	facet_grid(.~metabolite, scales = "free") +
	stat_compare_means(comparisons = compare_f, method = "t.test", size = 3) +
	ylab("Relative amount in liver tissue") +
	scale_y_log10(limits = c(.1,40)) +
	annotation_logticks(sides = "l", short = unit(0.5,"mm"), mid = unit(0.5,"mm"), long = unit(1,"mm")) +
	theme_pubr() +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white")) +
	margin



# d. MMA in liver

plt7d <- 
ggplot(tbl7d, aes(x = type, y = value, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(width = 0.1) +
	stat_compare_means(comparisons = compare_f, method = "t.test", size = 3) +
	ylab("Methylmalonate [nmol/g liver]") +
	scale_y_log10() +
	annotation_logticks(sides = "l", short = unit(0.5,"mm"), mid = unit(0.5,"mm"), long = unit(1,"mm")) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank()) +
	margin




plt7 <-
(plt7a + plt7b + plot_layout(widths=c(1, 3.3))) / (plt7c + plt7d + plot_layout(widths=c(3.8 ,1))) + plot_annotation(tag_levels = 'a') & theme(plot.tag = element_text(face = 'bold'))



ggsave(paste(fig_path, "Fig7.png", sep = ""), plt7, device = png(), width = 8, height = 6.5, bg = "white")
dev.off()

ggsave(paste(fig_path, "Fig7.pdf", sep = ""), plt7, device = "pdf", width = 8, height = 6.5, bg = "white")
dev.off()
















