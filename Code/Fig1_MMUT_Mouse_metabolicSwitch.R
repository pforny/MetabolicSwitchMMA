### METABOLIC SWITCH IN A MOUSE MODEL OF METHYLMALONIC ACIDURIA
# FIGURE 1

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
require(multcomp)



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
# FIGURE 1
####################################################################
####################################################################

# theme & info:
# body composition
# a. body mass
# b. leanMass vs body mass
# c. fatMass vs body mass

####################################################################
# DATA IMPORT AND TIDY
####################################################################



# import file

tbl1_combo <- data.table(read_excel("Data/Fig1/PF_Fig1_combined data.xlsx"))
tbl1_combo[, gender := "male"]
tbl1_combo[type == mylvls[1] | type == mylvls[2], gender := "female"]
tbl1_combo$gender <- factor(tbl1_combo$gender)
tbl1_combo[, genotype := "ki_wt"]
tbl1_combo[type == mylvls[2] | type == mylvls[4], genotype := "ko_ki"]
tbl1_combo$genotype <- factor(tbl1_combo$genotype)

tbl1_combo <- tbl1_combo[!is.na(bw), ]

tbl1_combo_f <- tbl1_combo[!is.na(bw) & gender == "female", ]
tbl1_combo_m <- tbl1_combo[!is.na(bw) & gender == "male", ]




####################################################################
# TABLE
####################################################################



# table for females
mean_tbl_f <- tbl1_combo_f %>%
	group_by(type) %>%
	summarise(body_mass = paste(round(mean(bw, na.rm = TRUE),1), "±", round(sd(bw, na.rm = TRUE),1), sep = " "), fat_mass = paste(round(mean(fat, na.rm = TRUE),1), "±", round(sd(fat, na.rm = TRUE),1), sep = " "), lean_mass = paste(round(mean(lean, na.rm = TRUE),1), "±", round(sd(lean, na.rm = TRUE),1), sep = " "))

mean_tbl_f <- t(mean_tbl_f)
colnames(mean_tbl_f) <- mean_tbl_f[1, ]
mean_tbl_f <- mean_tbl_f[-1, ]


bw_f_ps <- c("NA", format.pval(coef(summary(lm(bw ~ genotype, data = tbl1_combo_f)))[2,4], digits = 2))
fat_f_ps <- format.pval(coef(summary(lm(fat ~ bw + genotype, data = tbl1_combo_f)))[2:3,4], digits = 2)
lean_f_ps <- format.pval(coef(summary(lm(lean ~ bw + genotype, data = tbl1_combo_f)))[2:3,4], digits = 2)
f_ps <- rbind(bw_f_ps, fat_f_ps, lean_f_ps)
colnames(f_ps) <- names(coef(summary(lm(lean ~ bw + genotype, data = tbl1_combo_f)))[2:3,4])

f_tbl <- cbind(mean_tbl_f, f_ps)


write.csv(f_tbl, paste(fig_tbl_path, "SuppTbl1_f.csv", sep=""))



# table for males
mean_tbl_m <- tbl1_combo_m %>%
	group_by(type) %>%
	summarise(body_mass = paste(round(mean(bw, na.rm = TRUE),1), "±", round(sd(bw, na.rm = TRUE),1), sep = " "), fat_mass = paste(round(mean(fat, na.rm = TRUE),1), "±", round(sd(fat, na.rm = TRUE),1), sep = " "), lean_mass = paste(round(mean(lean, na.rm = TRUE),1), "±", round(sd(lean, na.rm = TRUE),1), sep = " "))

mean_tbl_m <- t(mean_tbl_m)
colnames(mean_tbl_m) <- mean_tbl_m[1, ]
mean_tbl_m <- mean_tbl_m[-1, ]


bw_m_ps <- c("NA", format.pval(coef(summary(lm(bw ~ genotype, data = tbl1_combo_m)))[2,4], digits = 2))
fat_m_ps <- format.pval(coef(summary(lm(fat ~ bw + genotype, data = tbl1_combo_m)))[2:3,4], digits = 2)
lean_m_ps <- format.pval(coef(summary(lm(lean ~ bw + genotype, data = tbl1_combo_m)))[2:3,4], digits = 2)
m_ps <- rbind(bw_m_ps, fat_m_ps, lean_m_ps)
colnames(m_ps) <- names(coef(summary(lm(lean ~ bw + genotype, data = tbl1_combo_m)))[2:3,4])

m_tbl <- cbind(mean_tbl_m, m_ps)


write.csv(m_tbl, paste(fig_tbl_path, "SuppTbl1_m.csv", sep=""))






####################################################################
# PLOTS AND ARRANGE AND SAVE
####################################################################

# further variables for plotting

# margs = c(0.5, 0.5, 0.5, 0.5)
margs = c(0.3, 0.3, 0.3, 0.3)
margin = theme(plot.margin = unit(margs, "cm"))



compare <- list(mylvls[c(1,2)], mylvls[c(3,4)])


# a. body weight

max_val <- max(tbl1_combo$bw)
min_val <- min(tbl1_combo$bw)

plt1A <- 
ggplot(tbl1_combo, aes(x = type, y = bw, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(width = 0.1, aes(shape = cohort)) +
	stat_compare_means(comparisons = compare, size = 3) +
	ylab("Body mass [g]") +
	ylim(0.9*min_val,1.1*max_val) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank()) +
	margin



# b. leanMass vs body weight

plt1B <- 
ggplot(tbl1_combo, aes(x = bw, y = lean, color = type)) +
	geom_point(aes(shape = cohort)) +
	geom_smooth(method = "lm", se = TRUE) +
 	stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, size = 3, aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")), cor.coef.name = "rho")+
	ylab("Lean mass [g]") +
	xlab("Body mass [g]") +
	theme_pubr() +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.position = "none") +
	margin



# c. fatMass vs body weight

plt1C <- 


ggplot(tbl1_combo, aes(x = bw, y = fat, color = type)) +
	geom_point(aes(shape = cohort)) +
	geom_smooth(method = "lm", se = TRUE) +
 	stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, size = 3, aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")), cor.coef.name = "rho")+
	ylab("Fat mass [g]") +
	xlab("Body mass [g]") +
	theme_pubr() +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.position = "none") +
	margin




# d. fatMass body weight ratio

max_val <- max(tbl1_combo$fat_bw_ratio)
min_val <- min(tbl1_combo$fat_bw_ratio)

plt1D <- 
ggplot(tbl1_combo, aes(x = type, y = fat_bw_ratio, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(width = 0.1, aes(shape = cohort)) +
	stat_compare_means(comparisons = compare, size = 3) +
	ylab("Fat/body mass") +
	ylim(0.9*min_val,1.1*max_val) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank()) +
	margin



plt1 <-
ggarrange(plt1A, plt1B, plt1C, plt1D, labels = c("a", "b", "c", "d"), align = "hv", common.legend = TRUE, legend = "right")

ggsave(paste(fig_path, "Fig1.png", sep = ""), plt1, device = png(), width = 8, height = 6, bg = "white")
dev.off()

ggsave(paste(fig_path, "Fig1.pdf", sep = ""), plt1, device = "pdf", width = 8, height = 6, bg = "white")
dev.off()












