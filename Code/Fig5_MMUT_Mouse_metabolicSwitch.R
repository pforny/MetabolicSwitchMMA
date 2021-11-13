### METABOLIC SWITCH IN A MOUSE MODEL OF METHYLMALONIC ACIDURIA
# FIGURE 5

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
# FIGURE 5
####################################################################
####################################################################

# theme & info:
# metabolic inflexibility
# a. glucose
# b. triglycerides
# c. cholesterol
# d. glyercol
# e. NEFA
# f. lactate
# e. FGF21

####################################################################
# DATA IMPORT AND TIDY
####################################################################



# glucose

tbl5a <- data.table(read_excel("Data/Fig5/PF_Fig5A_Glucose.xlsx"))
setnames(tbl5a, c(mylvls, "eat"))
tbl5a <- melt.data.table(tbl5a, id.vars = "eat")
setnames(tbl5a, c("eat", "type", "value"))
tbl5a$value <- as.numeric(tbl5a$value)
tbl5a <- tbl5a[!is.na(tbl5a$value), ]
tbl5a$type <- factor(tbl5a$type, levels = mylvls)
tbl5a[eat == "fasting", eat := "fasted"]
tbl5a$eat <- factor(tbl5a$eat, levels = c("fed", "fasted"))



# triglycerides

tbl5b <- data.table(read_excel("Data/Fig5/PF_Fig5B_Triglycerides.xlsx"))
setnames(tbl5b, c(mylvls, "eat"))
tbl5b <- melt.data.table(tbl5b, id.vars = "eat")
setnames(tbl5b, c("eat", "type", "value"))
tbl5b$value <- as.numeric(tbl5b$value)
tbl5b <- tbl5b[!is.na(tbl5b$value), ]
tbl5b$type <- factor(tbl5b$type, levels = mylvls)
tbl5b[eat == "fasting", eat := "fasted"]
tbl5b$eat <- factor(tbl5b$eat, levels = c("fed", "fasted"))



# cholesterol

tbl5c <- data.table(read_excel("Data/Fig5/PF_Fig5C_Cholesterol.xlsx"))
setnames(tbl5c, c(mylvls, "eat"))
tbl5c <- melt.data.table(tbl5c, id.vars = "eat")
setnames(tbl5c, c("eat", "type", "value"))
tbl5c$value <- as.numeric(tbl5c$value)
tbl5c <- tbl5c[!is.na(tbl5c$value), ]
tbl5c$type <- factor(tbl5c$type, levels = mylvls)
tbl5c[eat == "fasting", eat := "fasted"]
tbl5c$eat <- factor(tbl5c$eat, levels = c("fed", "fasted"))



# glycerol

tbl5d2 <- data.table(read_excel("Data/Fig5/Fasted_CliChe_old_Sec-Screen.xlsx"))
tbl5d2 <- tbl5d2[-1, ]
colnames(tbl5d2)
tbl5d2[, c("Glycerol_fasting", "type")]
tbl5d2$type <- factor(tbl5d2$type, levels = mylvls)
tbl5d2$Glycerol_fasting <- as.numeric(tbl5d2$Glycerol_fasting)
tbl5d2[, overall := "fasted"]




# NEFA

tbl5e2 <- data.table(read_excel("Data/Fig5/Fasted_CliChe_old_Sec-Screen.xlsx"))
tbl5e2 <- tbl5e2[-1, ]
colnames(tbl5e2)
tbl5e2[, c("NEFA_fasting", "type")]
tbl5e2$type <- factor(tbl5e2$type, levels = mylvls)
tbl5e2$NEFA_fasting <- as.numeric(tbl5e2$NEFA_fasting)
tbl5e2[, overall := "fasted"]





# lactate

tbl5d <- data.table(read_excel("Data/Fig5/PF_Fig5D_Lactate.xlsx"))
setnames(tbl5d, c(mylvls, "eat"))
tbl5d <- melt.data.table(tbl5d, id.vars = "eat")
setnames(tbl5d, c("eat", "type", "value"))
tbl5d$value <- as.numeric(tbl5d$value)
tbl5d <- tbl5d[!is.na(tbl5d$value), ]
tbl5d$type <- factor(tbl5d$type, levels = mylvls)
tbl5d[eat == "fasting", eat := "fasted"]
tbl5d$eat <- factor(tbl5d$eat, levels = c("fed", "fasted"))



# FGF21

tbl5e <- data.table(read_excel("Data/Fig5/PF_Fig5A_fgf21.xlsx"))
setnames(tbl5e, c(mylvls, "eat"))
tbl5e <- melt.data.table(tbl5e, id.vars = "eat")
setnames(tbl5e, c("eat", "type", "value"))
tbl5e$value <- as.numeric(tbl5e$value)
tbl5e <- tbl5e[!is.na(tbl5e$value), ]
tbl5e$type <- factor(tbl5e$type, levels = mylvls)
tbl5e[eat == "fasting", eat := "fasted"]
tbl5e$eat <- factor(tbl5e$eat, levels = c("fed", "fasted"))
tbl5e[, type_mod := ifelse(type == mylvls[1] | type == mylvls[3], mylvls2[1], NA)]
tbl5e[is.na(type_mod), type_mod := mylvls2[2]]
tbl5e[type_mod == mylvls2[1], type3 := "control"]
tbl5e[type_mod == mylvls2[2], type3 := "mutant"]






####################################################################
# PLOTS AND ARRANGE AND SAVE
####################################################################


margs = c(0.5, 0.5, 0.5, 0.5)
margin = theme(plot.margin = unit(margs, "cm"))



compare <- list(c(mylvls[1], mylvls[2]), c(mylvls[3], mylvls[4]))



# a. glucose

plt5a <- 
ggplot(tbl5a, aes(x = type, y = value)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA, aes(color = type, fill = type)) +
	geom_jitter(width = 0.1, aes(color = type, fill = type)) +
	facet_grid(~eat) +
	stat_compare_means(comparisons = compare, size = 3) +
	ylab("Glucose [mmol/L]") +
	ylim(0,26) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white")) +
	margin



# b. triglycerides

plt5b <- 
ggplot(tbl5b, aes(x = type, y = value)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA, aes(color = type, fill = type)) +
	geom_jitter(width = 0.1, aes(color = type, fill = type)) +
	facet_grid(~eat) +
	stat_compare_means(comparisons = compare, size = 3) +
	ylab("Triglycerides [mmol/L]") +
	ylim(0, 4) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white")) +
	margin



# c. cholesterol

plt5c <- 
ggplot(tbl5c, aes(x = type, y = value)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA, aes(color = type, fill = type)) +
	geom_jitter(width = 0.1, aes(color = type, fill = type)) +
	facet_grid(~eat) +
	stat_compare_means(comparisons = compare, size = 3) +
	ylab("Cholesterol [mmol/L]") +
	ylim(0, 2.9) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white")) +
	margin



# d. glycerol

plt5d <- 
ggplot(tbl5d2, aes(x = type, y = Glycerol_fasting)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA, aes(color = type, fill = type)) +
	geom_jitter(width = 0.1, aes(color = type, fill = type)) +
	facet_wrap(~overall) +
	stat_compare_means(comparisons = compare, size = 3) +
	ylab("Glycerol [mmol/L]") +
	ylim(0, 0.6) +
	theme_pubr() +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white")) +
	margin





# e. NEFA

plt5e <- 
ggplot(tbl5e2, aes(x = type, y = NEFA_fasting)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA, aes(color = type, fill = type)) +
	geom_jitter(width = 0.1, aes(color = type, fill = type)) +
	facet_wrap(~overall) +
	stat_compare_means(comparisons = compare, size = 3) +
	ylab("NEFA [mmol/L]") +
	ylim(0, 2.3) +
	theme_pubr() +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white")) +
	margin





# f. lactate

plt5f <- 
ggplot(tbl5d, aes(x = type, y = value)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA, aes(color = type, fill = type)) +
	geom_jitter(width = 0.1, aes(color = type, fill = type)) +
	facet_grid(~eat) +
	stat_compare_means(comparisons = compare, size = 3) +
	ylab("Lactate [mmol/L]") +
	ylim(0, 18) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white")) +
	margin



# g. FGF21

max_val <- max(tbl5e$value)
min_val <- min(tbl5e$value)

compare_5e <- list(c("control", "mutant"))

plt5g <- 
ggplot(tbl5e, aes(x = type3, y = value)) +
	geom_boxplot(alpha = 0.1, outlier.shape = NA, aes(x = type3), color = "black", fill = "black") +
	geom_jitter(aes(color = type), width = 0.1) +
	facet_wrap(~eat) +
	stat_compare_means(comparisons = compare_5e, size = 3) +
	ylab("Fgf21 [pg/mL]") +
	scale_y_log10(limits = c(0.9*min_val, 2*max_val)) +
	annotation_logticks(sides = "l", short = unit(0.5,"mm"), mid = unit(0.5,"mm"), long = unit(1,"mm")) +
	scale_color_manual(values = mypal) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), strip.background = element_rect(fill = "white")) +
	margin





plt5 <- 
(plt5a + plt5b + plt5c) / (plt5d + plt5e + plt5f + plt5g + plot_layout(widths = c(1, 1, 2, 1.5))) + plot_annotation(tag_levels = 'a') & theme(plot.tag = element_text(face = 'bold'))


ggsave(paste(fig_path, "Fig5.png", sep = ""), plt5, device = png(), width = 11, height = 7, bg = "white")
dev.off()


ggsave(paste(fig_path, "Fig5.pdf", sep = ""), plt5, device = "pdf", width = 11, height = 7, bg = "white")
dev.off()




















