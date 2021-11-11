# wd
Dropbox/PF/Post_PhD/Zurich_2019_2022/MMUT_Mouse_metabolicSwitch
setwd("/Volumes/PFT7/Dropbox/PF/Post_PhD/Zurich_2019_2022/MMUT_Mouse_metabolicSwitch")


# libraries
require(data.table)
require(ggplot2)
require(tidyverse)
require(readxl)
require(ggpubr)
library(multcomp)



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

system("mkdir Figs")
system("mkdir Figs/tablesv3")
fig_tbl_path <- c("Figs/tablesv3/")



####################################################################
####################################################################
# FIGURE 1
####################################################################
####################################################################

# theme & info:
# body composition
# a. body weight
# b. leanMass vs body weight
# c. fatMass vs body weight
# d. leanMass/body weight
# e. fatMass/body weight
# f. obesitas index (fatMass/leanMass)

####################################################################
# DATA IMPORT AND TIDY
####################################################################



# overall file

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



#######
# correct all body weight dependent variables (lean, fat) - genders analysed separately

# average body weight
tbl1_combo_f$avBW <- mean(tbl1_combo$bw, na.rm = TRUE)
tbl1_combo_m$avBW <- mean(tbl1_combo$bw, na.rm = TRUE)


# linear model for lean mass
mod_lean_f <- lm(lean ~ bw + genotype, data = tbl1_combo_f)
mod_lean_m <- lm(lean ~ bw + genotype, data = tbl1_combo_m)
summary(mod_lean_f)
summary(mod_lean_m)

# add residuals to data.table
tbl1_combo_f$resid_lean <- resid(mod_lean_f)
tbl1_combo_m$resid_lean <- resid(mod_lean_m)

# calculate ajdusted lean mass
tbl1_combo_f <- tbl1_combo_f %>%
  mutate(adj_lean = case_when(
    genotype=="ki_wt"~ coef(mod_lean_f)[1] + coef(mod_lean_f)[2]*avBW + resid_lean,
    genotype=="ko_ki"~ (coef(mod_lean_f)[1]+ coef(mod_lean_f)[3]) + coef(mod_lean_f)[2]*avBW + resid_lean))

tbl1_combo_m <- tbl1_combo_m %>%
  mutate(adj_lean = case_when(
    genotype=="ki_wt"~ coef(mod_lean_m)[1] + coef(mod_lean_m)[2]*avBW + resid_lean,
    genotype=="ko_ki"~ (coef(mod_lean_m)[1]+ coef(mod_lean_m)[3]) + coef(mod_lean_m)[2]*avBW + resid_lean))



# # linear model for lean mass (no separation by gender)
# tbl1_combo$avBW <- mean(tbl1_combo$bw, na.rm = TRUE)

# mod_lean <- lm(lean ~ bw + genotype + gender, data = tbl1_combo)
# summary(mod_lean)

# # add residuals to data.table
# tbl1_combo$resid_lean <- resid(mod_lean)

# # calculate ajdusted lean mass
# coef(mod_lean)
# tbl1_combo <- tbl1_combo %>%
#   mutate(adj_lean = case_when(
#     genotype=="ki_wt"~ coef(mod_lean)[1] + coef(mod_lean)[2]*avBW + coef(mod_lean)[4] + resid_lean,
#     genotype=="ko_ki"~ (coef(mod_lean)[1]+ coef(mod_lean)[3]) + coef(mod_lean)[2]*avBW + coef(mod_lean)[4] + resid_lean))



# ggplot(tbl1_combo, aes(x = type, y = adj_lean, color = type, fill = type)) +
# 	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
# 	geom_jitter(width = 0.1, aes(shape = cohort)) +
# 	stat_compare_means(comparisons = compare, method = "t.test") +
# 	ylab("Adjusted lean mass [g]") +
# 	theme_pubr() +
# 	rotate_x_text(angle = 45) +
# 	scale_fill_manual(values = mypal) +
# 	scale_color_manual(values = mypal) +
# 	theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank()) +
# 	margin

# ggsave(paste(fig_path, "leanMass_mixed genders.png", sep = ""), device = png(), width = 3, height = 3, bg = "white")
# dev.off()



# linear model for fat mass
mod_fat_f <- lm(fat ~ bw + genotype, data = tbl1_combo_f)
mod_fat_m <- lm(fat ~ bw + genotype, data = tbl1_combo_m)
summary(mod_fat_f)
summary(mod_fat_m)

# add residuals to data.table
tbl1_combo_f$resid_fat <- resid(mod_fat_f)
tbl1_combo_m$resid_fat <- resid(mod_fat_m)

# calculate ajdusted lean mass
tbl1_combo_f <- tbl1_combo_f %>%
  mutate(adj_fat = case_when(
    genotype=="ki_wt"~ coef(mod_fat_f)[1] + coef(mod_fat_f)[2]*avBW + resid_lean,
    genotype=="ko_ki"~ (coef(mod_fat_f)[1]+ coef(mod_fat_f)[3]) + coef(mod_fat_f)[2]*avBW + resid_lean))

tbl1_combo_m <- tbl1_combo_m %>%
  mutate(adj_fat = case_when(
    genotype=="ki_wt"~ coef(mod_fat_m)[1] + coef(mod_fat_m)[2]*avBW + resid_lean,
    genotype=="ko_ki"~ (coef(mod_fat_m)[1]+ coef(mod_fat_m)[3]) + coef(mod_fat_m)[2]*avBW + resid_lean))


tbl1_combo_adj <- rbind(tbl1_combo_f, tbl1_combo_m)

tbl1_combo_adj[, obesitas_index_adj := adj_fat/adj_lean]




# multiple linear regression model

# tbl_both <- 
# data.table(gender = "both",
# 	p_vals_of = names(coef(summary(lm(fat ~ genotype + bw, tbl1_combo)))[-1, "Pr(>|t|)"]), 
# 	fat = coef(summary(lm(fat ~ genotype + bw, tbl1_combo)))[-1, "Pr(>|t|)"],
# 	lean = coef(summary(lm(lean ~ genotype + bw, tbl1_combo)))[-1, "Pr(>|t|)"],
# 	bw = coef(summary(lm(bw ~ genotype + bw, tbl1_combo)))[-1, "Pr(>|t|)"],
# 	obesitasIndex = coef(summary(lm(obesitas_index ~ genotype + bw, tbl1_combo)))[-1, "Pr(>|t|)"])

# tbl_female <- 
# data.table(gender = "female", 
# 	p_vals_of = names(coef(summary(lm(fat ~ genotype + bw, tbl1_combo[gender == "female", ])))[-1, "Pr(>|t|)"]), 
# 	fat = coef(summary(lm(fat ~ genotype + bw, tbl1_combo[gender == "female", ])))[-1, "Pr(>|t|)"],
# 	lean = coef(summary(lm(lean ~ genotype + bw, tbl1_combo[gender == "female", ])))[-1, "Pr(>|t|)"],
# 	bw = coef(summary(lm(bw ~ genotype + bw, tbl1_combo[gender == "female", ])))[-1, "Pr(>|t|)"],
# 	obesitasIndex = coef(summary(lm(obesitas_index ~ genotype + bw, tbl1_combo[gender == "female", ])))[-1, "Pr(>|t|)"])

# tbl_male <- 
# data.table(gender = "male", 
# 	p_vals_of = names(coef(summary(lm(fat ~ genotype + bw, tbl1_combo[gender == "male", ])))[-1, "Pr(>|t|)"]), 
# 	fat = coef(summary(lm(fat ~ genotype + bw, tbl1_combo[gender == "male", ])))[-1, "Pr(>|t|)"],
# 	lean = coef(summary(lm(lean ~ genotype + bw, tbl1_combo[gender == "male", ])))[-1, "Pr(>|t|)"],
# 	bw = coef(summary(lm(bw ~ genotype + bw, tbl1_combo[gender == "male", ])))[-1, "Pr(>|t|)"],
# 	obesitasIndex = coef(summary(lm(obesitas_index ~ genotype + bw, tbl1_combo[gender == "male", ])))[-1, "Pr(>|t|)"])

# linr_tbl <- rbind(tbl_both, tbl_female, tbl_male)

# write.csv(linr_tbl, paste(fig_tbl_path, "Fig1_linearModelling.csv", sep=""))





# a. body weight

tbl1a <- data.table(read_excel("Data/Fig1/PF_Fig1A_bw.xlsx"))
setnames(tbl1a, rep(c("bw", "cohort", "type"), 4))
tbl1a <- rbind(tbl1a[,1:3], tbl1a[,4:6], tbl1a[,7:9], tbl1a[,10:12])
tbl1a$bw <- as.numeric(tbl1a$bw)
tbl1a <- tbl1a[!is.na(tbl1a$bw), ]
tbl1a$type <- factor(tbl1a$type, levels = mylvls)



# b. leanMass vs body weight

tbl1b <- data.table(read_excel("Data/Fig1/PF_Fig1B_lean_bw_regression.xlsx"))
tbl1b$bw <- as.numeric(tbl1b$bw)
tbl1b <- tbl1b[!is.na(tbl1b$bw), ]
tbl1b$lean <- as.numeric(tbl1b$lean)
tbl1b <- tbl1b[!is.na(tbl1b$lean), ]
tbl1b$type <- factor(tbl1b$type, levels = mylvls)
tbl1b_f <- tbl1b[type == mylvls[1] | type == mylvls[2], ]
tbl1b_m <- tbl1b[type == mylvls[3] | type == mylvls[4], ]

# model.b <- glm(lean ~ bw + type + bw:type, tbl1b_m, family = gaussian (link = "identity"))
# summary(model.b)
# glht(model.b, mcp(type = "Tukey"))
# anova(model.b)





# c. fatMass vs body weight

tbl1c <- data.table(read_excel("Data/Fig1/PF_Fig1C_fat_bw_regression.xlsx"))
tbl1c$bw <- as.numeric(tbl1c$bw)
tbl1c <- tbl1c[!is.na(tbl1c$bw), ]
tbl1c$fat <- as.numeric(tbl1c$fat)
tbl1c <- tbl1c[!is.na(tbl1c$fat), ]
tbl1c$type <- factor(tbl1c$type, levels = mylvls)



# # d. leanMass/body weight

# tbl1d <- data.table(read_excel("Data/Fig1/PF_Fig1D_lean_bw_ratio.xlsx"))
# setnames(tbl1d, rep(c("lean_bw_ratio", "cohort", "type"), 4))
# tbl1d <- rbind(tbl1d[,1:3], tbl1d[,4:6], tbl1d[,7:9], tbl1d[,10:12])
# tbl1d$lean_bw_ratio <- as.numeric(tbl1d$lean_bw_ratio)
# tbl1d <- tbl1d[!is.na(tbl1d$lean_bw_ratio), ]
# tbl1d$type <- factor(tbl1d$type, levels = mylvls)



# # e. fatMass/body weight

# tbl1e <- data.table(read_excel("Data/Fig1/PF_Fig1E_fat_bw_ratio.xlsx"))
# setnames(tbl1e, rep(c("fat_bw_ratio", "cohort", "type"), 4))
# tbl1e <- rbind(tbl1e[,1:3], tbl1e[,4:6], tbl1e[,7:9], tbl1e[,10:12])
# tbl1e$fat_bw_ratio <- as.numeric(tbl1e$fat_bw_ratio)
# tbl1e <- tbl1e[!is.na(tbl1e$fat_bw_ratio), ]
# tbl1e$type <- factor(tbl1e$type, levels = mylvls)



# # f. obesitas index (fatMass/leanMass)

# tbl1f <- data.table(read_excel("Data/Fig1/PF_Fig1F_obesitas_index.xlsx"))
# setnames(tbl1f, rep(c("obesitas_index", "cohort", "type"), 4))
# tbl1f <- rbind(tbl1f[,1:3], tbl1f[,4:6], tbl1f[,7:9], tbl1f[,10:12])
# tbl1f$obesitas_index <- as.numeric(tbl1f$obesitas_index)
# tbl1f <- tbl1f[!is.na(tbl1f$obesitas_index), ]
# tbl1f$type <- factor(tbl1f$type, levels = mylvls)




# further variables for plotting

# margs = c(0.5, 0.5, 0.5, 0.5)
margs = c(0.3, 0.3, 0.3, 0.3)
margin = theme(plot.margin = unit(margs, "cm"))



####################################################################
# PLOTS AND ARRANGE AND SAVE
####################################################################


compare <- list(mylvls[c(1,2)], mylvls[c(3,4)])


# a. body weight

max_val <- max(tbl1a$bw)
min_val <- min(tbl1a$bw)

plt1A <- 
ggplot(tbl1a, aes(x = type, y = bw, color = type, fill = type)) +
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
ggplot(tbl1b, aes(x = bw, y = lean, color = type)) +
	geom_point(aes(shape = cohort)) +
	geom_smooth(method = "lm", se = TRUE) +
 	# stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, size = 3, aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")))+
	ylab("Lean mass [g]") +
	xlab("Body mass [g]") +
	theme_pubr() +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.position = "none") +
	margin







# c. fatMass vs body weight

plt1C <- 
ggplot(tbl1c, aes(x = bw, y = fat, color = type)) +
	geom_point(aes(shape = cohort)) +
	geom_smooth(method = "lm", se = TRUE) +
 	# stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, size = 3, aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")))+
	ylab("Fat mass [g]") +
	xlab("Body mass [g]") +
	theme_pubr() +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.position = "none") +
	margin



# d. leanMass/body weight

# max_val <- max(tbl1d$lean_bw_ratio)
# min_val <- min(tbl1d$lean_bw_ratio)

# plt1D <- 
# ggplot(tbl1d, aes(x = type, y = lean_bw_ratio, color = type, fill = type)) +
# 	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
# 	geom_jitter(width = 0.1, aes(shape = cohort)) +
# 	stat_compare_means(comparisons = compare) +
# 	ylab("Lean mass/body mass ratio") +
# 	ylim(0.9*min_val,1.1*max_val) +
# 	theme_pubr() +
# 	rotate_x_text(angle = 45) +
# 	scale_fill_manual(values = mypal) +
# 	scale_color_manual(values = mypal) +
# 	theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank()) +
# 	margin

max_val <- max(tbl1_combo_adj$adj_lean)
min_val <- min(tbl1_combo_adj$adj_lean)

plt1D <- 
ggplot(tbl1_combo_adj, aes(x = type, y = adj_lean, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(width = 0.1, aes(shape = cohort)) +
	stat_compare_means(comparisons = compare, method = "t.test", size = 3) +
	ylab("Adjusted lean mass [g]") +
	ylim(0.9*min_val,1.1*max_val) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank()) +
	margin




# e. fatMass/body weight

# max_val <- max(tbl1e$fat_bw_ratio)
# min_val <- min(tbl1e$fat_bw_ratio)

# plt1E <- 
# ggplot(tbl1e, aes(x = type, y = fat_bw_ratio, color = type, fill = type)) +
# 	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
# 	geom_jitter(width = 0.1, aes(shape = cohort)) +
# 	stat_compare_means(comparisons = compare) +
# 	ylab("Fat mass/body mass ratio") +
# 	ylim(0.9*min_val,1.1*max_val) +
# 	theme_pubr() +
# 	rotate_x_text(angle = 45) +
# 	scale_fill_manual(values = mypal) +
# 	scale_color_manual(values = mypal) +
# 	theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank()) +
# 	margin

max_val <- max(tbl1_combo_adj$adj_fat)
min_val <- min(tbl1_combo_adj$adj_fat)

plt1E <- 
ggplot(tbl1_combo_adj, aes(x = type, y = adj_fat, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(width = 0.1, aes(shape = cohort)) +
	stat_compare_means(comparisons = compare, method = "t.test", size = 3) +
	ylab("Adjusted fat mass [g]") +
	ylim(0.9*min_val,1.2*max_val) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank()) +
	margin




# f. obesitas index (fatMass/leanMass)

# max_val <- max(tbl1_combo_adj$obesitas_index)
# min_val <- min(tbl1_combo_adj$obesitas_index)

# plt1F <- 
# ggplot(tbl1_combo_adj, aes(x = type, y = obesitas_index, color = type, fill = type)) +
# 	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
# 	geom_jitter(width = 0.1, aes(shape = cohort)) +
# 	stat_compare_means(comparisons = compare, size = 3) +
# 	ylab("Adiposity index") +
# 	ylim(0.9*min_val,1.15*max_val) +
# 	theme_pubr() +
# 	rotate_x_text(angle = 45) +
# 	scale_fill_manual(values = mypal) +
# 	scale_color_manual(values = mypal) +
# 	theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank()) +
# 	margin


plt1_bottom <-
ggarrange(plt1B, plt1C, labels = c("b", "c"), align = "hv", common.legend = TRUE, legend = "none")

plt1_top <-
ggarrange(plt1A, plt1D, plt1E, labels = c("a", "d", "e"), align = "hv", common.legend = TRUE, legend = "top", nrow = 1)

plt1 <-
ggarrange(plt1_top, plt1_bottom, nrow = 2)

# plt1 <-
# ggarrange(plt1A, plt1B, plt1C, plt1D, plt1E, labels = "auto", font.label = list(size = 18), align = "hv", common.legend = TRUE, legend = "bottom")

ggsave(paste(fig_path, "Fig1.png", sep = ""), plt1, device = png(), width = 7, height = 6, bg = "white")
dev.off()


ggsave(paste(fig_path, "Fig1.pdf", sep = ""), plt1, device = "pdf", width = 7, height = 6, bg = "white")
dev.off()


















