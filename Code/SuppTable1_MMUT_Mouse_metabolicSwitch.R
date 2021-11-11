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
# Supp. Table 1
####################################################################
####################################################################


####################################################################
# DATA IMPORT AND TIDY AND GENERATE TABLE 1
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



# table for females
mean_tbl_f <- tbl1_combo_f %>%
	group_by(type) %>%
	summarise(body_mass = paste(round(mean(bw, na.rm = TRUE),1), "±", round(sd(bw, na.rm = TRUE),1), sep = " "), fat_mass = paste(round(mean(fat, na.rm = TRUE),1), "±", round(sd(fat, na.rm = TRUE),1), sep = " "), lean_mass = paste(round(mean(lean, na.rm = TRUE),1), "±", round(sd(lean, na.rm = TRUE),1), sep = " "))

mean_tbl_f <- t(mean_tbl_f)
colnames(mean_tbl_f) <- mean_tbl_f[1, ]
mean_tbl_f <- mean_tbl_f[-1, ]


bw_f_ps <- c("NA", coef(summary(lm(bw ~ genotype, data = tbl1_combo_f)))[2,4])
fat_f_ps <- coef(summary(lm(fat ~ bw + genotype, data = tbl1_combo_f)))[2:3,4]
lean_f_ps <- coef(summary(lm(lean ~ bw + genotype, data = tbl1_combo_f)))[2:3,4]
f_ps <- rbind(bw_f_ps, fat_f_ps, lean_f_ps)


f_tbl <- cbind(mean_tbl_f, f_ps)


write.csv(f_tbl, paste(fig_tbl_path, "SuppTbl1_f.csv", sep=""))





# table for males
mean_tbl_m <- tbl1_combo_m %>%
	group_by(type) %>%
	summarise(body_mass = paste(round(mean(bw, na.rm = TRUE),1), "±", round(sd(bw, na.rm = TRUE),1), sep = " "), fat_mass = paste(round(mean(fat, na.rm = TRUE),1), "±", round(sd(fat, na.rm = TRUE),1), sep = " "), lean_mass = paste(round(mean(lean, na.rm = TRUE),1), "±", round(sd(lean, na.rm = TRUE),1), sep = " "))

mean_tbl_m <- t(mean_tbl_m)
colnames(mean_tbl_m) <- mean_tbl_m[1, ]
mean_tbl_m <- mean_tbl_m[-1, ]


bw_m_ps <- c("NA", coef(summary(lm(bw ~ genotype, data = tbl1_combo_m)))[2,4])
fat_m_ps <- coef(summary(lm(fat ~ bw + genotype, data = tbl1_combo_m)))[2:3,4]
lean_m_ps <- coef(summary(lm(lean ~ bw + genotype, data = tbl1_combo_m)))[2:3,4]
m_ps <- rbind(bw_m_ps, fat_m_ps, lean_m_ps)


m_tbl <- cbind(mean_tbl_m, m_ps)


write.csv(m_tbl, paste(fig_tbl_path, "SuppTbl1_m.csv", sep=""))











