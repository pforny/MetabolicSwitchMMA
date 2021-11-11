# wd
Dropbox/PF/Post_PhD/Zurich_2019_2022/MMUT_Mouse_metabolicSwitch
setwd("/Volumes/PFT7/Dropbox/PF/Post_PhD/Zurich_2019_2022/MMUT_Mouse_metabolicSwitch")


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
mylvls = c("f_Mmut-ki/wt", "f_Mmut-ko/ki", "m_Mmut-ki/wt", "m_Mmut-ko/ki")

# create figures path
system("mkdir Figs")
system("mkdir Figs/v3")
fig_path <- c("Figs/v3/")



####################################################################
####################################################################
# FIGURE 6
####################################################################
####################################################################

# theme & info:
# impaired glucose tolerance
# a. fasting glc ipgtt
# b. glc over time
# c. auc ipgtt
# d. fasting glc ipITT
# e. glc over time percent
# f. auc ipITT

####################################################################
# DATA IMPORT AND TIDY
####################################################################



# # a. fasting glc ipgtt

# tbl6a <- data.table(read_excel("Data/Fig6/PF_Fig6a_fastingGlc.xlsx", col_names = FALSE))
# setnames(tbl6a, rep(c("cohort1", "cohort2", "type"), 4))
# tbl6a <- rbind(tbl6a[, 1:3], tbl6a[, 4:6], tbl6a[, 7:9], tbl6a[, 10:12])
# tbl6a <- melt.data.table(tbl6a, id.vars = "type")
# tbl6a <- tbl6a[!is.na(value), ]
# setnames(tbl6a, c("type", "cohort", "value"))



# b. glc over time

tbl6b <- data.table(read_excel("Data/Fig6/PF_Fig6b_glcOverTime.xlsx"))
colnames(tbl6b)

fkiwt6b00 <- tbl6b[, c(2:4)]
fkiwt6b0 <- fkiwt6b00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
fkiwt6b <- cbind(data.table(fkiwt6b0[, c("mean", "sd")]), 
	type = rep(mylvls[1], length(fkiwt6b0$mean)),
	time = tbl6b$time)

fkoki6b00 <- tbl6b[, c(5:7)]
fkoki6b0 <- fkoki6b00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
fkoki6b <- cbind(data.table(fkoki6b0[, c("mean", "sd")]), 
	type = rep(mylvls[2], length(fkoki6b0$mean)),
	time = tbl6b$time)

mkiwt6b00 <- tbl6b[, c(8:10)]
mkiwt6b0 <- mkiwt6b00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
mkiwt6b <- cbind(data.table(mkiwt6b0[, c("mean", "sd")]), 
	type = rep(mylvls[3], length(mkiwt6b0$mean)),
	time = tbl6b$time)

mkoki6b00 <- tbl6b[, c(11:13)]
mkoki6b0 <- mkoki6b00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
mkoki6b <- cbind(data.table(mkoki6b0[, c("mean", "sd")]), 
	type = rep(mylvls[4], length(fkoki6b0$mean)),
	time = tbl6b$time)

tbl6b_merge <- rbind(fkiwt6b, fkoki6b, mkiwt6b, mkoki6b)



# c. auc ipgtt

tbl6c <- data.table(read_excel("Data/Fig6/PF_Fig6c_AUCipGTT.xlsx", col_names = FALSE))
setnames(tbl6c, rep(c("cohort1", "cohort2", "type"), 4))
tbl6c <- rbind(tbl6c[, 1:3], tbl6c[, 4:6], tbl6c[, 7:9], tbl6c[, 10:12])
tbl6c <- melt.data.table(tbl6c, id.vars = "type")
tbl6c <- tbl6c[!is.na(value), ]
setnames(tbl6c, c("type", "cohort", "value"))



# # d. fasting glc ipITT

# tbl6d <- data.table(read_excel("Data/Fig6/PF_Fig6d_fastingGlcipiTT.xlsx"))
# setnames(tbl6d, rep(c("glc", "type"), 4))
# tbl6d <- rbind(tbl6d[, 1:2], tbl6d[, 3:4], tbl6d[, 5:6], tbl6d[, 7:8])
# tbl6d <- melt.data.table(tbl6d, id.vars = "type")
# tbl6d <- tbl6d[!is.na(value), ]
# tbl6d[, value_mmol := value*0.0555]



# e. glc over time percent

tbl6e <- data.table(read_excel("Data/Fig6/PF_Fig6d_glcOverTimePercent.xlsx"))
colnames(tbl6e)

fkiwt6e00 <- tbl6e[, c(2:4)]
fkiwt6e0 <- fkiwt6e00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
fkiwt6e <- cbind(data.table(fkiwt6e0[, c("mean", "sd")]), 
	type = rep(mylvls[1], length(fkiwt6e0$mean)),
	time = tbl6e$time)

fkoki6e00 <- tbl6e[, c(5:7)]
fkoki6e0 <- fkoki6e00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
fkoki6e <- cbind(data.table(fkoki6e0[, c("mean", "sd")]), 
	type = rep(mylvls[2], length(fkoki6e0$mean)),
	time = tbl6e$time)

mkiwt6e00 <- tbl6e[, c(8:10)]
mkiwt6e0 <- mkiwt6e00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
mkiwt6e <- cbind(data.table(mkiwt6e0[, c("mean", "sd")]), 
	type = rep(mylvls[3], length(mkiwt6e0$mean)),
	time = tbl6e$time)

mkoki6e00 <- tbl6e[, c(11:13)]
mkoki6e0 <- mkoki6e00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
mkoki6e <- cbind(data.table(mkoki6e0[, c("mean", "sd")]), 
	type = rep(mylvls[4], length(fkoki6e0$mean)),
	time = tbl6e$time)

tbl6e_merge <- rbind(fkiwt6e, fkoki6e, mkiwt6e, mkoki6e)




# f. auc ipITT

tbl6f <- data.table(read_excel("Data/Fig6/PF_Fig6f_AUCipiTT.xlsx"))
setnames(tbl6f, rep(c("glc", "type"), 4))
tbl6f <- rbind(tbl6f[, 1:2], tbl6f[, 3:4], tbl6f[, 5:6], tbl6f[, 7:8])
tbl6f <- melt.data.table(tbl6f, id.vars = "type")
tbl6f <- tbl6f[!is.na(value), ]










# further variables for plotting

margs = c(0.3, 0.3, 0.3, 0.3)
margin = theme(plot.margin = unit(margs, "cm"))



####################################################################
# PLOTS AND ARRANGE AND SAVE
####################################################################



# a. fasting glc ipgtt

compare <- list(mylvls[c(1,2)], mylvls[c(3,4)])


# max_val <- max(tbl6a$value)
# min_val <- min(tbl6a$value)

# plt6a <- 
# ggplot(tbl6a, aes(x = type, y = value, color = type, fill = type)) +
# 	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
# 	geom_jitter(width = 0.1, aes(shape = cohort)) +
# 	stat_compare_means(comparisons = compare) +
# 	ylab("Fasting blood glc (ipGTT) [mmol/L]") +
# 	# ylim(0.9*min_val,1.2*max_val) +
# 	ylim(0, 17) +
# 	theme_pubr() +
# 	rotate_x_text(angle = 45) +
# 	scale_fill_manual(values = mypal) +
# 	scale_color_manual(values = mypal) +
# 	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank()) +
# 	margin




# b. glc over time

plt6b <- 
ggplot(tbl6b_merge, aes(x = time, y = mean, color = type)) +
	geom_line() +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6, width = 2) +
	geom_point(size = 2) +
	ylab("Glucose [mmol/L]") +
	xlab("Time [min]") +
	theme_pubr() +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none") +
	margin



# c. auc ipgtt

max_val <- max(tbl6c$value)
min_val <- min(tbl6c$value)

plt6c <- 
ggplot(tbl6c, aes(x = type, y = value, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(width = 0.1, aes(shape = cohort)) +
	stat_compare_means(comparisons = compare, size = 3) +
	ylab("AUC ipGTT") +
	ylim(0.9*min_val,1.2*max_val) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank()) +
	margin



# # d. fasting glc ipITT

# max_val <- max(tbl6d$value_mmol)
# min_val <- min(tbl6d$value_mmol)

# plt6d <- 
# ggplot(tbl6d, aes(x = type, y = value_mmol, color = type, fill = type)) +
# 	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
# 	geom_jitter(width = 0.1, shape = 17) +
# 	stat_compare_means(comparisons = compare) +
# 	ylab("Fasting blood glc (ipITT) [mmol/L]") +
# 	# ylim(0.9*min_val,1.2*max_val) +
# 	ylim(0, 17) +
# 	theme_pubr() +
# 	rotate_x_text(angle = 45) +
# 	scale_fill_manual(values = mypal) +
# 	scale_color_manual(values = mypal) +
# 	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank()) +
	# margin



# e. glc over time percent

plt6e <- 
ggplot(tbl6e_merge, aes(x = time, y = mean, color = type)) +
	geom_line() +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6, width = 2) +
	geom_point(size = 2) +
	ylab("Glucose [% of basal]") +
	xlab("Time [min]") +
	theme_pubr() +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none") +
	margin



# f. auc ipITT

max_val <- max(tbl6f$value)
min_val <- min(tbl6f$value)

plt6f <- 
ggplot(tbl6f, aes(x = type, y = value, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(width = 0.1, shape = 17) +
	stat_compare_means(comparisons = compare, size = 3) +
	ylab("AUC ipITT (of basal)") +
	ylim(0.9*min_val,1.2*max_val) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank()) +
	margin










# plt6 <-
# ggarrange(plt6b, plt6c, plt6e, plt6f, labels = "auto", widths = c(1, 1.4, 1), font.label = list(size = 18), common.legend = TRUE, legend = "bottom")


plt6 <-
plot_grid(plt6b, plt6c, plt6e, plt6f, align = "vh", axis = "bl", nrow = 2, ncol = 2, labels = "auto", rel_widths = c(1.5,1))

ggsave(paste(fig_path, "Fig6.png", sep = ""), plt6, device = png(), width = 5, height = 5, bg = "white")
dev.off()

ggsave(paste(fig_path, "Fig6.pdf", sep = ""), plt6, device = "pdf", width = 5, height = 5, bg = "white")
dev.off()




















