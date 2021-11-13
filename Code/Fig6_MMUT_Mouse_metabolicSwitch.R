### METABOLIC SWITCH IN A MOUSE MODEL OF METHYLMALONIC ACIDURIA
# FIGURE 6

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
# FIGURE 6
####################################################################
####################################################################

# theme & info:
# impaired glucose tolerance
# a. glc over time
# b. auc ipgtt
# c glc over time percent
# d. auc ipITT

####################################################################
# DATA IMPORT AND TIDY
####################################################################


# glc over time

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



# auc ipgtt

tbl6c <- data.table(read_excel("Data/Fig6/PF_Fig6c_AUCipGTT.xlsx", col_names = FALSE))
setnames(tbl6c, rep(c("cohort1", "cohort2", "type"), 4))
tbl6c <- rbind(tbl6c[, 1:3], tbl6c[, 4:6], tbl6c[, 7:9], tbl6c[, 10:12])
tbl6c <- melt.data.table(tbl6c, id.vars = "type")
tbl6c <- tbl6c[!is.na(value), ]
setnames(tbl6c, c("type", "cohort", "value"))



# glc over time percent

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




# auc ipITT

tbl6f <- data.table(read_excel("Data/Fig6/PF_Fig6f_AUCipiTT.xlsx"))
setnames(tbl6f, rep(c("glc", "type"), 4))
tbl6f <- rbind(tbl6f[, 1:2], tbl6f[, 3:4], tbl6f[, 5:6], tbl6f[, 7:8])
tbl6f <- melt.data.table(tbl6f, id.vars = "type")
tbl6f <- tbl6f[!is.na(value), ]











####################################################################
# PLOTS AND ARRANGE AND SAVE
####################################################################


margs = c(0.3, 0.3, 0.3, 0.3)
margin = theme(plot.margin = unit(margs, "cm"))



compare <- list(mylvls[c(1,2)], mylvls[c(3,4)])



# glc over time

line_df <- 
data.frame(dot_x = c(rep(min(tbl6b_merge$time), 4),rep(max(tbl6b_merge$time), 4)),
	dot_y = rep(tbl6b_merge[time == dot_start_x, ]$mean, 2),
	type = rep(tbl6b_merge[time == dot_start_x, ]$type, 2)
)

plt6b <- 
ggplot(tbl6b_merge, aes(x = time, y = mean, color = type)) +
	geom_line(data = line_df, aes(x = dot_x, y = dot_y, color = type), inherit.aes = FALSE, linetype = 2) +
	geom_line() +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6, width = 2) +
	geom_point(size = 2) +
	ylab("Glucose [mmol/L]") +
	xlab("Time [min]") +
	theme_pubr() +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none") +
	margin



# auc ipgtt

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




# glc over time percent

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



#auc ipITT

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






plt6 <-
plot_grid(plt6b, plt6c, plt6e, plt6f, align = "vh", axis = "bl", nrow = 2, ncol = 2, labels = "auto", rel_widths = c(1.5,1))

ggsave(paste(fig_path, "Fig6.png", sep = ""), plt6, device = png(), width = 5, height = 5, bg = "white")
dev.off()

ggsave(paste(fig_path, "Fig6.pdf", sep = ""), plt6, device = "pdf", width = 5, height = 5, bg = "white")
dev.off()




















