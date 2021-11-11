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
# FIGURE 7
####################################################################
####################################################################

# theme & info:
# liver damage
# a. liver weight
# b. transaminases (ALT, ASAT) and ALP
# c. hallmark metabolites
# d. MMA in liver
# e. glucose over time

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


# e. glucose over time

tbl7e <- data.table(read_excel("Data/Fig7/PF_Fig7E_glcOverTime.xlsx"))
colnames(tbl7e)

fkoki7e00 <- tbl7e[, c(2:9)]
fkoki7e0 <- fkoki7e00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
fkoki7e <- cbind(data.table(fkoki7e0[, c("mean", "sd")]), 
	type = rep(mylvls[2], length(fkoki7e0$mean)),
	time = tbl7e$time)

fkiwt7e00 <- tbl7e[, c(10:17)]
fkiwt7e0 <- fkiwt7e00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
fkiwt7e <- cbind(data.table(fkiwt7e0[, c("mean", "sd")]), 
	type = rep(mylvls[1], length(fkiwt7e0$mean)),
	time = tbl7e$time)

f7e <- rbind(fkiwt7e, fkoki7e)









# further variables for plotting

margs = c(0.5, 0.5, 0.5, 0.5)
margin = theme(plot.margin = unit(margs, "cm"))



####################################################################
# PLOTS AND ARRANGE AND SAVE
####################################################################


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
	theme(legend.title = element_blank(), legend.position = "bottom", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white")) +
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
	theme(legend.title = element_blank(), legend.position = "bottom", axis.title.x = element_blank(), axis.text.x = element_blank()) +
	margin



# e. glucose over time

# plt7e <- 
# ggplot(f7e, aes(x = time, y = mean, color = type)) +
# 	geom_line() +
# 	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6, width = 2) +
# 	geom_point(size = 2) +
# 	ylab("Glucose [mmol/L]") +
# 	xlab("Time [min]") +
# 	theme_pubr() +
# 	scale_color_manual(values = mypal[c(1,2)]) +
# 	theme(legend.title = element_blank()) +
# 	margin







plt7_top <-
ggarrange(plt7a, plt7b, labels = c("a", "b"), widths = c(1, 2.5), common.legend = TRUE, legend = "none")

plt7_bottom <-
ggarrange(plt7c, plt7d, labels = c("c", "d"), widths = c(2.9, 1), common.legend = TRUE, legend = "none")

plt7 <-
ggarrange(plt7_top, plt7_bottom, heights = c(1, 1), nrow = 2)


ggsave(paste(fig_path, "Fig7.png", sep = ""), plt7, device = png(), width = 8, height = 6, bg = "white")
dev.off()

ggsave(paste(fig_path, "Fig7.pdf", sep = ""), plt7, device = "pdf", width = 8, height = 6, bg = "white")
dev.off()
















