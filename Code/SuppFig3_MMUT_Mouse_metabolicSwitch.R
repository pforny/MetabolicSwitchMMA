# wd
Dropbox/PF/Post_PhD/Zurich_2019_2022/MMUT_Mouse_metabolicSwitch

# libraries
require(data.table)
require(ggplot2)
require(tidyverse)
require(readxl)
require(ggpubr)
library(ggplotify)

# color pallette (manual): order:
# female-KI/WT; female-KO/KI
# male_KI/WT; male-KO/KI
# mypal = c("#E67F7F", "#A40000", "#7D9EC0", "#0E467D")


# middle color female: #FF8100
# middle color male: #008080
mypal = c("#ffa64c", "#b25a00", "#66b2b2", "#004c4c")
mylvls = c("f_Mmut-ki/wt", "f_Mmut-ko/ki", "m_Mmut-ki/wt", "m_Mmut-ko/ki")


# create figures path
system("mkdir Figs")
system("mkdir Figs/v3")
fig_path <- c("Figs/v3/")



start_cold <- 11+1/3

####################################################################
####################################################################
# SUPP. FIGURE 3
####################################################################
####################################################################

# theme & info: calorimetry cold challenge supp data
# a: mean O2 consumption per type
# b: RER / time 
# c: cumulative distance / time
# d: distance per type

####################################################################
# DATA IMPORT AND TIDY
####################################################################




# glucose over time

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







# glucose over time

plt_glcTime <- 
ggplot(f7e, aes(x = time, y = mean, color = type)) +
	geom_line() +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6, width = 2) +
	geom_point(size = 2) +
	ylab("Glucose [mmol/L]") +
	xlab("Time [min]") +
	theme_pubr() +
	scale_color_manual(values = mypal[c(1,2)]) +
	theme(legend.title = element_blank(), legend.position = c(0.8, 0.8))






pltsupp3 <- 
ggarrange(plt_glcTime, labels = "b", ncol = 1, nrow = 1)


ggsave(paste(fig_path, "SuppFig3.png", sep = ""), pltsupp3, device = png(), width = 4, height = 3, bg = "white")
dev.off()

ggsave(paste(fig_path, "SuppFig3.pdf", sep = ""), pltsupp3, device = "pdf", width = 4, height = 3, bg = "white")
dev.off()




##########################################
##########################################
##########################################
##########################################
##########################################
##########################################
##########################################
##########################################
# old code





# a: mean O2 consumption per type

# tbl2a_supp <- data.table(read_excel("Data/SuppFig2/PF_SuppFig2A_avgO2cons.xlsx"))
# colnames(tbl2a_supp)

# tbl2a_supp_neutral <- tbl2a_supp[, 1:67]
# tbl2a_supp_cold <- tbl2a_supp[, 69:135]
# length(1:67) == length(69:135)
# data.frame(colnames(tbl2a_supp_neutral), colnames(tbl2a_supp_cold))
# tbl2a_supp <- rbind(tbl2a_supp_neutral, tbl2a_supp_cold, use.names = FALSE)
# colnames(tbl2a_supp)
# tbl2a_supp <- tbl2a_supp[, -c(17, 18, 35)]
# ncol(tbl2a_supp)
# setnames(tbl2a_supp, c("condition", rep(mylvls[1], 15), rep(mylvls[2], 16), rep(mylvls[3], 17), rep(mylvls[4], 15)))
# tbl2a_supp <- melt.data.table(tbl2a_supp, id.vars = "condition")
# tbl2a_supp <- tbl2a_supp[!is.na(value), ]
# setnames(tbl2a_supp, c("condition", "type", "value"))
# tbl2a_supp$condition <- factor(tbl2a_supp$condition, levels = c("neutral", "cold"))



# b (now a): RER / time 

tbl2b_supp <- data.table(read_excel("Data/SuppFig2/PF_RER_SuppFig2b.xlsx"))
colnames(tbl2b_supp)

mkoki2b00_supp <- tbl2b_supp[, c(2:18)]
mkoki2b0_supp <- mkoki2b00_supp %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
mkoki2b_supp <- cbind(data.table(mkoki2b0_supp[, c("mean", "sd")]), 
	type = rep(mylvls[4], length(mkoki2b0_supp$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkoki2b0_supp$mean)))

mkiwt2b00_supp <- tbl2b_supp[, c(36:52)]
mkiwt2b0_supp <- mkiwt2b00_supp %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
mkiwt2b_supp <- cbind(data.table(mkiwt2b0_supp[, c("mean", "sd")]), 
	type = rep(mylvls[3], length(mkiwt2b0_supp$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkiwt2b0_supp$mean)))

m2b_supp <- rbind(mkiwt2b_supp, mkoki2b_supp)


fkoki2b00_supp <- tbl2b_supp[, c(19:35)]
fkoki2b0_supp <- fkoki2b00_supp %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
fkoki2b_supp <- cbind(data.table(fkoki2b0_supp[, c("mean", "sd")]), 
	type = rep(mylvls[2], length(fkoki2b0_supp$mean)),
	timemod = seq(0, by = 1/3, length.out = length(fkoki2b0_supp$mean)))

fkiwt2b00_supp <- tbl2b_supp[, c(53:69)]
fkiwt2b0_supp <- fkiwt2b00_supp %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
fkiwt2b_supp <- cbind(data.table(fkiwt2b0_supp[, c("mean", "sd")]), 
	type = rep("f_Mmut-ki/wt", length(fkiwt2b0_supp$mean)),
	timemod = seq(0, by = 1/3, length.out = length(fkiwt2b0_supp$mean)))

f2b_supp <- rbind(fkiwt2b_supp, fkoki2b_supp)


m2b_supp[, gender := "male"]
f2b_supp[, gender := "female"]

tbl2b_supp_merge <- rbind(m2b_supp, f2b_supp)
tbl2b_supp_merge[, condition := ifelse(timemod > 11, "cold", "neutral")]
tbl2b_supp_merge$condition <- factor(tbl2b_supp_merge$condition, levels = c("neutral", "cold"))



# new b: carbohydrate oxidation (as moved here from fig 3)

tbl3c <- data.table(read_excel("Data/Fig3/PF_Fig3_CHOoxidation.xlsx"))
colnames(tbl3c)

mkoki3c00 <- tbl3c[, c(53:67)]
mkoki3c0 <- mkoki3c00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
mkoki3c <- cbind(data.table(mkoki3c0[, c("mean", "sd")]), 
	type = rep(mylvls[4], length(mkoki3c0$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkoki3c0$mean)))

mkiwt3c00 <- tbl3c[, c(36:51)]
mkiwt3c0 <- mkiwt3c00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
mkiwt3c <- cbind(data.table(mkiwt3c0[, c("mean", "sd")]), 
	type = rep(mylvls[3], length(mkiwt3c0$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkiwt3c0$mean)))

m3c <- rbind(mkiwt3c, mkoki3c)


fkoki3c00 <- tbl3c[, c(19:34)]
fkoki3c0 <- fkoki3c00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
fkoki3c <- cbind(data.table(fkoki3c0[, c("mean", "sd")]), 
	type = rep(mylvls[2], length(fkoki3c0$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkoki3c0$mean)))

fkiwt3c00 <- tbl3c[, c(2:15)]
fkiwt3c0 <- fkiwt3c00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
fkiwt3c <- cbind(data.table(fkiwt3c0[, c("mean", "sd")]), 
	type = rep(mylvls[1], length(fkiwt3c0$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkiwt3c0$mean)))

f3c <- rbind(fkiwt3c, fkoki3c)


m3c[, gender := "male"]
f3c[, gender := "female"]

tbl3c_merge <- rbind(m3c, f3c)
tbl3c_merge[, condition := ifelse(timemod > 11, "cold", "neutral")]
tbl3c_merge$condition <- factor(tbl3c_merge$condition, levels = c("neutral", "cold"))


# tbl3cmeans <- data.table(read_excel("Data/Fig3/PF_Fig3_CHOoxidationMEANS.xlsx"))
# colnames(tbl3cmeans)
# setnames(tbl3cmeans, c("condition",
# 	rep(mylvls[1], length(2:18)),
# 	rep(mylvls[2], length(19:35)),
# 	rep(mylvls[3], length(36:52)),
# 	rep(mylvls[4], length(53:67))))
# tbl3cmeans <- melt.data.table(tbl3cmeans, id.vars = "condition")
# tbl3cmeans <- tbl3cmeans[!is.na(value), ]
# setnames(tbl3cmeans, c("condition", "type", "value"))
# tbl3cmeans$condition <- factor(tbl3cmeans$condition, levels = c("thermo", "cold"))
# levels(tbl3cmeans$condition) <- c("neutral", "cold")



# c: cumulative distance / time

tbl2c_supp <- data.table(read_excel("Data/SuppFig2/PF_CumulativeDistance_SuppFig2c.xlsx"))
colnames(tbl2c_supp)

mkoki2c00_supp <- tbl2c_supp[, c(2:18)]
mkoki2c0_supp <- mkoki2c00_supp %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
mkoki2c_supp <- cbind(data.table(mkoki2c0_supp[, c("mean", "sd")]), 
	type = rep(mylvls[4], length(mkoki2c0_supp$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkoki2c0_supp$mean)))

mkiwt2c00_supp <- tbl2c_supp[, c(36:52)]
mkiwt2c0_supp <- mkiwt2c00_supp %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
mkiwt2c_supp <- cbind(data.table(mkiwt2c0_supp[, c("mean", "sd")]), 
	type = rep(mylvls[3], length(mkiwt2c0_supp$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkiwt2c0_supp$mean)))

m2c_supp <- rbind(mkiwt2c_supp, mkoki2c_supp)


fkoki2c00_supp <- tbl2c_supp[, c(19:35)]
fkoki2c0_supp <- fkoki2c00_supp %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
fkoki2c_supp <- cbind(data.table(fkoki2c0_supp[, c("mean", "sd")]), 
	type = rep(mylvls[2], length(fkoki2c0_supp$mean)),
	timemod = seq(0, by = 1/3, length.out = length(fkoki2c0_supp$mean)))

fkiwt2c00_supp <- tbl2c_supp[, c(53:67)]
fkiwt2c0_supp <- fkiwt2c00_supp %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
fkiwt2c_supp <- cbind(data.table(fkiwt2c0_supp[, c("mean", "sd")]), 
	type = rep("f_Mmut-ki/wt", length(fkiwt2c0_supp$mean)),
	timemod = seq(0, by = 1/3, length.out = length(fkiwt2c0_supp$mean)))

f2c_supp <- rbind(fkiwt2c_supp, fkoki2c_supp)


m2c_supp[, gender := "male"]
f2c_supp[, gender := "female"]


tbl2c_supp_merge <- rbind(m2c_supp, f2c_supp)
tbl2c_supp_merge[, condition := ifelse(timemod > 11, "cold", "neutral")]
tbl2c_supp_merge$condition <- factor(tbl2c_supp_merge$condition, levels = c("neutral", "cold"))


# start_cold variable doesn't work for some reason
xno <- min(tbl2c_supp_merge[condition == "cold" & type == mylvls[1], ]$timemod)


diff <- tbl2c_supp_merge[condition == "neutral" & type == mylvls[1] & timemod == 0, ]$mean
tbl2c_supp_merge[condition == "neutral" & type == mylvls[1], phase_mean := mean-diff]
diff <- tbl2c_supp_merge[condition == "neutral" & type == mylvls[2] & timemod == 0, ]$mean
tbl2c_supp_merge[condition == "neutral" & type == mylvls[2], phase_mean := mean-diff]

diff <- tbl2c_supp_merge[condition == "cold" & type == mylvls[1] & timemod == xno, ]$mean
tbl2c_supp_merge[condition == "cold" & type == mylvls[1], phase_mean := mean-diff]
diff <- tbl2c_supp_merge[condition == "cold" & type == mylvls[2] & timemod == xno, ]$mean
tbl2c_supp_merge[condition == "cold" & type == mylvls[2], phase_mean := mean-diff]


diff <- tbl2c_supp_merge[condition == "neutral" & type == mylvls[3] & timemod == 0, ]$mean
tbl2c_supp_merge[condition == "neutral" & type == mylvls[3], phase_mean := mean-diff]
diff <- tbl2c_supp_merge[condition == "neutral" & type == mylvls[4] & timemod == 0, ]$mean
tbl2c_supp_merge[condition == "neutral" & type == mylvls[4], phase_mean := mean-diff]

diff <- tbl2c_supp_merge[condition == "cold" & type == mylvls[3] & timemod == xno, ]$mean
tbl2c_supp_merge[condition == "cold" & type == mylvls[3], phase_mean := mean-diff]
diff <- tbl2c_supp_merge[condition == "cold" & type == mylvls[4] & timemod == xno, ]$mean
tbl2c_supp_merge[condition == "cold" & type == mylvls[4], phase_mean := mean-diff]






# c (same panel as the graph above): distance per type

tbl2d_supp <- data.table(read_excel("Data/SuppFig2/PF_SuppFig2D_distance.xlsx"))
colnames(tbl2d_supp)
setnames(tbl2d_supp, mylvls)
tbl2d_supp <- melt.data.table(tbl2d_supp)
setnames(tbl2d_supp, c("type", "value"))
tbl2d_supp <- tbl2d_supp[!is.na(value), ]




# more variables
# indicate starting point of cold exposure
start_cold <- 11+1/3
light_cycle <- c(5, 17)
lights_off <- light_cycle[1]
lights_on <- light_cycle[2]

margs = c(0.5, 0.5, 0.5, 0.5)
margin = theme(plot.margin = unit(margs, "cm"))


####################################################################
# PLOTS AND ARRANGE AND SAVE
####################################################################


compare <- list(c(mylvls[1], mylvls[2]), c(mylvls[3], mylvls[4]))



# a: mean O2 consumption per type

# max_val <- max(tbl2a_supp$value)

# plt2a_supp <- 
# ggplot(tbl2a_supp, aes(x = type, y = value)) +
# 	geom_boxplot(alpha = 0.6, outlier.shape = NA, aes(color = type, fill = type)) +
# 	geom_jitter(width = 0.1, aes(color = type, fill = type)) +
# 	facet_grid(~condition) +
# 	stat_compare_means(comparisons = compare) +
# 	ylab("Mean O2 consumption [mL]") +
# 	ylim(0,(max_val+0.2*max_val)) +
# 	theme_pubr() +
# 	rotate_x_text(angle = 45) +
# 	scale_fill_manual(values = mypal) +
# 	scale_color_manual(values = mypal) +
# 	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank()) +
# 	margin



# b (now a): RER / time 

top_value <- max(f2b_supp$mean)+max(f2b_supp$sd)
min_value <- min(f2b_supp$mean)-max(f2b_supp$sd)

# female
plt2b_supp_chart_f <-
ggplot(f2b_supp, aes(x = timemod, y = mean, color = type)) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(f2b_supp$timemod), ymin = top_value*1.03, ymax = top_value*1.06, fill = "lightgrey", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, max(f2b_supp$timemod))), y = mean(c(top_value*1.03, top_value*1.06)), color = "black", label = "cold", size = 3) +
	annotate(geom = "rect", xmin = min(f2b_supp$timemod), xmax = start_cold, ymin = top_value*1.03, ymax = top_value*1.06, fill = "white", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, min(f2b_supp$timemod))), y = mean(c(top_value*1.03, top_value*1.06)), color = "black", label = "neutral", size = 3) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(f2b_supp$timemod), ymin = min_value, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
	geom_point(shape = 17, size = 1) +
	annotate(geom = "segment", x = lights_on, xend = lights_on, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "segment", x = lights_off, xend = lights_off, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "text", x = lights_on, y = top_value*1.015, label = "on", angle = 90, size = 3) +
	annotate(geom = "text", x = lights_off, y = top_value*1.015, label = "off", angle = 90, size = 3) +
	ylab(expression("RER"~"[VCO"[2]/VO[2]*"]")) +
	xlab("Time [h]") +
	scale_color_manual(values = mypal[c(1,2)]) +
	theme_pubr() +
	theme(legend.title = element_blank(), legend.position = "none")


compare2b_supp_f <- list(c(mylvls[1], mylvls[2]))
max_val_f <- max(tbl2b_supp_merge[type == mylvls[1] | type == mylvls[2] ,]$mean)

plt2b_supp_means_f_prov <-
ggplot(tbl2b_supp_merge[type == mylvls[1] | type == mylvls[2], ], aes(x = type, y = mean, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(shape = 17, width = 0.1) +
	stat_compare_means(comparisons = compare2b_supp_f, label.y = (max_val_f+0.01*max_val_f)) +
	ylab(expression("Mean"~"RER"~"[VCO"[2]/VO[2]*"]")) +
	ylim(0.72, 0.83) +
	facet_wrap(.~condition) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal[c(1,2)]) +
	scale_color_manual(values = mypal[c(1,2)]) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank())

g <- ggplot_gtable(ggplot_build(plt2b_supp_means_f_prov))
stripr <- which(grepl('strip-t', g$layout$name))
fills <- c("white", "lightgrey")
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}

plt2b_supp_means_f <- as.ggplot(g)


plt2b_supp_f <-
ggarrange(plt2b_supp_chart_f, plt2b_supp_means_f, nrow = 2)

# ggplot(tbl2b_supp_merge, aes(x = type, y = mean, color = type, fill = type)) +
# 	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
# 	geom_jitter(shape = 17, width = 0.1) +
# 	stat_compare_means(comparisons = compare3c_m) +
# 	ylab("Mean O2 cons. [mL/20 min]") +
# 	facet_grid(.~gender+condition, scales = "free", labeller = labeller(gender = function(x) {rep("", length(x))})) +
# 	theme_pubr() +
# 	rotate_x_text(angle = 45) +
# 	scale_fill_manual(values = mypal) +
# 	scale_color_manual(values = mypal) +
# 	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_blank())




# male
plt2b_supp_chart_m <-
ggplot(m2b_supp, aes(x = timemod, y = mean, color = type)) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(f2b_supp$timemod), ymin = top_value*1.03, ymax = top_value*1.06, fill = "lightgrey", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, max(f2b_supp$timemod))), y = mean(c(top_value*1.03, top_value*1.06)), color = "black", label = "cold", size = 3) +
	annotate(geom = "rect", xmin = min(f2b_supp$timemod), xmax = start_cold, ymin = top_value*1.03, ymax = top_value*1.06, fill = "white", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, min(f2b_supp$timemod))), y = mean(c(top_value*1.03, top_value*1.06)), color = "black", label = "neutral", size = 3) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(m2b_supp$timemod), ymin = min_value, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
	geom_point(shape = 17, size = 1) +
	annotate(geom = "segment", x = lights_on, xend = lights_on, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "segment", x = lights_off, xend = lights_off, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "text", x = lights_on, y = top_value*1.015, label = "on", angle = 90, size = 3) +
	annotate(geom = "text", x = lights_off, y = top_value*1.015, label = "off", angle = 90, size = 3) +
	ylab(expression("RER"~"[VCO"[2]/VO[2]*"]")) +
	xlab("Time [h]") +
	scale_color_manual(values = mypal[c(3,4)]) +
	theme_pubr() +
	theme(legend.title = element_blank(), legend.position = "none")



compare2b_supp_m <- list(c(mylvls[3], mylvls[4]))
max_val_m <- max(tbl2b_supp_merge[type == mylvls[3] | type == mylvls[4] ,]$mean)

plt2b_supp_means_m_prov <-
ggplot(tbl2b_supp_merge[type == mylvls[3] | type == mylvls[4], ], aes(x = type, y = mean, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(shape = 17, width = 0.1) +
	stat_compare_means(comparisons = compare2b_supp_m, label.y = (max_val_m+0.01*max_val_m)) +
	ylab(expression("Mean"~"RER"~"[VCO"[2]/VO[2]*"]")) +
	ylim(0.72, 0.83) +
	facet_wrap(.~condition) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal[c(3,4)]) +
	scale_color_manual(values = mypal[c(3,4)]) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank())

g <- ggplot_gtable(ggplot_build(plt2b_supp_means_m_prov))
stripr <- which(grepl('strip-t', g$layout$name))
fills <- c("white", "lightgrey")
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}

plt2b_supp_means_m <- as.ggplot(g)



plt2b_supp_m <-
ggarrange(plt2b_supp_chart_m, plt2b_supp_means_m, nrow = 2)

plt2a_supp <-
ggarrange(plt2b_supp_f, plt2b_supp_m) + margin







# b CHO oxidation plot (was fig. 3 before)

top_value <- max(m3c$mean)+max(m3c$sd)
min_value <- min(m3c$mean)-max(m3c$sd)

# male

plt3c_m_chart <- 
ggplot(m3c, aes(x = timemod, y = mean, color = type)) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(m3c$timemod), ymin = top_value*1.2, ymax = top_value*1.4, fill = "lightgrey", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, max(m3c$timemod))), y = mean(c(top_value*1.2, top_value*1.4)), color = "black", label = "cold", size = 3) +
	annotate(geom = "rect", xmin = min(m3c$timemod), xmax = start_cold, ymin = top_value*1.2, ymax = top_value*1.4, fill = "white", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, min(m3c$timemod))), y = mean(c(top_value*1.2, top_value*1.4)), color = "black", label = "neutral", size = 3) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(m3c$timemod), ymin = min_value, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
	geom_point(shape = 17, size = 1) +
	annotate(geom = "segment", x = lights_on, xend = lights_on, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "segment", x = lights_off, xend = lights_off, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "text", x = lights_on, y = top_value*1.1, label = "on", angle = 90, size = 3) +
	annotate(geom = "text", x = lights_off, y = top_value*1.1, label = "off", angle = 90, size = 3) +
	ylab("CHO oxidation [mL/min]") +
	xlab("Time [h]") +
	scale_color_manual(values = mypal[c(3,4)]) +
	theme_pubr() +
	theme(legend.title = element_blank(), legend.position = "none")


compare3c_m <- list(c(mylvls[3], mylvls[4]))
max_val_m <- max(tbl3c_merge[type == mylvls[3] | type == mylvls[4] ,]$mean)


plt3c_m_means_prov <-
ggplot(tbl3c_merge[type == mylvls[3] | type == mylvls[4], ], aes(x = type, y = mean, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(shape = 17, width = 0.1) +
	stat_compare_means(comparisons = compare3c_m, label.y = max_val_m*1.02) +
	ylab(expression("Mean"~"RER"~"[VCO"[2]/VO[2]*"]")) +
	ylim(0, max_val_m*1.1) +
	facet_wrap(.~condition) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal[c(3,4)]) +
	scale_color_manual(values = mypal[c(3,4)]) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank())

g <- ggplot_gtable(ggplot_build(plt3c_m_means_prov))
stripr <- which(grepl('strip-t', g$layout$name))
fills <- c("white", "lightgrey")
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}

plt3c_m_means <- as.ggplot(g)


plt3c_m <-
ggarrange(plt3c_m_chart, plt3c_m_means, nrow = 2)


# female

plt3c_f_chart <- 
ggplot(f3c, aes(x = timemod, y = mean, color = type)) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(f3c$timemod), ymin = top_value*1.2, ymax = top_value*1.4, fill = "lightgrey", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, max(f3c$timemod))), y = mean(c(top_value*1.2, top_value*1.4)), color = "black", label = "cold", size = 3) +
	annotate(geom = "rect", xmin = min(f3c$timemod), xmax = start_cold, ymin = top_value*1.2, ymax = top_value*1.4, fill = "white", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, min(f3c$timemod))), y = mean(c(top_value*1.2, top_value*1.4)), color = "black", label = "neutral", size = 3) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(f3c$timemod), ymin = min_value, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
	geom_point(shape = 17, size = 1) +
	annotate(geom = "segment", x = lights_on, xend = lights_on, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "segment", x = lights_off, xend = lights_off, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "text", x = lights_on, y = top_value*1.1, label = "on", angle = 90, size = 3) +
	annotate(geom = "text", x = lights_off, y = top_value*1.1, label = "off", angle = 90, size = 3) +
	ylab("CHO oxidation [mL/min]") +
	xlab("Time [h]") +
	scale_color_manual(values = mypal[c(1,2)]) +
	theme_pubr() +
	theme(legend.title = element_blank(), legend.position = "none")


compare3c_f <- list(c(mylvls[1], mylvls[2]))
max_val_f <- max(tbl3c_merge[type == mylvls[1] | type == mylvls[2] ,]$mean)


plt3c_f_means_prov <-
ggplot(tbl3c_merge[type == mylvls[1] | type == mylvls[2], ], aes(x = type, y = mean, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(shape = 17, width = 0.1) +
	stat_compare_means(comparisons = compare3c_f, label.y = max_val_f*1.02) +
	ylab(expression("Mean"~"RER"~"[VCO"[2]/VO[2]*"]")) +
	ylim(0, max_val_f*1.1) +
	facet_wrap(.~condition) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal[c(1,2)]) +
	scale_color_manual(values = mypal[c(1,2)]) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank())

g <- ggplot_gtable(ggplot_build(plt3c_f_means_prov))
stripr <- which(grepl('strip-t', g$layout$name))
fills <- c("white", "lightgrey")
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}

plt3c_f_means <- as.ggplot(g)


plt3c_f <-
ggarrange(plt3c_f_chart, plt3c_f_means, nrow = 2)


plt2b_supp <-
ggarrange(plt3c_f, plt3c_m) + margin








# c: cumulative distance / time

compare_m <- list(c(mylvls[3], mylvls[4]))
compare_f <- list(c(mylvls[1], mylvls[2]))

top_value <- max(tbl2c_supp_merge$mean)+max(tbl2c_supp_merge$sd)
min_value <- min(tbl2c_supp_merge$mean)-max(tbl2c_supp_merge$sd)

# male

cum_dist_m <- 
ggplot(tbl2c_supp_merge[gender == "male", ], aes(x = timemod, y = mean, color = type)) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(f3c$timemod), ymin = top_value*1.2, ymax = top_value*1.4, fill = "lightgrey", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, max(f3c$timemod))), y = mean(c(top_value*1.2, top_value*1.4)), color = "black", label = "cold", size = 3) +
	annotate(geom = "rect", xmin = min(f3c$timemod), xmax = start_cold, ymin = top_value*1.2, ymax = top_value*1.4, fill = "white", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, min(f3c$timemod))), y = mean(c(top_value*1.2, top_value*1.4)), color = "black", label = "neutral", size = 3) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(m3c$timemod), ymin = min_value, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
	geom_point(shape = 17, size = 1) +
	annotate(geom = "segment", x = lights_on, xend = lights_on, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "segment", x = lights_off, xend = lights_off, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "text", x = lights_on, y = top_value*1.1, label = "on", angle = 90, size = 3) +
	annotate(geom = "text", x = lights_off, y = top_value*1.1, label = "off", angle = 90, size = 3) +
	ylab("Cumulative distance [cm]") +
	xlab("Time [h]") +
	scale_color_manual(values = mypal[c(3,4)]) +
	theme_pubr() +
	theme(legend.title = element_blank(), legend.position = "none")


max_value <- max(tbl2c_supp_merge$phase_mean)

cum_dist_m_box <-
ggplot(tbl2c_supp_merge[gender == "male" ,], aes(x = type, y = phase_mean, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(shape = 17, width = 0.1) +
	stat_compare_means(comparisons = compare_m, label.y = (max_value*1.05)) +
	ylab("Mean distance [cm]") +
	facet_wrap(.~condition) +
	ylim(0, max_value*1.15) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal[c(3,4)]) +
	scale_color_manual(values = mypal[c(3,4)]) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank())

g <- ggplot_gtable(ggplot_build(cum_dist_m_box))
stripr <- which(grepl('strip-t', g$layout$name))
fills <- c("white", "lightgrey")
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}

cum_dist_m_box2 <- as.ggplot(g)



dist_m <-
ggarrange(cum_dist_m, cum_dist_m_box2, nrow = 2)


# female

cum_dist_f <- 
ggplot(tbl2c_supp_merge[gender == "female", ], aes(x = timemod, y = mean, color = type)) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(f3c$timemod), ymin = top_value*1.2, ymax = top_value*1.4, fill = "lightgrey", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, max(f3c$timemod))), y = mean(c(top_value*1.2, top_value*1.4)), color = "black", label = "cold", size = 3) +
	annotate(geom = "rect", xmin = min(f3c$timemod), xmax = start_cold, ymin = top_value*1.2, ymax = top_value*1.4, fill = "white", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, min(f3c$timemod))), y = mean(c(top_value*1.2, top_value*1.4)), color = "black", label = "neutral", size = 3) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(m3c$timemod), ymin = min_value, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
	geom_point(shape = 17, size = 1) +
	annotate(geom = "segment", x = lights_on, xend = lights_on, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "segment", x = lights_off, xend = lights_off, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "text", x = lights_on, y = top_value*1.1, label = "on", angle = 90, size = 3) +
	annotate(geom = "text", x = lights_off, y = top_value*1.1, label = "off", angle = 90, size = 3) +
	ylab("Cumulative distance [cm]") +
	xlab("Time [h]") +
	scale_color_manual(values = mypal[c(1,2)]) +
	theme_pubr() +
	theme(legend.title = element_blank(), legend.position = "none")


max_value <- max(tbl2c_supp_merge$phase_mean)

cum_dist_f_box <-
ggplot(tbl2c_supp_merge[gender == "female" ,], aes(x = type, y = phase_mean, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(shape = 17, width = 0.1) +
	stat_compare_means(comparisons = compare_f, label.y = (max_value*1.05)) +
	ylab("Mean distance [cm]") +
	facet_wrap(.~condition) +
	ylim(0, max_value*1.15) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal[c(1,2)]) +
	scale_color_manual(values = mypal[c(1,2)]) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank())

g <- ggplot_gtable(ggplot_build(cum_dist_f_box))
stripr <- which(grepl('strip-t', g$layout$name))
fills <- c("white", "lightgrey")
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}

cum_dist_f_box2 <- as.ggplot(g)



dist_f <-
ggarrange(cum_dist_f, cum_dist_f_box2, nrow = 2)


dist_plt <-
ggarrange(dist_f, dist_m) + margin






plt2_supp <-
ggarrange(plt2a_supp, plt2b_supp, dist_plt, labels = c("a", "b", "c"), font.label = list(size = 18), common.legend = TRUE)

ggsave(paste(fig_path, "SuppFig3.png", sep = ""), plt2_supp, device = png(), width = 12, height = 11, bg = "white")
dev.off()











