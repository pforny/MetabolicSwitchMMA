# wd
Dropbox/PF/Post_PhD/Zurich_2019_2022/MMUT_Mouse_metabolicSwitch

# libraries
require(data.table)
require(ggplot2)
require(tidyverse)
require(readxl)
require(ggpubr)
library(ggplotify)
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
# FIGURE 3
####################################################################
####################################################################

# theme & info: calorimetry
# a: explanatory figure
# b: O2 consumption 
# c: carbohydrate oxidation
# d: lipid oxidation (based on O2 and CHO oxidation)
# e: mean heat (average over time for each mouse) / body weight (mean before and after) ratio
# f: mean heat / lean mass

####################################################################
# DATA IMPORT AND TIDY
####################################################################





margs = c(0.5, 0.5, 0.5, 0.5)
margin = theme(plot.margin = unit(margs, "cm"))






# master file

master3 <- data.table(read_excel("Data/Fig3/ParsedData_masterfile_cleaned_addedLOXandCHOOX.xlsx"))
colnames(master3)






####################################################################
# O2 (VO2 in table)


# create table to calculate means and sds per time point (not per mouse)
o2_tbl <- master3[,c(7, 27:89)]
colnames(o2_tbl)
setnames(o2_tbl, c("type", seq(0, by = 1/3, length.out = (length(o2_tbl)-1))))
o2_tbl2 <- melt.data.table(o2_tbl) %>% group_by(type, variable) %>% summarize(mean = mean(value), sd = sd(value))
o2_tbl2 <- data.table(o2_tbl2)
o2_tbl2$variable <- as.numeric(as.character(o2_tbl2$variable))
o2_tbl2[, sex := ifelse(type == mylvls[1] | type == mylvls[2], "female", "male")]



# plot variables
# indicate starting point of cold exposure
start_cold <- 11+1/3
light_cycle <- c(5, 17)
lights_off <- light_cycle[1]
lights_on <- light_cycle[2]
top_value <- max(o2_tbl2$mean)+max(o2_tbl2$sd)


o2_raw_plt <- 
ggplot(o2_tbl2, aes(x = variable, y = mean, color = type)) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(o2_tbl2$variable), ymin = top_value*1.2, ymax = top_value*1.35, fill = "lightgrey", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, max(o2_tbl2$variable))), y = mean(c(top_value*1.2, top_value*1.35)), color = "black", label = "cold", size = 3) +
	annotate(geom = "rect", xmin = min(o2_tbl2$variable), xmax = start_cold, ymin = top_value*1.2, ymax = top_value*1.35, fill = "white", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, min(o2_tbl2$variable))), y = mean(c(top_value*1.2, top_value*1.35)), color = "black", label = "neutral", size = 3) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(o2_tbl2$variable), ymin = 0, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
	geom_point(shape = 17, size = 1) +
	annotate(geom = "segment", x = lights_on, xend = lights_on, y = 0, yend = top_value, linetype = "dashed") +
	annotate(geom = "segment", x = lights_off, xend = lights_off, y = 0, yend = top_value, linetype = "dashed") +
	annotate(geom = "text", x = lights_on, y = top_value*1.1, label = "on", angle = 90, size = 3) +
	annotate(geom = "text", x = lights_off, y = top_value*1.1, label = "off", angle = 90, size = 3) +
	ylab(expression("O"[2]*" consumption [mL/min]")) +
	xlab("Time [h]") +
	facet_wrap(.~sex) +
	scale_color_manual(values = mypal) +
	theme_pubr() +
	theme(legend.title = element_blank(), legend.position = "none", strip.background = element_blank())




# create table to calculate means per animal (grouped by neutral and cold phase)
o2_tbl_neutral <- master3[,c(27:(27+32))]
o2_tbl_neutral1 <- o2_tbl_neutral %>% rowwise() %>% summarize(mean_neutral = mean(c_across(where(is.numeric))))

o2_tbl_cold <- master3[,c((27+32):89)]
o2_tbl_cold1 <- o2_tbl_cold %>% rowwise() %>% summarize(mean_cold = mean(c_across(where(is.numeric))))

o2_tbl3 <- cbind(o2_tbl_neutral1, o2_tbl_cold1, master3$leanMass, master3$typeReal)
setnames(o2_tbl3, c("mean_neutral", "mean_cold", "leanMass", "type"))
o2_tbl3 <- data.table(o2_tbl3)

ggplot(o2_tbl3, aes(x = leanMass, y = mean_neutral, color = type)) +
	geom_point() +
	geom_smooth(method = "lm")



#######
# correct 02 consumption with lean mass

# split the sexes
o2_tbl3_f <- o2_tbl3[type == mylvls[1] | type == mylvls[2], ]
o2_tbl3_m <- o2_tbl3[type != mylvls[1] & type != mylvls[2], ]

# average lean mass
o2_tbl3_f$avLM <- mean(o2_tbl3$leanMass, na.rm = TRUE)
o2_tbl3_m$avLM <- mean(o2_tbl3$leanMass, na.rm = TRUE)

# linear model for O2 consumption
mod_neutral_f <- lm(mean_neutral ~ leanMass + type, data = o2_tbl3_f)
mod_neutral_m <- lm(mean_neutral ~ leanMass + type, data = o2_tbl3_m)
summary(mod_neutral_f)
summary(mod_neutral_m)
mod_cold_f <- lm(mean_cold ~ leanMass + type, data = o2_tbl3_f)
mod_cold_m <- lm(mean_cold ~ leanMass + type, data = o2_tbl3_m)
summary(mod_cold_f)
summary(mod_cold_m)

# add residuals to data.table
o2_tbl3_f$resid_O2_neutral <- resid(mod_neutral_f)
o2_tbl3_m$resid_O2_neutral <- resid(mod_neutral_m)
o2_tbl3_f$resid_O2_cold <- resid(mod_cold_f)
o2_tbl3_m$resid_O2_cold <- resid(mod_cold_m)

# calculate ajdusted lean mass
o2_tbl3_f <- o2_tbl3_f %>%
  mutate(neutral = case_when(
    type==mylvls[1]~ coef(mod_neutral_f)[1] + coef(mod_neutral_f)[2]*avLM + resid_O2_neutral,
    type==mylvls[2]~ (coef(mod_neutral_f)[1]+ coef(mod_neutral_f)[3]) + coef(mod_neutral_f)[2]*avLM + resid_O2_neutral),
  	cold = case_when(
    type==mylvls[1]~ coef(mod_cold_f)[1] + coef(mod_cold_f)[2]*avLM + resid_O2_cold,
    type==mylvls[2]~ (coef(mod_cold_f)[1]+ coef(mod_cold_f)[3]) + coef(mod_cold_f)[2]*avLM + resid_O2_cold))

o2_tbl3_m <- o2_tbl3_m %>%
  mutate(neutral = case_when(
    type==mylvls[3]~ coef(mod_neutral_m)[1] + coef(mod_neutral_m)[2]*avLM + resid_O2_neutral,
    type==mylvls[4]~ (coef(mod_neutral_m)[1]+ coef(mod_neutral_m)[3]) + coef(mod_neutral_m)[2]*avLM + resid_O2_neutral),
  	cold = case_when(
    type==mylvls[3]~ coef(mod_cold_m)[1] + coef(mod_cold_m)[2]*avLM + resid_O2_cold,
    type==mylvls[4]~ (coef(mod_cold_m)[1]+ coef(mod_cold_m)[3]) + coef(mod_cold_m)[2]*avLM + resid_O2_cold))

o2_tbl3_combo <- rbind(o2_tbl3_f, o2_tbl3_m)
o2_tbl3_combo_adj <- melt.data.table(o2_tbl3_combo[,c("type", "neutral", "cold")], id.vars = c("type"))
o2_tbl3_combo_adj[, sex := "male"]
o2_tbl3_combo_adj[type == mylvls[1] | type == mylvls[2], sex := "female"]



compare <- list(c(mylvls[1], mylvls[2]), c(mylvls[3], mylvls[4]))
min_val <- min(o2_tbl3_combo_adj$value)
max_val <- max(o2_tbl3_combo_adj$value)


adj_o2_mean_plt0 <- 
ggplot(o2_tbl3_combo_adj, aes(x = type, y = value, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(shape = 17, width = 0.1) +
	facet_wrap(.~variable) +
	stat_compare_means(comparisons = compare, size = 3, method = "t.test") +
	ylab(expression(atop("Lean mass adjusted","mean"~"O"[2]*" cons. [mL/min]"))) +
	ylim(min_val, max_val*1.2) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white"))

g <- ggplot_gtable(ggplot_build(adj_o2_mean_plt0))
striprt <- which( grepl('strip-r', g$layout$name) | grepl('strip-t', g$layout$name))
fills <- c("white","lightgrey")
k <- 1
for (i in striprt) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
adj_o2_mean_plt <- ggdraw(g)


cold_chall_O2 <- 
ggarrange(o2_raw_plt, adj_o2_mean_plt, widths = c(1, 1))












####################################################################
# RER


colnames(master3)
# create table to calculate means and sds per time point (not per mouse)
rer_tbl <- master3[,c(7, 153:215)]
colnames(rer_tbl)
setnames(rer_tbl, c("type", seq(0, by = 1/3, length.out = (length(rer_tbl)-1))))
rer_tbl2 <- melt.data.table(rer_tbl) %>% group_by(type, variable) %>% summarize(mean = mean(value), sd = sd(value))
rer_tbl2 <- data.table(rer_tbl2)
rer_tbl2$variable <- as.numeric(as.character(rer_tbl2$variable))
rer_tbl2[, sex := ifelse(type == mylvls[1] | type == mylvls[2], "female", "male")]



# plot variables
# indicate starting point of cold exposure
start_cold <- 11+1/3
light_cycle <- c(5, 17)
lights_off <- light_cycle[1]
lights_on <- light_cycle[2]
top_value <- max(rer_tbl2$mean)+max(rer_tbl2$sd)
min_value <- min(rer_tbl2$mean)-max(rer_tbl2$sd)

rer_raw_plt <-
ggplot(rer_tbl2, aes(x = variable, y = mean, color = type)) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(rer_tbl2$variable), ymin = top_value*1.04, ymax = top_value*1.07, fill = "lightgrey", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, max(rer_tbl2$variable))), y = mean(c(top_value*1.04, top_value*1.07)), color = "black", label = "cold", size = 3) +
	annotate(geom = "rect", xmin = min(rer_tbl2$variable), xmax = start_cold, ymin = top_value*1.04, ymax = top_value*1.07, fill = "white", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, min(rer_tbl2$variable))), y = mean(c(top_value*1.04, top_value*1.07)), color = "black", label = "neutral", size = 3) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(rer_tbl2$variable), ymin = min_value, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
	geom_point(shape = 17, size = 1) +
	annotate(geom = "segment", x = lights_on, xend = lights_on, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "segment", x = lights_off, xend = lights_off, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "text", x = lights_on, y = top_value*1.02, label = "on", angle = 90, size = 3) +
	annotate(geom = "text", x = lights_off, y = top_value*1.02, label = "off", angle = 90, size = 3) +
	ylab(expression("RER"~"[VCO"[2]/VO[2]*"]")) +
	xlab("Time [h]") +
	facet_wrap(.~sex) +
	scale_color_manual(values = mypal) +
	theme_pubr() +
	theme(legend.title = element_blank(), legend.position = "none", strip.background = element_blank())




# create table to calculate means per animal (grouped by neutral and cold phase)
rer_tbl_neutral <- master3[,c(153:(153+32))]
rer_tbl_neutral1 <- rer_tbl_neutral %>% rowwise() %>% summarize(neutral = mean(c_across(where(is.numeric))))

rer_tbl_cold <- master3[,c((153+32):215)]
rer_tbl_cold1 <- rer_tbl_cold %>% rowwise() %>% summarize(cold = mean(c_across(where(is.numeric))))

rer_tbl3 <- cbind(rer_tbl_neutral1, rer_tbl_cold1, master3$typeReal)
setnames(rer_tbl3, c("neutral", "cold", "type"))
rer_tbl3 <- data.table(rer_tbl3)
rer_tbl3_melt <- melt.data.table(rer_tbl3, id.vars = "type")



#######
# don't correct RER (as not body mass dependent)


compare <- list(c(mylvls[1], mylvls[2]), c(mylvls[3], mylvls[4]))
min_val <- min(rer_tbl3_melt$value)
max_val <- max(rer_tbl3_melt$value)


adj_rer_mean_plt0 <- 
ggplot(rer_tbl3_melt, aes(x = type, y = value, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(shape = 17, width = 0.1) +
	facet_wrap(.~variable) +
	stat_compare_means(comparisons = compare, size = 3, method = "t.test") +
	ylab(expression("Mean RER"~"[VCO"[2]/VO[2]*"]")) +
	ylim(min_val, max_val*1.04) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white"))

g <- ggplot_gtable(ggplot_build(adj_rer_mean_plt0))
striprt <- which( grepl('strip-r', g$layout$name) | grepl('strip-t', g$layout$name))
fills <- c("white","lightgrey")
k <- 1
for (i in striprt) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
adj_rer_mean_plt <- ggdraw(g)


cold_chall_rer <- 
ggarrange(rer_raw_plt, adj_rer_mean_plt, widths = c(1, 1))








####################################################################
# LIPID OXIDATION


colnames(master3)
# create table to calculate means and sds per time point (not per mouse)
lox_tbl <- master3[,c(7, 847:909)]
colnames(lox_tbl)
setnames(lox_tbl, c("type", seq(0, by = 1/3, length.out = (length(lox_tbl)-1))))
lox_tbl2 <- melt.data.table(lox_tbl) %>% group_by(type, variable) %>% summarize(mean = mean(value), sd = sd(value))
lox_tbl2 <- data.table(lox_tbl2)
lox_tbl2$variable <- as.numeric(as.character(lox_tbl2$variable))
lox_tbl2[, sex := ifelse(type == mylvls[1] | type == mylvls[2], "female", "male")]



# plot variables
# indicate starting point of cold exposure
start_cold <- 11+1/3
light_cycle <- c(5, 17)
lights_off <- light_cycle[1]
lights_on <- light_cycle[2]
top_value <- max(lox_tbl2$mean)+max(lox_tbl2$sd)


lox_raw_plt <- 
ggplot(lox_tbl2, aes(x = variable, y = mean, color = type)) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(lox_tbl2$variable), ymin = top_value*1.2, ymax = top_value*1.35, fill = "lightgrey", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, max(lox_tbl2$variable))), y = mean(c(top_value*1.2, top_value*1.35)), color = "black", label = "cold", size = 3) +
	annotate(geom = "rect", xmin = min(lox_tbl2$variable), xmax = start_cold, ymin = top_value*1.2, ymax = top_value*1.35, fill = "white", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, min(lox_tbl2$variable))), y = mean(c(top_value*1.2, top_value*1.35)), color = "black", label = "neutral", size = 3) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(lox_tbl2$variable), ymin = 0, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
	geom_point(shape = 17, size = 1) +
	annotate(geom = "segment", x = lights_on, xend = lights_on, y = 0, yend = top_value, linetype = "dashed") +
	annotate(geom = "segment", x = lights_off, xend = lights_off, y = 0, yend = top_value, linetype = "dashed") +
	annotate(geom = "text", x = lights_on, y = top_value*1.1, label = "on", angle = 90, size = 3) +
	annotate(geom = "text", x = lights_off, y = top_value*1.1, label = "off", angle = 90, size = 3) +
	ylab("Lipid oxidation [mL/min]") +
	xlab("Time [h]") +
	facet_wrap(.~sex) +
	scale_color_manual(values = mypal) +
	theme_pubr() +
	theme(legend.title = element_blank(), legend.position = "none", strip.background = element_blank())




# create table to calculate means per animal (grouped by neutral and cold phase)
lox_tbl_neutral <- master3[,c(847:(847+32))]
lox_tbl_neutral1 <- lox_tbl_neutral %>% rowwise() %>% summarize(mean_neutral = mean(c_across(where(is.numeric))))

lox_tbl_cold <- master3[,c((847+32):909)]
lox_tbl_cold1 <- lox_tbl_cold %>% rowwise() %>% summarize(mean_cold = mean(c_across(where(is.numeric))))

lox_tbl3 <- cbind(lox_tbl_neutral1, lox_tbl_cold1, master3$leanMass, master3$typeReal)
setnames(lox_tbl3, c("mean_neutral", "mean_cold", "leanMass", "type"))
lox_tbl3 <- data.table(lox_tbl3)

ggplot(lox_tbl3, aes(x = leanMass, y = mean_neutral, color = type)) +
	geom_point() +
	geom_smooth(method = "lm")



#######
# correct lipid oxidation with lean mass

# split the sexes
lox_tbl3_f <- lox_tbl3[type == mylvls[1] | type == mylvls[2], ]
lox_tbl3_m <- lox_tbl3[type != mylvls[1] & type != mylvls[2], ]

# average lean mass
lox_tbl3_f$avLM <- mean(lox_tbl3$leanMass, na.rm = TRUE)
lox_tbl3_m$avLM <- mean(lox_tbl3$leanMass, na.rm = TRUE)

# linear model for lipid oxidation
mod_neutral_f <- lm(mean_neutral ~ leanMass + type, data = lox_tbl3_f)
mod_neutral_m <- lm(mean_neutral ~ leanMass + type, data = lox_tbl3_m)
summary(mod_neutral_f)
summary(mod_neutral_m)
mod_cold_f <- lm(mean_cold ~ leanMass + type, data = lox_tbl3_f)
mod_cold_m <- lm(mean_cold ~ leanMass + type, data = lox_tbl3_m)
summary(mod_cold_f)
summary(mod_cold_m)

# add residuals to data.table
lox_tbl3_f$resid_LOX_neutral <- resid(mod_neutral_f)
lox_tbl3_m$resid_LOX_neutral <- resid(mod_neutral_m)
lox_tbl3_f$resid_LOX_cold <- resid(mod_cold_f)
lox_tbl3_m$resid_LOX_cold <- resid(mod_cold_m)

# calculate ajdusted lean mass
lox_tbl3_f <- lox_tbl3_f %>%
  mutate(neutral = case_when(
    type==mylvls[1]~ coef(mod_neutral_f)[1] + coef(mod_neutral_f)[2]*avLM + resid_LOX_neutral,
    type==mylvls[2]~ (coef(mod_neutral_f)[1]+ coef(mod_neutral_f)[3]) + coef(mod_neutral_f)[2]*avLM + resid_LOX_neutral),
  	cold = case_when(
    type==mylvls[1]~ coef(mod_cold_f)[1] + coef(mod_cold_f)[2]*avLM + resid_LOX_cold,
    type==mylvls[2]~ (coef(mod_cold_f)[1]+ coef(mod_cold_f)[3]) + coef(mod_cold_f)[2]*avLM + resid_LOX_cold))

lox_tbl3_m <- lox_tbl3_m %>%
  mutate(neutral = case_when(
    type==mylvls[3]~ coef(mod_neutral_m)[1] + coef(mod_neutral_m)[2]*avLM + resid_LOX_neutral,
    type==mylvls[4]~ (coef(mod_neutral_m)[1]+ coef(mod_neutral_m)[3]) + coef(mod_neutral_m)[2]*avLM + resid_LOX_neutral),
  	cold = case_when(
    type==mylvls[3]~ coef(mod_cold_m)[1] + coef(mod_cold_m)[2]*avLM + resid_LOX_cold,
    type==mylvls[4]~ (coef(mod_cold_m)[1]+ coef(mod_cold_m)[3]) + coef(mod_cold_m)[2]*avLM + resid_LOX_cold))

lox_tbl3_combo <- rbind(lox_tbl3_f, lox_tbl3_m)
lox_tbl3_combo_adj <- melt.data.table(lox_tbl3_combo[,c("type", "neutral", "cold")], id.vars = c("type"))
lox_tbl3_combo_adj[, sex := "male"]
lox_tbl3_combo_adj[type == mylvls[1] | type == mylvls[2], sex := "female"]



compare <- list(c(mylvls[1], mylvls[2]), c(mylvls[3], mylvls[4]))
min_val <- min(lox_tbl3_combo_adj$value)
max_val <- max(lox_tbl3_combo_adj$value)


adj_lox_mean_plt0 <- 
ggplot(lox_tbl3_combo_adj, aes(x = type, y = value, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(shape = 17, width = 0.1) +
	facet_wrap(.~variable) +
	stat_compare_means(comparisons = compare, size = 3, method = "t.test") +
	ylab(expression(atop("Lean mass adjusted","lipid oxidation [mL/min]"))) +
	ylim(min_val, max_val*1.2) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white"))

g <- ggplot_gtable(ggplot_build(adj_lox_mean_plt0))
striprt <- which( grepl('strip-r', g$layout$name) | grepl('strip-t', g$layout$name))
fills <- c("white","lightgrey")
k <- 1
for (i in striprt) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
adj_lox_mean_plt <- ggdraw(g)


cold_chall_lox <- 
ggarrange(lox_raw_plt, adj_lox_mean_plt, widths = c(1, 1))










####################################################################
# CARBOHYDRATE OXIDATION


colnames(master3)
# create table to calculate means and sds per time point (not per mouse)
cho_tbl <- master3[,c(7, 910:972)]
colnames(cho_tbl)
setnames(cho_tbl, c("type", seq(0, by = 1/3, length.out = (length(cho_tbl)-1))))
cho_tbl2 <- melt.data.table(cho_tbl) %>% group_by(type, variable) %>% summarize(mean = mean(value), sd = sd(value))
cho_tbl2 <- data.table(cho_tbl2)
cho_tbl2$variable <- as.numeric(as.character(cho_tbl2$variable))
cho_tbl2[, sex := ifelse(type == mylvls[1] | type == mylvls[2], "female", "male")]



# plot variables
# indicate starting point of cold exposure
start_cold <- 11+1/3
light_cycle <- c(5, 17)
lights_off <- light_cycle[1]
lights_on <- light_cycle[2]
top_value <- max(cho_tbl2$mean)+max(cho_tbl2$sd)


cho_raw_plt <- 
ggplot(cho_tbl2, aes(x = variable, y = mean, color = type)) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(cho_tbl2$variable), ymin = top_value*1.2, ymax = top_value*1.35, fill = "lightgrey", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, max(cho_tbl2$variable))), y = mean(c(top_value*1.2, top_value*1.35)), color = "black", label = "cold", size = 3) +
	annotate(geom = "rect", xmin = min(cho_tbl2$variable), xmax = start_cold, ymin = top_value*1.2, ymax = top_value*1.35, fill = "white", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, min(cho_tbl2$variable))), y = mean(c(top_value*1.2, top_value*1.35)), color = "black", label = "neutral", size = 3) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(cho_tbl2$variable), ymin = 0, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
	geom_point(shape = 17, size = 1) +
	annotate(geom = "segment", x = lights_on, xend = lights_on, y = 0, yend = top_value, linetype = "dashed") +
	annotate(geom = "segment", x = lights_off, xend = lights_off, y = 0, yend = top_value, linetype = "dashed") +
	annotate(geom = "text", x = lights_on, y = top_value*1.1, label = "on", angle = 90, size = 3) +
	annotate(geom = "text", x = lights_off, y = top_value*1.1, label = "off", angle = 90, size = 3) +
	ylab("CHO oxidation [mL/min]") +
	xlab("Time [h]") +
	facet_wrap(.~sex) +
	scale_color_manual(values = mypal) +
	theme_pubr() +
	theme(legend.title = element_blank(), legend.position = "none", strip.background = element_blank())




# create table to calculate means per animal (grouped by neutral and cold phase)
cho_tbl_neutral <- master3[,c(910:(910+32))]
cho_tbl_neutral1 <- cho_tbl_neutral %>% rowwise() %>% summarize(mean_neutral = mean(c_across(where(is.numeric))))

cho_tbl_cold <- master3[,c((910+32):972)]
cho_tbl_cold1 <- cho_tbl_cold %>% rowwise() %>% summarize(mean_cold = mean(c_across(where(is.numeric))))

cho_tbl3 <- cbind(cho_tbl_neutral1, cho_tbl_cold1, master3$leanMass, master3$typeReal)
setnames(cho_tbl3, c("mean_neutral", "mean_cold", "leanMass", "type"))
cho_tbl3 <- data.table(cho_tbl3)

ggplot(cho_tbl3, aes(x = leanMass, y = mean_neutral, color = type)) +
	geom_point() +
	geom_smooth(method = "lm")



#######
# correct CHO oxidation with lean mass

# split the sexes
cho_tbl3_f <- cho_tbl3[type == mylvls[1] | type == mylvls[2], ]
cho_tbl3_m <- cho_tbl3[type != mylvls[1] & type != mylvls[2], ]

# average lean mass
cho_tbl3_f$avLM <- mean(cho_tbl3$leanMass, na.rm = TRUE)
cho_tbl3_m$avLM <- mean(cho_tbl3$leanMass, na.rm = TRUE)

# linear model for CHO oxidation
mod_neutral_f <- lm(mean_neutral ~ leanMass + type, data = cho_tbl3_f)
mod_neutral_m <- lm(mean_neutral ~ leanMass + type, data = cho_tbl3_m)
summary(mod_neutral_f)
summary(mod_neutral_m)
mod_cold_f <- lm(mean_cold ~ leanMass + type, data = cho_tbl3_f)
mod_cold_m <- lm(mean_cold ~ leanMass + type, data = cho_tbl3_m)
summary(mod_cold_f)
summary(mod_cold_m)

# add residuals to data.table
cho_tbl3_f$resid_CHO_neutral <- resid(mod_neutral_f)
cho_tbl3_m$resid_CHO_neutral <- resid(mod_neutral_m)
cho_tbl3_f$resid_CHO_cold <- resid(mod_cold_f)
cho_tbl3_m$resid_CHO_cold <- resid(mod_cold_m)

# calculate ajdusted lean mass
cho_tbl3_f <- cho_tbl3_f %>%
  mutate(neutral = case_when(
    type==mylvls[1]~ coef(mod_neutral_f)[1] + coef(mod_neutral_f)[2]*avLM + resid_CHO_neutral,
    type==mylvls[2]~ (coef(mod_neutral_f)[1]+ coef(mod_neutral_f)[3]) + coef(mod_neutral_f)[2]*avLM + resid_CHO_neutral),
  	cold = case_when(
    type==mylvls[1]~ coef(mod_cold_f)[1] + coef(mod_cold_f)[2]*avLM + resid_CHO_cold,
    type==mylvls[2]~ (coef(mod_cold_f)[1]+ coef(mod_cold_f)[3]) + coef(mod_cold_f)[2]*avLM + resid_CHO_cold))

cho_tbl3_m <- cho_tbl3_m %>%
  mutate(neutral = case_when(
    type==mylvls[3]~ coef(mod_neutral_m)[1] + coef(mod_neutral_m)[2]*avLM + resid_CHO_neutral,
    type==mylvls[4]~ (coef(mod_neutral_m)[1]+ coef(mod_neutral_m)[3]) + coef(mod_neutral_m)[2]*avLM + resid_CHO_neutral),
  	cold = case_when(
    type==mylvls[3]~ coef(mod_cold_m)[1] + coef(mod_cold_m)[2]*avLM + resid_CHO_cold,
    type==mylvls[4]~ (coef(mod_cold_m)[1]+ coef(mod_cold_m)[3]) + coef(mod_cold_m)[2]*avLM + resid_CHO_cold))

cho_tbl3_combo <- rbind(cho_tbl3_f, cho_tbl3_m)
cho_tbl3_combo_adj <- melt.data.table(cho_tbl3_combo[,c("type", "neutral", "cold")], id.vars = c("type"))
cho_tbl3_combo_adj[, sex := "male"]
cho_tbl3_combo_adj[type == mylvls[1] | type == mylvls[2], sex := "female"]



compare <- list(c(mylvls[1], mylvls[2]), c(mylvls[3], mylvls[4]))
min_val <- min(cho_tbl3_combo_adj$value)
max_val <- max(cho_tbl3_combo_adj$value)


adj_cho_mean_plt0 <- 
ggplot(cho_tbl3_combo_adj, aes(x = type, y = value, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(shape = 17, width = 0.1) +
	facet_wrap(.~variable) +
	stat_compare_means(comparisons = compare, size = 3, method = "t.test") +
	ylab(expression(atop("Lean mass adjusted","CHO oxidation [mL/min]"))) +
	ylim(min_val, max_val*1.2) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white"))

g <- ggplot_gtable(ggplot_build(adj_cho_mean_plt0))
striprt <- which( grepl('strip-r', g$layout$name) | grepl('strip-t', g$layout$name))
fills <- c("white","lightgrey")
k <- 1
for (i in striprt) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
adj_cho_mean_plt <- ggdraw(g)


cold_chall_cho <- 
ggarrange(cho_raw_plt, adj_cho_mean_plt, widths = c(1, 1))












####################################################################
# HEAT PRODUCTION


colnames(master3)
# create table to calculate means and sds per time point (not per mouse)
hp_tbl <- master3[,c(7, 783:845)]
colnames(hp_tbl)
setnames(hp_tbl, c("type", seq(0, by = 1/3, length.out = (length(hp_tbl)-1))))
hp_tbl2 <- melt.data.table(hp_tbl) %>% group_by(type, variable) %>% summarize(mean = mean(value), sd = sd(value))
hp_tbl2 <- data.table(hp_tbl2)
hp_tbl2$variable <- as.numeric(as.character(hp_tbl2$variable))
hp_tbl2[, sex := ifelse(type == mylvls[1] | type == mylvls[2], "female", "male")]



# plot variables
# indicate starting point of cold exposure
start_cold <- 11+1/3
light_cycle <- c(5, 17)
lights_off <- light_cycle[1]
lights_on <- light_cycle[2]
top_value <- max(hp_tbl2$mean)+max(hp_tbl2$sd)


hp_raw_plt <- 
ggplot(hp_tbl2, aes(x = variable, y = mean, color = type)) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(hp_tbl2$variable), ymin = top_value*1.2, ymax = top_value*1.35, fill = "lightgrey", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, max(hp_tbl2$variable))), y = mean(c(top_value*1.2, top_value*1.35)), color = "black", label = "cold", size = 3) +
	annotate(geom = "rect", xmin = min(hp_tbl2$variable), xmax = start_cold, ymin = top_value*1.2, ymax = top_value*1.35, fill = "white", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, min(hp_tbl2$variable))), y = mean(c(top_value*1.2, top_value*1.35)), color = "black", label = "neutral", size = 3) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(hp_tbl2$variable), ymin = 0, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
	geom_point(shape = 17, size = 1) +
	annotate(geom = "segment", x = lights_on, xend = lights_on, y = 0, yend = top_value, linetype = "dashed") +
	annotate(geom = "segment", x = lights_off, xend = lights_off, y = 0, yend = top_value, linetype = "dashed") +
	annotate(geom = "text", x = lights_on, y = top_value*1.1, label = "on", angle = 90, size = 3) +
	annotate(geom = "text", x = lights_off, y = top_value*1.1, label = "off", angle = 90, size = 3) +
	ylab("Heat production [kJ/h]") +
	xlab("Time [h]") +
	facet_wrap(.~sex) +
	scale_color_manual(values = mypal) +
	theme_pubr() +
	theme(legend.title = element_blank(), legend.position = "none", strip.background = element_blank())




# create table to calculate means per animal (grouped by neutral and cold phase)
hp_tbl_neutral <- master3[,c(783:(783+32))]
hp_tbl_neutral1 <- hp_tbl_neutral %>% rowwise() %>% summarize(mean_neutral = mean(c_across(where(is.numeric))))

hp_tbl_cold <- master3[,c((783+32):845)]
hp_tbl_cold1 <- hp_tbl_cold %>% rowwise() %>% summarize(mean_cold = mean(c_across(where(is.numeric))))

hp_tbl3 <- cbind(hp_tbl_neutral1, hp_tbl_cold1, master3$leanMass, master3$typeReal)
setnames(hp_tbl3, c("mean_neutral", "mean_cold", "leanMass", "type"))
hp_tbl3 <- data.table(hp_tbl3)

ggplot(hp_tbl3, aes(x = leanMass, y = mean_neutral, color = type)) +
	geom_point() +
	geom_smooth(method = "lm")



#######
# correct heat production with lean mass

# split the sexes
hp_tbl3_f <- hp_tbl3[type == mylvls[1] | type == mylvls[2], ]
hp_tbl3_m <- hp_tbl3[type != mylvls[1] & type != mylvls[2], ]

# average lean mass
hp_tbl3_f$avLM <- mean(hp_tbl3$leanMass, na.rm = TRUE)
hp_tbl3_m$avLM <- mean(hp_tbl3$leanMass, na.rm = TRUE)

# linear model for HP oxidation
mod_neutral_f <- lm(mean_neutral ~ leanMass + type, data = hp_tbl3_f)
mod_neutral_m <- lm(mean_neutral ~ leanMass + type, data = hp_tbl3_m)
summary(mod_neutral_f)
summary(mod_neutral_m)
mod_cold_f <- lm(mean_cold ~ leanMass + type, data = hp_tbl3_f)
mod_cold_m <- lm(mean_cold ~ leanMass + type, data = hp_tbl3_m)
summary(mod_cold_f)
summary(mod_cold_m)

# add residuals to data.table
hp_tbl3_f$resid_HP_neutral <- resid(mod_neutral_f)
hp_tbl3_m$resid_HP_neutral <- resid(mod_neutral_m)
hp_tbl3_f$resid_HP_cold <- resid(mod_cold_f)
hp_tbl3_m$resid_HP_cold <- resid(mod_cold_m)

# calculate ajdusted lean mass
hp_tbl3_f <- hp_tbl3_f %>%
  mutate(neutral = case_when(
    type==mylvls[1]~ coef(mod_neutral_f)[1] + coef(mod_neutral_f)[2]*avLM + resid_HP_neutral,
    type==mylvls[2]~ (coef(mod_neutral_f)[1]+ coef(mod_neutral_f)[3]) + coef(mod_neutral_f)[2]*avLM + resid_HP_neutral),
  	cold = case_when(
    type==mylvls[1]~ coef(mod_cold_f)[1] + coef(mod_cold_f)[2]*avLM + resid_HP_cold,
    type==mylvls[2]~ (coef(mod_cold_f)[1]+ coef(mod_cold_f)[3]) + coef(mod_cold_f)[2]*avLM + resid_HP_cold))

hp_tbl3_m <- hp_tbl3_m %>%
  mutate(neutral = case_when(
    type==mylvls[3]~ coef(mod_neutral_m)[1] + coef(mod_neutral_m)[2]*avLM + resid_HP_neutral,
    type==mylvls[4]~ (coef(mod_neutral_m)[1]+ coef(mod_neutral_m)[3]) + coef(mod_neutral_m)[2]*avLM + resid_HP_neutral),
  	cold = case_when(
    type==mylvls[3]~ coef(mod_cold_m)[1] + coef(mod_cold_m)[2]*avLM + resid_HP_cold,
    type==mylvls[4]~ (coef(mod_cold_m)[1]+ coef(mod_cold_m)[3]) + coef(mod_cold_m)[2]*avLM + resid_HP_cold))

hp_tbl3_combo <- rbind(hp_tbl3_f, hp_tbl3_m)
hp_tbl3_combo_adj <- melt.data.table(hp_tbl3_combo[,c("type", "neutral", "cold")], id.vars = c("type"))
hp_tbl3_combo_adj[, sex := "male"]
hp_tbl3_combo_adj[type == mylvls[1] | type == mylvls[2], sex := "female"]



compare <- list(c(mylvls[1], mylvls[2]), c(mylvls[3], mylvls[4]))
min_val <- min(hp_tbl3_combo_adj$value)
max_val <- max(hp_tbl3_combo_adj$value)


adj_hp_mean_plt0 <- 
ggplot(hp_tbl3_combo_adj, aes(x = type, y = value, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(shape = 17, width = 0.1) +
	facet_wrap(.~variable) +
	stat_compare_means(comparisons = compare, size = 3, method = "t.test") +
	ylab(expression(atop("Lean mass adjusted","Heat production [kJ/h]"))) +
	ylim(min_val, max_val*1.2) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white"))

g <- ggplot_gtable(ggplot_build(adj_hp_mean_plt0))
striprt <- which( grepl('strip-r', g$layout$name) | grepl('strip-t', g$layout$name))
fills <- c("white","lightgrey")
k <- 1
for (i in striprt) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
adj_hp_mean_plt <- ggdraw(g)


cold_chall_hp <- 
ggarrange(hp_raw_plt, adj_hp_mean_plt, widths = c(1, 1))











####################################################################
# CUMULATIVE DISTANCE


colnames(master3)
# create table to calculate means and sds per time point (not per mouse)
dist_tbl <- master3[,c(7, 657:719)]
colnames(dist_tbl)
setnames(dist_tbl, c("type", seq(0, by = 1/3, length.out = (length(dist_tbl)-1))))
dist_tbl2 <- melt.data.table(dist_tbl) %>% group_by(type, variable) %>% summarize(mean = mean(value), sd = sd(value))
dist_tbl2 <- data.table(dist_tbl2)
dist_tbl2$variable <- as.numeric(as.character(dist_tbl2$variable))
dist_tbl2[, sex := ifelse(type == mylvls[1] | type == mylvls[2], "female", "male")]



# plot variables
# indicate starting point of cold exposure
start_cold <- 11+1/3
light_cycle <- c(5, 17)
lights_off <- light_cycle[1]
lights_on <- light_cycle[2]
top_value <- max(dist_tbl2$mean)+max(dist_tbl2$sd)


dist_raw_plt <- 
ggplot(dist_tbl2, aes(x = variable, y = mean, color = type)) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(dist_tbl2$variable), ymin = top_value*1.2, ymax = top_value*1.35, fill = "lightgrey", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, max(dist_tbl2$variable))), y = mean(c(top_value*1.2, top_value*1.35)), color = "black", label = "cold", size = 3) +
	annotate(geom = "rect", xmin = min(dist_tbl2$variable), xmax = start_cold, ymin = top_value*1.2, ymax = top_value*1.35, fill = "white", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, min(dist_tbl2$variable))), y = mean(c(top_value*1.2, top_value*1.35)), color = "black", label = "neutral", size = 3) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(dist_tbl2$variable), ymin = 0, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
	geom_point(shape = 17, size = 1) +
	annotate(geom = "segment", x = lights_on, xend = lights_on, y = 0, yend = top_value, linetype = "dashed") +
	annotate(geom = "segment", x = lights_off, xend = lights_off, y = 0, yend = top_value, linetype = "dashed") +
	annotate(geom = "text", x = lights_on, y = top_value*1.1, label = "on", angle = 90, size = 3) +
	annotate(geom = "text", x = lights_off, y = top_value*1.1, label = "off", angle = 90, size = 3) +
	ylab("Distance [cm]") +
	xlab("Time [h]") +
	facet_wrap(.~sex) +
	scale_color_manual(values = mypal) +
	theme_pubr() +
	theme(legend.title = element_blank(), legend.position = "none", strip.background = element_blank())




# create table to calculate means per animal (grouped by neutral and cold phase)
dist_tbl_neutral <- master3[,c(657:(657+32))]
dist_tbl_neutral1 <- dist_tbl_neutral %>% rowwise() %>% summarize(neutral = mean(c_across(where(is.numeric))))

dist_tbl_cold <- master3[,c((657+32):719)]
dist_tbl_cold1 <- dist_tbl_cold %>% rowwise() %>% summarize(cold = mean(c_across(where(is.numeric))))

dist_tbl3 <- cbind(dist_tbl_neutral1, dist_tbl_cold1, master3$typeReal)
setnames(dist_tbl3, c("neutral", "cold", "type"))
dist_tbl3 <- data.table(dist_tbl3)
dist_tbl3_melt <- melt.data.table(dist_tbl3, id.vars = "type")


rer_tbl_neutral <- master3[,c(153:(153+32))]
rer_tbl_neutral1 <- rer_tbl_neutral %>% rowwise() %>% summarize(neutral = mean(c_across(where(is.numeric))))

rer_tbl_cold <- master3[,c((153+32):215)]
rer_tbl_cold1 <- rer_tbl_cold %>% rowwise() %>% summarize(cold = mean(c_across(where(is.numeric))))

rer_tbl3 <- cbind(rer_tbl_neutral1, rer_tbl_cold1, master3$typeReal)
setnames(rer_tbl3, c("neutral", "cold", "type"))
rer_tbl3 <- data.table(rer_tbl3)
rer_tbl3_melt <- melt.data.table(rer_tbl3, id.vars = "type")




#######
# don't correct DIST


compare <- list(c(mylvls[1], mylvls[2]), c(mylvls[3], mylvls[4]))
min_val <- min(dist_tbl3_melt$value)
max_val <- max(dist_tbl3_melt$value)


adj_dist_mean_plt0 <- 
ggplot(dist_tbl3_melt, aes(x = type, y = value, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(shape = 17, width = 0.1) +
	facet_wrap(.~variable) +
	stat_compare_means(comparisons = compare, size = 3, method = "t.test") +
	ylab("Mean distance [cm]") +
	ylim(min_val, max_val*1.2) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white"))

g <- ggplot_gtable(ggplot_build(adj_dist_mean_plt0))
striprt <- which( grepl('strip-r', g$layout$name) | grepl('strip-t', g$layout$name))
fills <- c("white","lightgrey")
k <- 1
for (i in striprt) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
adj_dist_mean_plt <- as.ggplot(g)


cold_chall_dist <- 
ggarrange(dist_raw_plt, adj_dist_mean_plt, widths = c(1, 1))






everyother_label <- c("a", "", "b", "", "c", "", "d", "", "e", "", "f", "", "g", "", "h", "", "i")


plt3 <- 
plot_grid(o2_raw_plt, adj_o2_mean_plt0, rer_raw_plt, adj_rer_mean_plt0, cho_raw_plt, adj_cho_mean_plt0, lox_raw_plt, adj_lox_mean_plt0, hp_raw_plt, adj_hp_mean_plt0, dist_raw_plt, adj_dist_mean_plt0, align = "vh", axis = "bl", nrow = 3, ncol = 4, labels = everyother_label, rel_widths = c(1.2,1))

# plt3 <- 
# plot_grid(o2_raw_plt, adj_o2_mean_plt, rer_raw_plt, adj_rer_mean_plt, cho_raw_plt, adj_cho_mean_plt, lox_raw_plt, adj_lox_mean_plt, hp_raw_plt, adj_hp_mean_plt, dist_raw_plt, adj_dist_mean_plt, align = "vh", axis = "bl", nrow = 3, ncol = 4, labels = everyother_label)
# plt3 <- 
# ggarrange(cold_chall_O2+margin, cold_chall_rer+margin, cold_chall_lox+margin, cold_chall_cho+margin, cold_chall_hp+margin, cold_chall_dist+margin, nrow = 3, ncol = 2, labels = "auto", common.legend = TRUE)



ggsave(paste(fig_path, "Fig3.png", sep = ""), plt3, device = png(), width = 14, height = 9, bg = "white")
dev.off()

ggsave(paste(fig_path, "Fig3.pdf", sep = ""), plt3, device = "pdf", width = 14, height = 9, bg = "white")
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




# b (now a): O2 consumption 

tbl3b <- data.table(read_excel("Data/Fig3/PF_Fig3_O2consumption.xlsx"))
colnames(tbl3b)

mkoki3b00 <- tbl3b[, c(53:67)]
mkoki3b0 <- mkoki3b00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
mkoki3b <- cbind(data.table(mkoki3b0[, c("mean", "sd")]), 
	type = rep(mylvls[4], length(mkoki3b0$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkoki3b0$mean)))

mkiwt3b00 <- tbl3b[, c(36:52)]
mkiwt3b0 <- mkiwt3b00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
mkiwt3b <- cbind(data.table(mkiwt3b0[, c("mean", "sd")]), 
	type = rep(mylvls[3], length(mkiwt3b0$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkiwt3b0$mean)))

m3b <- rbind(mkiwt3b, mkoki3b)


fkoki3b00 <- tbl3b[, c(19:35)]
fkoki3b0 <- fkoki3b00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
fkoki3b <- cbind(data.table(fkoki3b0[, c("mean", "sd")]), 
	type = rep(mylvls[2], length(fkoki3b0$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkoki3b0$mean)))

fkiwt3b00 <- tbl3b[, c(2:18)]
fkiwt3b0 <- fkiwt3b00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
fkiwt3b <- cbind(data.table(fkiwt3b0[, c("mean", "sd")]), 
	type = rep(mylvls[1], length(fkiwt3b0$mean)),
	timemod = seq(0, by = 1/3, length.out = length(fkiwt3b0$mean)))

f3b <- rbind(fkiwt3b, fkoki3b)


tbl3bmeans <- data.table(read_excel("Data/Fig3/PF_Fig3_O2consumptionMEANS.xlsx"))
colnames(tbl3bmeans)
setnames(tbl3bmeans, c("condition",
	rep(mylvls[1], length(2:18)),
	rep(mylvls[2], length(19:35)),
	rep(mylvls[3], length(36:52)),
	rep(mylvls[4], length(53:67))))
tbl3bmeans <- melt.data.table(tbl3bmeans, id.vars = "condition")
tbl3bmeans <- tbl3bmeans[!is.na(value), ]
setnames(tbl3bmeans, c("condition", "type", "value"))
tbl3bmeans$condition <- factor(tbl3bmeans$condition, levels = c("thermo", "cold"))
levels(tbl3bmeans$condition) <- c("neutral", "cold")




# d (now b): lipid oxidation (based on O2 and CHO oxidation)

tbl3d <- data.table(read_excel("Data/Fig3/PF_Fig3_lipidoxidation.xlsx"))
colnames(tbl3d)

mkoki3d00 <- tbl3d[, c(53:67)]
mkoki3d0 <- mkoki3d00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
mkoki3d <- cbind(data.table(mkoki3d0[, c("mean", "sd")]), 
	type = rep(mylvls[4], length(mkoki3d0$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkoki3d0$mean)))

mkiwt3d00 <- tbl3d[, c(36:52)]
mkiwt3d0 <- mkiwt3d00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
mkiwt3d <- cbind(data.table(mkiwt3d0[, c("mean", "sd")]), 
	type = rep(mylvls[3], length(mkiwt3d0$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkiwt3d0$mean)))

m3d <- rbind(mkiwt3d, mkoki3d)


fkoki3d00 <- tbl3d[, c(19:35)]
fkoki3d0 <- fkoki3d00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
fkoki3d <- cbind(data.table(fkoki3d0[, c("mean", "sd")]), 
	type = rep(mylvls[2], length(fkoki3d0$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkoki3d0$mean)))

fkiwt3d00 <- tbl3d[, c(2:18)]
fkiwt3d0 <- fkiwt3d00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
fkiwt3d <- cbind(data.table(fkiwt3d0[, c("mean", "sd")]), 
	type = rep(mylvls[1], length(fkiwt3d0$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkiwt3d0$mean)))

f3d <- rbind(fkiwt3d, fkoki3d)


tbl3dmeans <- data.table(read_excel("Data/Fig3/PF_Fig3_lipidoxidationMEANS.xlsx"))
colnames(tbl3dmeans)
setnames(tbl3dmeans, c("condition",
	rep(mylvls[1], length(2:18)),
	rep(mylvls[2], length(19:35)),
	rep(mylvls[3], length(36:52)),
	rep(mylvls[4], length(53:67))))
tbl3dmeans <- melt.data.table(tbl3dmeans, id.vars = "condition")
tbl3dmeans <- tbl3dmeans[!is.na(value), ]
setnames(tbl3dmeans, c("condition", "type", "value"))
tbl3dmeans$condition <- factor(tbl3dmeans$condition, levels = c("thermo", "cold"))
levels(tbl3dmeans$condition) <- c("neutral", "cold")



# e (now c): mean heat (average over time for each mouse) / body weight (mean before and after) ratio

tbl3e <- data.table(read_excel("Data/Fig3/PF_Fig3_bw_heat_regression.xlsx"))
tbl3e$bw <- as.numeric(tbl3e$bw)
tbl3e <- tbl3e[!is.na(tbl3e$bw), ]
tbl3e$heat <- as.numeric(tbl3e$heat)
tbl3e <- tbl3e[!is.na(tbl3e$heat), ]
tbl3e$type <- factor(tbl3e$type, levels = mylvls)

tbl3e[, genotype := "ko_ki"]
tbl3e[type == mylvls[1] | type == mylvls[3], genotype := "ki_wt"]
tbl3e[, gender := "male"]
tbl3e[type == mylvls[1] | type == mylvls[2], gender := "female"]


summary(lm(heat ~ bw + genotype, tbl3e))

mean(tbl3e[type == mylvls[1]]$heat, na.rm = TRUE)




# f (now d): mean heat / lean mass

tbl3f <- data.table(read_excel("Data/Fig3/PF_Fig3_lean_heat_regression.xlsx"))
tbl3f$lean <- as.numeric(tbl3f$lean)
tbl3f <- tbl3f[!is.na(tbl3f$lean), ]
tbl3f$heat <- as.numeric(tbl3f$heat)
tbl3f <- tbl3f[!is.na(tbl3f$heat), ]
tbl3f$type <- factor(tbl3f$type, levels = mylvls)

tbl3f[, genotype := "ko_ki"]
tbl3f[type == mylvls[1] | type == mylvls[3], genotype := "ki_wt"]
tbl3f[, gender := "male"]
tbl3f[type == mylvls[1] | type == mylvls[2], gender := "female"]

summary(lm(heat ~ bw + genotype, tbl3f))




# combined table of body weight, heat, lean

tbl3_combo <- data.table(read_excel("Data/Fig3/PF_Fig3_bw_heat_lean_regression.xlsx"))
tbl3_combo$lean <- as.numeric(tbl3_combo$lean)
tbl3_combo <- tbl3_combo[!is.na(tbl3_combo$lean), ]
tbl3_combo$heat <- as.numeric(tbl3_combo$heat)
tbl3_combo <- tbl3_combo[!is.na(tbl3_combo$heat), ]
tbl3_combo$type <- factor(tbl3_combo$type, levels = mylvls)

tbl3_combo[, genotype := "ko_ki"]
tbl3_combo[type == mylvls[1] | type == mylvls[3], genotype := "ki_wt"]
tbl3_combo[, gender := "male"]
tbl3_combo[type == mylvls[1] | type == mylvls[2], gender := "female"]

summary(lm(heat ~ lean + genotype, tbl3_combo))







#############
# add figure fat mass vs mean heat. use fat mass from fig. 1

tbl3x <- data.table(read_excel("Data/Fig3/2ndCohort_heatFatMass.xlsx"))
colnames(tbl3x)
tbl3x$fatMass <- as.numeric(tbl3x$fatMass)
tbl3x$mean_heat <- as.numeric(tbl3x$mean_heat)
tbl3x$type <- factor(tbl3x$type, levels = mylvls)








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


# b (now a): O2 consumption 

top_value <- max(m3b$mean)+max(m3b$sd)

# male

plt3b_m_chart <- 
ggplot(m3b, aes(x = timemod, y = mean, color = type)) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(m3b$timemod), ymin = top_value+30, ymax = top_value+50, fill = "lightgrey", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, max(m3b$timemod))), y = mean(c(top_value+30, top_value+50)), color = "black", label = "cold", size = 3) +
	annotate(geom = "rect", xmin = min(m3b$timemod), xmax = start_cold, ymin = top_value+30, ymax = top_value+50, fill = "white", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, min(m3b$timemod))), y = mean(c(top_value+30, top_value+50)), color = "black", label = "neutral", size = 3) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(m3b$timemod), ymin = 0, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
	geom_point(shape = 17, size = 1) +
	annotate(geom = "segment", x = lights_on, xend = lights_on, y = 0, yend = top_value, linetype = "dashed") +
	annotate(geom = "segment", x = lights_off, xend = lights_off, y = 0, yend = top_value, linetype = "dashed") +
	annotate(geom = "text", x = lights_on, y = top_value+15, label = "on", angle = 90, size = 3) +
	annotate(geom = "text", x = lights_off, y = top_value+15, label = "off", angle = 90, size = 3) +
	ylab(expression("O"[2]*" consumption [mL/min]")) +
	xlab("Time [h]") +
	scale_color_manual(values = mypal[c(3,4)]) +
	theme_pubr() +
	theme(legend.title = element_blank(), legend.position = "none")

compare3b_m <- list(c(mylvls[3], mylvls[4]))
max_val_m <- max(tbl3bmeans[type == mylvls[3] | type == mylvls[4] ,]$value)

plt3b_m_means_prov <-
ggplot(tbl3bmeans[type == mylvls[3] | type == mylvls[4] ,], aes(x = type, y = value, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(shape = 17, width = 0.1) +
	stat_compare_means(comparisons = compare3b_m, label.y = (max_val_m+0.05*max_val_m)) +
	ylab(expression("Mean"~"O"[2]*" cons. [mL/min]")) +
	ylim(0, 160) +
	# ylim(0,(max_val_m+0.15*max_val_m)) +
	facet_wrap(.~condition) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal[c(3,4)]) +
	scale_color_manual(values = mypal[c(3,4)]) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank())

g <- ggplot_gtable(ggplot_build(plt3b_m_means_prov))
stripr <- which(grepl('strip-t', g$layout$name))
fills <- c("white", "lightgrey")
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}

plt3b_m_means <- as.ggplot(g)



plt3b_m <-
ggarrange(plt3b_m_chart, plt3b_m_means, nrow = 2, heights = c(1, 1))


# female

plt3b_f_chart <- 
ggplot(f3b, aes(x = timemod, y = mean, color = type)) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(f3b$timemod), ymin = top_value+30, ymax = top_value+50, fill = "lightgrey", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, max(f3b$timemod))), y = mean(c(top_value+30, top_value+50)), color = "black", label = "cold", size = 3) +
	annotate(geom = "rect", xmin = min(f3b$timemod), xmax = start_cold, ymin = top_value+30, ymax = top_value+50, fill = "white", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, min(f3b$timemod))), y = mean(c(top_value+30, top_value+50)), color = "black", label = "neutral", size = 3) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(f3b$timemod), ymin = 0, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
	geom_point(shape = 17, size = 1) +
	annotate(geom = "segment", x = lights_on, xend = lights_on, y = 0, yend = top_value, linetype = "dashed") +
	annotate(geom = "segment", x = lights_off, xend = lights_off, y = 0, yend = top_value, linetype = "dashed") +
	annotate(geom = "text", x = lights_on, y = top_value+15, label = "on", angle = 90, size = 3) +
	annotate(geom = "text", x = lights_off, y = top_value+15, label = "off", angle = 90, size = 3) +
	ylab(expression("O"[2]*" consumption [mL/min]")) +
	xlab("Time [h]") +
	scale_color_manual(values = mypal[c(1,2)]) +
	theme_pubr() +
	theme(legend.title = element_blank(), legend.position = "none")

compare3b_f <- list(c(mylvls[1], mylvls[2]))
max_val_f <- max(tbl3bmeans[type == mylvls[1] | type == mylvls[2] ,]$value)

plt3b_f_means_prov <- 
ggplot(tbl3bmeans[type == mylvls[1] | type == mylvls[2] ,], aes(x = type, y = value, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(shape = 17, width = 0.1) +
	stat_compare_means(comparisons = compare3b_f, label.y = max_val_f*1.05) +
	ylab(expression("Mean"~"O"[2]*" cons. [mL/min]")) +
	# ylim(0,(max_val_f+0.15*max_val_f)) +
	ylim(0,160) +
	facet_wrap(.~condition) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal[c(1,2)]) +
	scale_color_manual(values = mypal[c(1,2)]) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank())

g <- ggplot_gtable(ggplot_build(plt3b_f_means_prov))
stripr <- which(grepl('strip-t', g$layout$name))
fills <- c("white", "lightgrey")
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}

plt3b_f_means <- as.ggplot(g)


plt3b_f <-
ggarrange(plt3b_f_chart, plt3b_f_means, nrow = 2, heights = c(1, 1))

plt3a <- 
ggarrange(plt3b_f, plt3b_m) + margin






# d (now b): lipid oxidation (based on O2 and CHO oxidation)

top_value <- max(m3d$mean)+max(m3d$sd)

# male

plt3d_m_chart <- 
ggplot(m3d, aes(x = timemod, y = mean, color = type)) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(m3d$timemod), ymin = top_value+0.25, ymax = top_value+0.4, fill = "lightgrey", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, max(m3d$timemod))), y = mean(c(top_value+0.25, top_value+0.4)), color = "black", label = "cold", size = 3) +
	annotate(geom = "rect", xmin = min(m3d$timemod), xmax = start_cold, ymin = top_value+0.25, ymax = top_value+0.4, fill = "white", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, min(m3d$timemod))), y = mean(c(top_value+0.25, top_value+0.4)), color = "black", label = "neutral", size = 3) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(m3d$timemod), ymin = 0, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
	geom_point(shape = 17, size = 1) +
	annotate(geom = "segment", x = lights_on, xend = lights_on, y = 0, yend = top_value, linetype = "dashed") +
	annotate(geom = "segment", x = lights_off, xend = lights_off, y = 0, yend = top_value, linetype = "dashed") +
	annotate(geom = "text", x = lights_on, y = top_value*1.1, label = "on", angle = 90, size = 3) +
	annotate(geom = "text", x = lights_off, y = top_value*1.1, label = "off", angle = 90, size = 3) +
	ylab("Lipid oxidation [mL/min]") +
	xlab("Time [h]") +
	scale_color_manual(values = mypal[c(3,4)]) +
	theme_pubr() +
	theme(legend.title = element_blank(), legend.position = "none")


compare3d_m <- list(c(mylvls[3], mylvls[4]))
max_val_m <- max(tbl3dmeans[type == mylvls[3] | type == mylvls[4] ,]$value)

plt3d_m_means_prov <- 
ggplot(tbl3dmeans[type == mylvls[3] | type == mylvls[4] ,], aes(x = type, y = value, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(shape = 17, width = 0.1) +
	stat_compare_means(comparisons = compare3d_m, label.y = (max_val_m+0.05*max_val_m)) +
	ylab("Mean Lipid ox. [mL/min]") +
	ylim(0, 1.4) +
	facet_wrap(.~condition) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal[c(3,4)]) +
	scale_color_manual(values = mypal[c(3,4)]) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank())

g <- ggplot_gtable(ggplot_build(plt3d_m_means_prov))
stripr <- which(grepl('strip-t', g$layout$name))
fills <- c("white", "lightgrey")
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}

plt3d_m_means <- as.ggplot(g)


plt3d_m <-
ggarrange(plt3d_m_chart, plt3d_m_means, nrow = 2, heights = c(1, 1))



# female

plt3d_f_chart <- 
ggplot(f3d, aes(x = timemod, y = mean, color = type)) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(f3d$timemod), ymin = top_value+0.25, ymax = top_value+0.4, fill = "lightgrey", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, max(f3d$timemod))), y = mean(c(top_value+0.25, top_value+0.4)), color = "black", label = "cold", size = 3) +
	annotate(geom = "rect", xmin = min(f3d$timemod), xmax = start_cold, ymin = top_value+0.25, ymax = top_value+0.4, fill = "white", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, min(f3d$timemod))), y = mean(c(top_value+0.25, top_value+0.4)), color = "black", label = "neutral", size = 3) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(f3d$timemod), ymin = 0, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
	geom_point(shape = 17, size = 1) +
	annotate(geom = "segment", x = lights_on, xend = lights_on, y = 0, yend = top_value, linetype = "dashed") +
	annotate(geom = "segment", x = lights_off, xend = lights_off, y = 0, yend = top_value, linetype = "dashed") +
	annotate(geom = "text", x = lights_on, y = top_value*1.1, label = "on", angle = 90, size = 3) +
	annotate(geom = "text", x = lights_off, y = top_value*1.1, label = "off", angle = 90, size = 3) +
	ylab("Lipid oxidation [mL/min]") +
	xlab("Time [h]") +
	scale_color_manual(values = mypal[c(1,2)]) +
	theme_pubr() +
	theme(legend.title = element_blank(), legend.position = "none")

compare3d_f <- list(c(mylvls[1], mylvls[2]))
max_val_f <- max(tbl3dmeans[type == mylvls[1] | type == mylvls[2] ,]$value)

plt3d_f_means_prov <- 
ggplot(tbl3dmeans[type == mylvls[1] | type == mylvls[2] ,], aes(x = type, y = value, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(shape = 17, width = 0.1) +
	stat_compare_means(comparisons = compare3d_f, label.y = (max_val_f+0.05*max_val_f)) +
	ylab("Mean Lipid ox. [mL/min]") +
	# ylim(0,(max_val_f+0.15*max_val_f)) +
	ylim(0, 1.4) +
	facet_wrap(.~condition) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal[c(1,2)]) +
	scale_color_manual(values = mypal[c(1,2)]) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank())

g <- ggplot_gtable(ggplot_build(plt3d_f_means_prov))
stripr <- which(grepl('strip-t', g$layout$name))
fills <- c("white", "lightgrey")
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}

plt3d_f_means <- as.ggplot(g)


plt3d_f <-
ggarrange(plt3d_f_chart, plt3d_f_means, nrow = 2, heights = c(1, 1))

plt3b <-
ggarrange(plt3d_f, plt3d_m) + margin



# e (now c): mean heat (average over time for each mouse) / body weight (mean before and after) ratio

plt3c <- 
ggplot(tbl3e, aes(x = bw, y = heat, color = type)) +
	geom_point(shape = 17) +
	geom_smooth(method = "lm", se = FALSE) +
 	stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, size = 3, aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")))+
	ylab("Mean heat [kJ/h]") +
	xlab("Mean body weight [g]") +
	theme_pubr() +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.position = "none", legend.title = element_blank())



# f (now d): mean heat / lean mass

plt3d <- 
ggplot(tbl3f, aes(x = lean, y = heat, color = type)) +
	geom_point(shape = 17) +
	geom_smooth(method = "lm", se = FALSE) +
 	stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, size = 3, aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")))+
	ylab("Mean heat [kJ/h]") +
	xlab("Lean mass [g]") +
	theme_pubr() +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.position = "none")



# add figure fat mass vs mean heat. use fat mass from fig. 1, potential figure

plt3x <- 
ggplot(tbl3x, aes(x = fatMass, y = mean_heat, color = type)) +
	geom_point(shape = 17) +
	geom_smooth(method = "lm", se = FALSE) +
 	stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, size = 3, aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")))+
	ylab("Mean heat [kJ/h]") +
	xlab("Fat mass [g]") +
	theme_pubr() +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.position = "none")







plt3 <- 
ggarrange(plt3a, plt3b, plt3c, plt3d, nrow = 2, ncol = 2, heights = c(1, 0.6), labels = c("a", "b", "c", "d"), font.label = list(size = 18), common.legend = TRUE, legend = "bottom")

ggsave(paste(fig_path, "Fig3.png", sep = ""), plt3, device = png(), width = 10, height = 9)
dev.off()




ggsave(paste(fig_path, "Fig3_fatMassHeat.png", sep = ""), plt3x, device = png(), width = 5, height = 4)
dev.off()







