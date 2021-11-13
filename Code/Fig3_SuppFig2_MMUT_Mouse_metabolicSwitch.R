### METABOLIC SWITCH IN A MOUSE MODEL OF METHYLMALONIC ACIDURIA
# FIGURE 3
# + Supp. Fig. 2

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
require(ggplotify)
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

o2_tbl3 <- cbind(o2_tbl_neutral1, o2_tbl_cold1, master3$bodyMass, master3$typeReal)
setnames(o2_tbl3, c("neutral", "cold", "bodyMass", "type"))
o2_tbl3 <- data.table(o2_tbl3)

o2_tbl3_melt <- melt.data.table(o2_tbl3, id.vars = c("type", "bodyMass"))


supp_o2 <- 
ggplot(o2_tbl3_melt, aes(x = bodyMass, y = value, color = type)) +
	geom_point() +
	geom_smooth(method = "lm", se = FALSE) +
	stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, size = 3, aes(label = paste(..p.label.., sep = "~`,`~")), cor.coef.name = "rho")+
	ylab(expression("Mean O"[2]*" consumption [mL/min]")) +
	xlab("Body mass [g]") +
	facet_wrap(.~variable) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme_pubr() +
	theme(strip.background = element_rect("white"))





#######
# correct 02 consumption with body mass

# split the sexes
o2_tbl3_f <- o2_tbl3[type == mylvls[1] | type == mylvls[2], ]
o2_tbl3_m <- o2_tbl3[type != mylvls[1] & type != mylvls[2], ]

# average body mass
o2_tbl3_f$avLM <- mean(o2_tbl3$bodyMass, na.rm = TRUE)
o2_tbl3_m$avLM <- mean(o2_tbl3$bodyMass, na.rm = TRUE)

# linear model for O2 consumption
mod_neutral_f <- lm(neutral ~ bodyMass + type, data = o2_tbl3_f)
mod_neutral_m <- lm(neutral ~ bodyMass + type, data = o2_tbl3_m)
summary(mod_neutral_f)
summary(mod_neutral_m)
mod_cold_f <- lm(cold ~ bodyMass + type, data = o2_tbl3_f)
mod_cold_m <- lm(cold ~ bodyMass + type, data = o2_tbl3_m)
summary(mod_cold_f)
summary(mod_cold_m)

# add residuals to data.table
o2_tbl3_f$resid_O2_neutral <- resid(mod_neutral_f)
o2_tbl3_m$resid_O2_neutral <- resid(mod_neutral_m)
o2_tbl3_f$resid_O2_cold <- resid(mod_cold_f)
o2_tbl3_m$resid_O2_cold <- resid(mod_cold_m)

# calculate ajdusted body mass
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
	ylab(expression(atop("Body mass adjusted","mean"~"O"[2]*" cons. [mL/min]"))) +
	ylim(min_val, max_val*1.2) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white"))


cold_chall_O2 <- 
o2_raw_plt + adj_o2_mean_plt0








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


cold_chall_rer <- 
rer_raw_plt + adj_rer_mean_plt0








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

lox_tbl3 <- cbind(lox_tbl_neutral1, lox_tbl_cold1, master3$bodyMass, master3$typeReal)
setnames(lox_tbl3, c("neutral", "cold", "bodyMass", "type"))
lox_tbl3 <- data.table(lox_tbl3)
lox_tbl3_melt <- melt.data.table(lox_tbl3, id.vars = c("type", "bodyMass"))


supp_lox <- 
ggplot(lox_tbl3_melt, aes(x = bodyMass, y = value, color = type)) +
	geom_point() +
	geom_smooth(method = "lm", se = FALSE) +
	stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, size = 3, aes(label = paste(..p.label.., sep = "~`,`~")), cor.coef.name = "rho")+
	ylab("Mean lipid oxidation [mL/min]") +
	xlab("Body mass [g]") +
	facet_wrap(.~variable) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme_pubr() +
	theme(strip.background = element_rect("white"))



#######
# correct lipid oxidation with body mass

# split the sexes
lox_tbl3_f <- lox_tbl3[type == mylvls[1] | type == mylvls[2], ]
lox_tbl3_m <- lox_tbl3[type != mylvls[1] & type != mylvls[2], ]

# average body mass
lox_tbl3_f$avLM <- mean(lox_tbl3$bodyMass, na.rm = TRUE)
lox_tbl3_m$avLM <- mean(lox_tbl3$bodyMass, na.rm = TRUE)

# linear model for lipid oxidation
mod_neutral_f <- lm(neutral ~ bodyMass + type, data = lox_tbl3_f)
mod_neutral_m <- lm(neutral ~ bodyMass + type, data = lox_tbl3_m)
summary(mod_neutral_f)
summary(mod_neutral_m)
mod_cold_f <- lm(cold ~ bodyMass + type, data = lox_tbl3_f)
mod_cold_m <- lm(cold ~ bodyMass + type, data = lox_tbl3_m)
summary(mod_cold_f)
summary(mod_cold_m)

# add residuals to data.table
lox_tbl3_f$resid_LOX_neutral <- resid(mod_neutral_f)
lox_tbl3_m$resid_LOX_neutral <- resid(mod_neutral_m)
lox_tbl3_f$resid_LOX_cold <- resid(mod_cold_f)
lox_tbl3_m$resid_LOX_cold <- resid(mod_cold_m)

# calculate ajdusted body mass
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
	ylab(expression(atop("Body mass adjusted","lipid oxidation [mL/min]"))) +
	ylim(min_val, max_val*1.2) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white"))


cold_chall_lox <- 
lox_raw_plt + adj_lox_mean_plt0










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

cho_tbl3 <- cbind(cho_tbl_neutral1, cho_tbl_cold1, master3$bodyMass, master3$typeReal)
setnames(cho_tbl3, c("neutral", "cold", "bodyMass", "type"))
cho_tbl3 <- data.table(cho_tbl3)
cho_tbl3_melt <- melt.data.table(cho_tbl3, id.vars = c("type", "bodyMass"))


supp_cho <- 
ggplot(cho_tbl3_melt, aes(x = bodyMass, y = value, color = type)) +
	geom_point() +
	geom_smooth(method = "lm", se = FALSE) +
	stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, size = 3, aes(label = paste(..p.label.., sep = "~`,`~")), cor.coef.name = "rho")+
	ylab("Mean CHO oxidation [mL/min]") +
	xlab("Body mass [g]") +
	facet_wrap(.~variable) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme_pubr() +
	theme(strip.background = element_rect("white"))




#######
# correct CHO oxidation with body mass

# split the sexes
cho_tbl3_f <- cho_tbl3[type == mylvls[1] | type == mylvls[2], ]
cho_tbl3_m <- cho_tbl3[type != mylvls[1] & type != mylvls[2], ]

# average body mass
cho_tbl3_f$avLM <- mean(cho_tbl3$bodyMass, na.rm = TRUE)
cho_tbl3_m$avLM <- mean(cho_tbl3$bodyMass, na.rm = TRUE)

# linear model for CHO oxidation
mod_neutral_f <- lm(neutral ~ bodyMass + type, data = cho_tbl3_f)
mod_neutral_m <- lm(neutral ~ bodyMass + type, data = cho_tbl3_m)
summary(mod_neutral_f)
summary(mod_neutral_m)
mod_cold_f <- lm(cold ~ bodyMass + type, data = cho_tbl3_f)
mod_cold_m <- lm(cold ~ bodyMass + type, data = cho_tbl3_m)
summary(mod_cold_f)
summary(mod_cold_m)

# add residuals to data.table
cho_tbl3_f$resid_CHO_neutral <- resid(mod_neutral_f)
cho_tbl3_m$resid_CHO_neutral <- resid(mod_neutral_m)
cho_tbl3_f$resid_CHO_cold <- resid(mod_cold_f)
cho_tbl3_m$resid_CHO_cold <- resid(mod_cold_m)

# calculate ajdusted body mass
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
	ylab(expression(atop("Body mass adjusted","CHO oxidation [mL/min]"))) +
	ylim(min_val, max_val*1.2) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white"))


cold_chall_cho <- 
cho_raw_plt + adj_cho_mean_plt0












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



cold_chall_dist <- 
dist_raw_plt + adj_dist_mean_plt0








plt3 <- 
ggarrange(cold_chall_O2+margin, cold_chall_rer+margin, cold_chall_cho+margin,cold_chall_lox+margin, cold_chall_dist+margin, nrow = 3, ncol = 2, labels = "auto", common.legend = TRUE, legend = "bottom")



ggsave(paste(fig_path, "Fig3.png", sep = ""), plt3, device = png(), width = 14, height = 9, bg = "white")
dev.off()

ggsave(paste(fig_path, "Fig3.pdf", sep = ""), plt3, device = "pdf", width = 14, height = 9, bg = "white")
dev.off()











####################################################################
####################################################################
# Supp. Fig. 2
####################################################################
####################################################################

# theme & info:
# plots to depict linear models used in Fig. 3 for:
	# O2 consumption
	# lipid oxidation
	# CHO oxidation


####################################################################
# panels are plotted above with the respective Fig. 3 panel


supp_fig2 <- 
ggarrange(supp_o2+margin, supp_lox+margin, supp_cho+margin, common.legend= TRUE, labels = "auto")


ggsave(paste(fig_path, "SuppFig2.png", sep = ""), supp_fig2, device = png(), width = 9, height = 6.5, bg = "white")
dev.off()

ggsave(paste(fig_path, "SuppFig2.pdf", sep = ""), supp_fig2, device = "pdf", width = 9, height = 6.5, bg = "white")
dev.off()


