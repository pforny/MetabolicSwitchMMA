# wd
Dropbox/PF/Post_PhD/Zurich_2019_2022/MMUT_Mouse_metabolicSwitch

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


light_cycle <- c(5, 17)
lights_off <- light_cycle[1]
lights_on <- light_cycle[2]


####################################################################
####################################################################
# FIGURE 4
####################################################################
####################################################################





margs = c(0.5, 0.5, 0.5, 0.5)
margin = theme(plot.margin = unit(margs, "cm"))






# master file

master4 <- data.table(read_excel("Data/Fig4/indirectCalorimetry_summaryFile_PFedition.xlsx"))












####################################################################
# O2 (VO2 in table)


colnames(master4)
# create table to calculate means and sds per time point (not per mouse)
start_o2 <- 18
end_o2 <- 80
o2_tbl <- master4[,c(6, start_o2:end_o2), with = FALSE]
colnames(o2_tbl)
setnames(o2_tbl, c("type", seq(0, by = 1/3, length.out = (length(o2_tbl)-1))))
o2_tbl2 <- melt.data.table(o2_tbl) %>% group_by(type, variable) %>% summarize(mean = mean(value), sd = sd(value))
o2_tbl2 <- data.table(o2_tbl2)
o2_tbl2$variable <- as.numeric(as.character(o2_tbl2$variable))
o2_tbl2[, sex := ifelse(type == mylvls[1] | type == mylvls[2], "female", "male")]



# plot variables
# indicate starting point of cold exposure
light_cycle <- c(5, 17)
lights_off <- light_cycle[1]
lights_on <- light_cycle[2]
top_value <- max(o2_tbl2$mean)+max(o2_tbl2$sd)
min_value <- min(o2_tbl2$mean)-max(o2_tbl2$sd)


o2_raw_plt <- 
ggplot(o2_tbl2, aes(x = variable, y = mean, color = type)) +
	annotate(geom = "rect", xmin = lights_on, xmax = lights_off, ymin = min_value, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
	geom_point(size = 1) +
	annotate(geom = "segment", x = lights_on, xend = lights_on, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "segment", x = lights_off, xend = lights_off, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "text", x = lights_on, y = top_value*1.1, label = "on", angle = 90, size = 3) +
	annotate(geom = "text", x = lights_off, y = top_value*1.1, label = "off", angle = 90, size = 3) +
	ylab(expression("O"[2]*" consumption [mL/min]")) +
	xlab("Time [h]") +
	facet_wrap(.~sex) +
	scale_color_manual(values = mypal) +
	theme_pubr() +
	theme(legend.title = element_blank(), legend.position = "none", strip.background = element_blank())




# create table to calculate means per animal (grouped by three phases)
o2_tbl_early <- master4[,c(start_o2:(start_o2+lights_off*3)), with = FALSE]
o2_tbl_early1 <- o2_tbl_early %>% rowwise() %>% summarize("early_light" = mean(c_across(where(is.numeric))))

o2_tbl_dark <- master4[,c((start_o2+lights_off*3):(start_o2+lights_on*3)), with = FALSE]
o2_tbl_dark1 <- o2_tbl_dark %>% rowwise() %>% summarize(dark = mean(c_across(where(is.numeric))))

o2_tbl_late <- master4[,c((start_o2+lights_on*3):end_o2), with = FALSE]
o2_tbl_late1 <- o2_tbl_late %>% rowwise() %>% summarize("late_light" = mean(c_across(where(is.numeric))))


o2_tbl3 <- cbind(o2_tbl_early1, o2_tbl_dark1, o2_tbl_late1, master4$lean_mass, master4$typeReal)
setnames(o2_tbl3, c("early_light", "dark", "late_light", "leanMass", "type"))
o2_tbl3 <- data.table(o2_tbl3)

ggplot(o2_tbl3, aes(x = leanMass, y = early_light, color = type)) +
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
mod_early_f <- lm(early_light ~ leanMass + type, data = o2_tbl3_f)
mod_early_m <- lm(early_light ~ leanMass + type, data = o2_tbl3_m)
summary(mod_early_f)
summary(mod_early_m)
mod_dark_f <- lm(dark ~ leanMass + type, data = o2_tbl3_f)
mod_dark_m <- lm(dark ~ leanMass + type, data = o2_tbl3_m)
summary(mod_dark_f)
summary(mod_dark_m)
mod_late_f <- lm(late_light ~ leanMass + type, data = o2_tbl3_f)
mod_late_m <- lm(late_light ~ leanMass + type, data = o2_tbl3_m)
summary(mod_late_f)
summary(mod_late_m)

# add residuals to data.table
o2_tbl3_f$resid_O2_early <- resid(mod_early_f)
o2_tbl3_m$resid_O2_early <- resid(mod_early_m)
o2_tbl3_f$resid_O2_dark <- resid(mod_dark_f)
o2_tbl3_m$resid_O2_dark <- resid(mod_dark_m)
o2_tbl3_f$resid_O2_late <- resid(mod_late_f)
o2_tbl3_m$resid_O2_late <- resid(mod_late_m)

# calculate ajdusted values
o2_tbl3_f <- o2_tbl3_f %>%
  mutate(early_light = case_when(
    type==mylvls[1]~ coef(mod_early_f)[1] + coef(mod_early_f)[2]*avLM + resid_O2_early,
    type==mylvls[2]~ (coef(mod_early_f)[1]+ coef(mod_early_f)[3]) + coef(mod_early_f)[2]*avLM + resid_O2_early),
  	dark = case_when(
    type==mylvls[1]~ coef(mod_dark_f)[1] + coef(mod_dark_f)[2]*avLM + resid_O2_dark,
    type==mylvls[2]~ (coef(mod_dark_f)[1]+ coef(mod_dark_f)[3]) + coef(mod_dark_f)[2]*avLM + resid_O2_dark),
    late_light = case_when(
    type==mylvls[1]~ coef(mod_late_f)[1] + coef(mod_late_f)[2]*avLM + resid_O2_late,
    type==mylvls[2]~ (coef(mod_late_f)[1]+ coef(mod_late_f)[3]) + coef(mod_late_f)[2]*avLM + resid_O2_late))

o2_tbl3_m <- o2_tbl3_m %>%
  mutate(early_light = case_when(
    type==mylvls[3]~ coef(mod_early_m)[1] + coef(mod_early_m)[2]*avLM + resid_O2_early,
    type==mylvls[4]~ (coef(mod_early_m)[1]+ coef(mod_early_m)[3]) + coef(mod_early_m)[2]*avLM + resid_O2_early),
  	dark = case_when(
    type==mylvls[3]~ coef(mod_dark_m)[1] + coef(mod_dark_m)[2]*avLM + resid_O2_dark,
    type==mylvls[4]~ (coef(mod_dark_m)[1]+ coef(mod_dark_m)[3]) + coef(mod_dark_m)[2]*avLM + resid_O2_dark),
    late_light = case_when(
    type==mylvls[3]~ coef(mod_late_m)[1] + coef(mod_late_m)[2]*avLM + resid_O2_late,
    type==mylvls[4]~ (coef(mod_late_m)[1]+ coef(mod_late_m)[3]) + coef(mod_late_m)[2]*avLM + resid_O2_late))

o2_tbl3_combo <- rbind(o2_tbl3_f, o2_tbl3_m)
o2_tbl3_combo_adj <- melt.data.table(o2_tbl3_combo[,c("type", "early_light", "dark", "late_light")], id.vars = c("type"))
o2_tbl3_combo_adj[, sex := "male"]
o2_tbl3_combo_adj[type == mylvls[1] | type == mylvls[2], sex := "female"]



compare <- list(c(mylvls[1], mylvls[2]), c(mylvls[3], mylvls[4]))
min_val <- min(o2_tbl3_combo_adj$value)
max_val <- max(o2_tbl3_combo_adj$value)


adj_o2_mean_plt <- 
ggplot(o2_tbl3_combo_adj, aes(x = type, y = value, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(width = 0.1) +
	facet_wrap(.~variable) +
	stat_compare_means(comparisons = compare, size = 3, method = "t.test") +
	ylab(expression(atop("Lean mass adjusted","mean"~"O"[2]*" cons. [mL/min]"))) +
	ylim(min_val, max_val*1.2) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white"))

ind_calor_O2 <- 
ggarrange(o2_raw_plt, adj_o2_mean_plt, widths = c(1, 1))









####################################################################
# FOOD INTAKE


colnames(master4)
# create table to calculate means and sds per time point (not per mouse)
start_food <- 900
end_food <- 962
food_tbl <- master4[,c(2, 6, start_food:end_food), with = FALSE]
colnames(food_tbl)
setnames(food_tbl, c("id", "type", seq(0, by = 1/3, length.out = (length(food_tbl)-2))))

food_tbl1 <- melt.data.table(food_tbl, id.vars = c("id", "type")) %>% group_by(id, type) %>% summarize(food_cumu = cumsum(value))
food_tbl1 <- data.table(food_tbl1)
food_tbl1[, variable := rep(colnames(food_tbl)[3:length(colnames(food_tbl))], length(unique(food_tbl1$id)))]

food_tbl2 <- food_tbl1 %>% group_by(type, variable) %>% summarize(mean_cumu = mean(food_cumu), sd_cumu = sd(food_cumu))
food_tbl2 <- data.table(food_tbl2)
food_tbl2$variable <- as.numeric(as.character(food_tbl2$variable))
food_tbl2[, sex := ifelse(type == mylvls[1] | type == mylvls[2], "female", "male")]



# plot variables
# indicate starting point of cold exposure
light_cycle <- c(5, 17)
lights_off <- light_cycle[1]
lights_on <- light_cycle[2]
top_value <- max(food_tbl2$mean_cumu)+max(food_tbl2$sd_cumu)
min_value <- min(food_tbl2$mean_cumu)-max(food_tbl2$sd_cumu)


food_raw_plt <- 
ggplot(food_tbl2, aes(x = variable, y = mean_cumu, color = type)) +
	annotate(geom = "rect", xmin = lights_on, xmax = lights_off, ymin = min_value, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean_cumu-sd_cumu, ymax = mean_cumu+sd_cumu), alpha = 0.6) +
	geom_point(size = 1) +
	annotate(geom = "segment", x = lights_on, xend = lights_on, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "segment", x = lights_off, xend = lights_off, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "text", x = lights_on, y = top_value*1.1, label = "on", angle = 90, size = 3) +
	annotate(geom = "text", x = lights_off, y = top_value*1.1, label = "off", angle = 90, size = 3) +
	ylab("Cumulative food intake [g]") +
	xlab("Time [h]") +
	facet_wrap(.~sex) +
	scale_color_manual(values = mypal) +
	theme_pubr() +
	theme(legend.title = element_blank(), legend.position = "none", strip.background = element_blank())




# create table to calculate means per animal (grouped by three phases)
food_tbl_early <- master4[,c(start_food:(start_food+lights_off*3)), with = FALSE]
food_tbl_early1 <- food_tbl_early %>% rowwise() %>% summarize("early_light" = mean(c_across(where(is.numeric))))

food_tbl_dark <- master4[,c((start_food+lights_off*3):(start_food+lights_on*3)), with = FALSE]
food_tbl_dark1 <- food_tbl_dark %>% rowwise() %>% summarize(dark = mean(c_across(where(is.numeric))))

food_tbl_late <- master4[,c((start_food+lights_on*3):end_food), with = FALSE]
food_tbl_late1 <- food_tbl_late %>% rowwise() %>% summarize("late_light" = mean(c_across(where(is.numeric))))


food_tbl3 <- cbind(food_tbl_early1, food_tbl_dark1, food_tbl_late1, master4$lean_mass, master4$typeReal)
setnames(food_tbl3, c("early_light", "dark", "late_light", "leanMass", "type"))
food_tbl3 <- data.table(food_tbl3)

ggplot(food_tbl3, aes(x = leanMass, y = dark, color = type)) +
	geom_point() +
	geom_smooth(method = "lm")



#######
# correct food intake with lean mass

# split the sexes
food_tbl3_f <- food_tbl3[type == mylvls[1] | type == mylvls[2], ]
food_tbl3_m <- food_tbl3[type != mylvls[1] & type != mylvls[2], ]

# average lean mass
food_tbl3_f$avLM <- mean(food_tbl3$leanMass, na.rm = TRUE)
food_tbl3_m$avLM <- mean(food_tbl3$leanMass, na.rm = TRUE)

# linear model for food intake
mod_early_f <- lm(early_light ~ leanMass + type, data = food_tbl3_f)
mod_early_m <- lm(early_light ~ leanMass + type, data = food_tbl3_m)
summary(mod_early_f)
summary(mod_early_m)
mod_dark_f <- lm(dark ~ leanMass + type, data = food_tbl3_f)
mod_dark_m <- lm(dark ~ leanMass + type, data = food_tbl3_m)
summary(mod_dark_f)
summary(mod_dark_m)
mod_late_f <- lm(late_light ~ leanMass + type, data = food_tbl3_f)
mod_late_m <- lm(late_light ~ leanMass + type, data = food_tbl3_m)
summary(mod_late_f)
summary(mod_late_m)

# add residuals to data.table
food_tbl3_f$resid_FOOD_early <- resid(mod_early_f)
food_tbl3_m$resid_FOOD_early <- resid(mod_early_m)
food_tbl3_f$resid_FOOD_dark <- resid(mod_dark_f)
food_tbl3_m$resid_FOOD_dark <- resid(mod_dark_m)
food_tbl3_f$resid_FOOD_late <- resid(mod_late_f)
food_tbl3_m$resid_FOOD_late <- resid(mod_late_m)

# calculate ajdusted values
food_tbl3_f <- food_tbl3_f %>%
  mutate(early_light = case_when(
    type==mylvls[1]~ coef(mod_early_f)[1] + coef(mod_early_f)[2]*avLM + resid_FOOD_early,
    type==mylvls[2]~ (coef(mod_early_f)[1]+ coef(mod_early_f)[3]) + coef(mod_early_f)[2]*avLM + resid_FOOD_early),
  	dark = case_when(
    type==mylvls[1]~ coef(mod_dark_f)[1] + coef(mod_dark_f)[2]*avLM + resid_FOOD_dark,
    type==mylvls[2]~ (coef(mod_dark_f)[1]+ coef(mod_dark_f)[3]) + coef(mod_dark_f)[2]*avLM + resid_FOOD_dark),
    late_light = case_when(
    type==mylvls[1]~ coef(mod_late_f)[1] + coef(mod_late_f)[2]*avLM + resid_FOOD_late,
    type==mylvls[2]~ (coef(mod_late_f)[1]+ coef(mod_late_f)[3]) + coef(mod_late_f)[2]*avLM + resid_FOOD_late))

food_tbl3_m <- food_tbl3_m %>%
  mutate(early_light = case_when(
    type==mylvls[3]~ coef(mod_early_m)[1] + coef(mod_early_m)[2]*avLM + resid_FOOD_early,
    type==mylvls[4]~ (coef(mod_early_m)[1]+ coef(mod_early_m)[3]) + coef(mod_early_m)[2]*avLM + resid_FOOD_early),
  	dark = case_when(
    type==mylvls[3]~ coef(mod_dark_m)[1] + coef(mod_dark_m)[2]*avLM + resid_FOOD_dark,
    type==mylvls[4]~ (coef(mod_dark_m)[1]+ coef(mod_dark_m)[3]) + coef(mod_dark_m)[2]*avLM + resid_FOOD_dark),
    late_light = case_when(
    type==mylvls[3]~ coef(mod_late_m)[1] + coef(mod_late_m)[2]*avLM + resid_FOOD_late,
    type==mylvls[4]~ (coef(mod_late_m)[1]+ coef(mod_late_m)[3]) + coef(mod_late_m)[2]*avLM + resid_FOOD_late))

food_tbl3_combo <- rbind(food_tbl3_f, food_tbl3_m)
food_tbl3_combo_adj <- melt.data.table(food_tbl3_combo[,c("type", "early_light", "dark", "late_light")], id.vars = c("type"))
food_tbl3_combo_adj[, sex := "male"]
food_tbl3_combo_adj[type == mylvls[1] | type == mylvls[2], sex := "female"]



compare <- list(c(mylvls[1], mylvls[2]), c(mylvls[3], mylvls[4]))
min_val <- min(food_tbl3_combo_adj$value)
max_val <- max(food_tbl3_combo_adj$value)


adj_food_mean_plt <- 
ggplot(food_tbl3_combo_adj, aes(x = type, y = value, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(width = 0.1) +
	facet_wrap(.~variable) +
	stat_compare_means(comparisons = compare, size = 3, method = "t.test") +
	ylab(expression(atop("Lean mass adjusted","mean food intake [g]"))) +
	ylim(min_val, max_val*1.3) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white"))

ind_calor_FOOD <- 
ggarrange(food_raw_plt, adj_food_mean_plt, widths = c(1, 1))










####################################################################
# LIPID OXIDATION


colnames(master4)
# create table to calculate means and sds per time point (not per mouse)
start_lipid <- 774
end_lipid <- 836
lipid_tbl <- master4[,c(6, start_lipid:end_lipid), with = FALSE]
colnames(lipid_tbl)
setnames(lipid_tbl, c("type", seq(0, by = 1/3, length.out = (length(lipid_tbl)-1))))
lipid_tbl2 <- melt.data.table(lipid_tbl) %>% group_by(type, variable) %>% summarize(mean = mean(value), sd = sd(value))
lipid_tbl2 <- data.table(lipid_tbl2)
lipid_tbl2$variable <- as.numeric(as.character(lipid_tbl2$variable))
lipid_tbl2[, sex := ifelse(type == mylvls[1] | type == mylvls[2], "female", "male")]



# plot variables
# indicate starting point of cold exposure
light_cycle <- c(5, 17)
lights_off <- light_cycle[1]
lights_on <- light_cycle[2]
top_value <- max(lipid_tbl2$mean)+max(lipid_tbl2$sd)
min_value <- min(lipid_tbl2$mean)-max(lipid_tbl2$sd)


lipid_raw_plt <- 
ggplot(lipid_tbl2, aes(x = variable, y = mean, color = type)) +
	annotate(geom = "rect", xmin = lights_on, xmax = lights_off, ymin = min_value, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
	geom_point(size = 1) +
	annotate(geom = "segment", x = lights_on, xend = lights_on, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "segment", x = lights_off, xend = lights_off, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "text", x = lights_on, y = top_value*1.1, label = "on", angle = 90, size = 3) +
	annotate(geom = "text", x = lights_off, y = top_value*1.1, label = "off", angle = 90, size = 3) +
	ylab("Lipid oxidation [mL/h]") +
	xlab("Time [h]") +
	facet_wrap(.~sex) +
	scale_color_manual(values = mypal) +
	theme_pubr() +
	theme(legend.title = element_blank(), legend.position = "none", strip.background = element_blank())




# create table to calculate means per animal (grouped by three phases)
lipid_tbl_early <- master4[,c(start_lipid:(start_lipid+lights_off*3)), with = FALSE]
lipid_tbl_early1 <- lipid_tbl_early %>% rowwise() %>% summarize("early_light" = mean(c_across(where(is.numeric))))

lipid_tbl_dark <- master4[,c((start_lipid+lights_off*3):(start_lipid+lights_on*3)), with = FALSE]
lipid_tbl_dark1 <- lipid_tbl_dark %>% rowwise() %>% summarize(dark = mean(c_across(where(is.numeric))))

lipid_tbl_late <- master4[,c((start_lipid+lights_on*3):end_lipid), with = FALSE]
lipid_tbl_late1 <- lipid_tbl_late %>% rowwise() %>% summarize("late_light" = mean(c_across(where(is.numeric))))


lipid_tbl3 <- cbind(lipid_tbl_early1, lipid_tbl_dark1, lipid_tbl_late1, master4$lean_mass, master4$typeReal)
setnames(lipid_tbl3, c("early_light", "dark", "late_light", "leanMass", "type"))
lipid_tbl3 <- data.table(lipid_tbl3)

ggplot(lipid_tbl3, aes(x = leanMass, y = dark, color = type)) +
	geom_point() +
	geom_smooth(method = "lm")



#######
# correct lipid intake with lean mass

# split the sexes
lipid_tbl3_f <- lipid_tbl3[type == mylvls[1] | type == mylvls[2], ]
lipid_tbl3_m <- lipid_tbl3[type != mylvls[1] & type != mylvls[2], ]

# average lean mass
lipid_tbl3_f$avLM <- mean(lipid_tbl3$leanMass, na.rm = TRUE)
lipid_tbl3_m$avLM <- mean(lipid_tbl3$leanMass, na.rm = TRUE)

# linear model for lipid intake
mod_early_f <- lm(early_light ~ leanMass + type, data = lipid_tbl3_f)
mod_early_m <- lm(early_light ~ leanMass + type, data = lipid_tbl3_m)
summary(mod_early_f)
summary(mod_early_m)
mod_dark_f <- lm(dark ~ leanMass + type, data = lipid_tbl3_f)
mod_dark_m <- lm(dark ~ leanMass + type, data = lipid_tbl3_m)
summary(mod_dark_f)
summary(mod_dark_m)
mod_late_f <- lm(late_light ~ leanMass + type, data = lipid_tbl3_f)
mod_late_m <- lm(late_light ~ leanMass + type, data = lipid_tbl3_m)
summary(mod_late_f)
summary(mod_late_m)

# add residuals to data.table
lipid_tbl3_f$resid_LIPID_early <- resid(mod_early_f)
lipid_tbl3_m$resid_LIPID_early <- resid(mod_early_m)
lipid_tbl3_f$resid_LIPID_dark <- resid(mod_dark_f)
lipid_tbl3_m$resid_LIPID_dark <- resid(mod_dark_m)
lipid_tbl3_f$resid_LIPID_late <- resid(mod_late_f)
lipid_tbl3_m$resid_LIPID_late <- resid(mod_late_m)

# calculate ajdusted values
lipid_tbl3_f <- lipid_tbl3_f %>%
  mutate(early_light = case_when(
    type==mylvls[1]~ coef(mod_early_f)[1] + coef(mod_early_f)[2]*avLM + resid_LIPID_early,
    type==mylvls[2]~ (coef(mod_early_f)[1]+ coef(mod_early_f)[3]) + coef(mod_early_f)[2]*avLM + resid_LIPID_early),
  	dark = case_when(
    type==mylvls[1]~ coef(mod_dark_f)[1] + coef(mod_dark_f)[2]*avLM + resid_LIPID_dark,
    type==mylvls[2]~ (coef(mod_dark_f)[1]+ coef(mod_dark_f)[3]) + coef(mod_dark_f)[2]*avLM + resid_LIPID_dark),
    late_light = case_when(
    type==mylvls[1]~ coef(mod_late_f)[1] + coef(mod_late_f)[2]*avLM + resid_LIPID_late,
    type==mylvls[2]~ (coef(mod_late_f)[1]+ coef(mod_late_f)[3]) + coef(mod_late_f)[2]*avLM + resid_LIPID_late))

lipid_tbl3_m <- lipid_tbl3_m %>%
  mutate(early_light = case_when(
    type==mylvls[3]~ coef(mod_early_m)[1] + coef(mod_early_m)[2]*avLM + resid_LIPID_early,
    type==mylvls[4]~ (coef(mod_early_m)[1]+ coef(mod_early_m)[3]) + coef(mod_early_m)[2]*avLM + resid_LIPID_early),
  	dark = case_when(
    type==mylvls[3]~ coef(mod_dark_m)[1] + coef(mod_dark_m)[2]*avLM + resid_LIPID_dark,
    type==mylvls[4]~ (coef(mod_dark_m)[1]+ coef(mod_dark_m)[3]) + coef(mod_dark_m)[2]*avLM + resid_LIPID_dark),
    late_light = case_when(
    type==mylvls[3]~ coef(mod_late_m)[1] + coef(mod_late_m)[2]*avLM + resid_LIPID_late,
    type==mylvls[4]~ (coef(mod_late_m)[1]+ coef(mod_late_m)[3]) + coef(mod_late_m)[2]*avLM + resid_LIPID_late))

lipid_tbl3_combo <- rbind(lipid_tbl3_f, lipid_tbl3_m)
lipid_tbl3_combo_adj <- melt.data.table(lipid_tbl3_combo[,c("type", "early_light", "dark", "late_light")], id.vars = c("type"))
lipid_tbl3_combo_adj[, sex := "male"]
lipid_tbl3_combo_adj[type == mylvls[1] | type == mylvls[2], sex := "female"]



compare <- list(c(mylvls[1], mylvls[2]), c(mylvls[3], mylvls[4]))
min_val <- min(lipid_tbl3_combo_adj$value)
max_val <- max(lipid_tbl3_combo_adj$value)


adj_lipid_mean_plt <- 
ggplot(lipid_tbl3_combo_adj, aes(x = type, y = value, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(width = 0.1) +
	facet_wrap(.~variable) +
	stat_compare_means(comparisons = compare, size = 3, method = "t.test") +
	ylab(expression(atop("Lean mass adjusted","mean lipid oxidation [mL/h]"))) +
	ylim(min_val, max_val*1.3) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white"))

ind_calor_LIPID <- 
ggarrange(lipid_raw_plt, adj_lipid_mean_plt, widths = c(1, 1))












####################################################################
# CARBOHYDRATE OXIDATION


colnames(master4)
# create table to calculate means and sds per time point (not per mouse)
start_cho <- 837
end_cho <- 899
cho_tbl <- master4[,c(6, start_cho:end_cho), with = FALSE]
colnames(cho_tbl)
setnames(cho_tbl, c("type", seq(0, by = 1/3, length.out = (length(cho_tbl)-1))))
cho_tbl2 <- melt.data.table(cho_tbl) %>% group_by(type, variable) %>% summarize(mean = mean(value), sd = sd(value))
cho_tbl2 <- data.table(cho_tbl2)
cho_tbl2$variable <- as.numeric(as.character(cho_tbl2$variable))
cho_tbl2[, sex := ifelse(type == mylvls[1] | type == mylvls[2], "female", "male")]



# plot variables
# indicate starting point of cold exposure
light_cycle <- c(5, 17)
lights_off <- light_cycle[1]
lights_on <- light_cycle[2]
top_value <- max(cho_tbl2$mean)+max(cho_tbl2$sd)
min_value <- min(cho_tbl2$mean)-max(cho_tbl2$sd)


cho_raw_plt <- 
ggplot(cho_tbl2, aes(x = variable, y = mean, color = type)) +
	annotate(geom = "rect", xmin = lights_on, xmax = lights_off, ymin = min_value, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
	geom_point(size = 1) +
	annotate(geom = "segment", x = lights_on, xend = lights_on, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "segment", x = lights_off, xend = lights_off, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "text", x = lights_on, y = top_value*1.1, label = "on", angle = 90, size = 3) +
	annotate(geom = "text", x = lights_off, y = top_value*1.1, label = "off", angle = 90, size = 3) +
	ylab("CHO oxidation [mL/h]") +
	xlab("Time [h]") +
	facet_wrap(.~sex) +
	scale_color_manual(values = mypal) +
	theme_pubr() +
	theme(legend.title = element_blank(), legend.position = "none", strip.background = element_blank())




# create table to calculate means per animal (grouped by three phases)
cho_tbl_early <- master4[,c(start_cho:(start_cho+lights_off*3)), with = FALSE]
cho_tbl_early1 <- cho_tbl_early %>% rowwise() %>% summarize("early_light" = mean(c_across(where(is.numeric))))

cho_tbl_dark <- master4[,c((start_cho+lights_off*3):(start_cho+lights_on*3)), with = FALSE]
cho_tbl_dark1 <- cho_tbl_dark %>% rowwise() %>% summarize(dark = mean(c_across(where(is.numeric))))

cho_tbl_late <- master4[,c((start_cho+lights_on*3):end_cho), with = FALSE]
cho_tbl_late1 <- cho_tbl_late %>% rowwise() %>% summarize("late_light" = mean(c_across(where(is.numeric))))


cho_tbl3 <- cbind(cho_tbl_early1, cho_tbl_dark1, cho_tbl_late1, master4$lean_mass, master4$typeReal)
setnames(cho_tbl3, c("early_light", "dark", "late_light", "leanMass", "type"))
cho_tbl3 <- data.table(cho_tbl3)

ggplot(cho_tbl3, aes(x = leanMass, y = dark, color = type)) +
	geom_point() +
	geom_smooth(method = "lm")



#######
# correct cho intake with lean mass

# split the sexes
cho_tbl3_f <- cho_tbl3[type == mylvls[1] | type == mylvls[2], ]
cho_tbl3_m <- cho_tbl3[type != mylvls[1] & type != mylvls[2], ]

# average lean mass
cho_tbl3_f$avLM <- mean(cho_tbl3$leanMass, na.rm = TRUE)
cho_tbl3_m$avLM <- mean(cho_tbl3$leanMass, na.rm = TRUE)

# linear model for cho intake
mod_early_f <- lm(early_light ~ leanMass + type, data = cho_tbl3_f)
mod_early_m <- lm(early_light ~ leanMass + type, data = cho_tbl3_m)
summary(mod_early_f)
summary(mod_early_m)
mod_dark_f <- lm(dark ~ leanMass + type, data = cho_tbl3_f)
mod_dark_m <- lm(dark ~ leanMass + type, data = cho_tbl3_m)
summary(mod_dark_f)
summary(mod_dark_m)
mod_late_f <- lm(late_light ~ leanMass + type, data = cho_tbl3_f)
mod_late_m <- lm(late_light ~ leanMass + type, data = cho_tbl3_m)
summary(mod_late_f)
summary(mod_late_m)

# add residuals to data.table
cho_tbl3_f$resid_CHO_early <- resid(mod_early_f)
cho_tbl3_m$resid_CHO_early <- resid(mod_early_m)
cho_tbl3_f$resid_CHO_dark <- resid(mod_dark_f)
cho_tbl3_m$resid_CHO_dark <- resid(mod_dark_m)
cho_tbl3_f$resid_CHO_late <- resid(mod_late_f)
cho_tbl3_m$resid_CHO_late <- resid(mod_late_m)

# calculate ajdusted values
cho_tbl3_f <- cho_tbl3_f %>%
  mutate(early_light = case_when(
    type==mylvls[1]~ coef(mod_early_f)[1] + coef(mod_early_f)[2]*avLM + resid_CHO_early,
    type==mylvls[2]~ (coef(mod_early_f)[1]+ coef(mod_early_f)[3]) + coef(mod_early_f)[2]*avLM + resid_CHO_early),
  	dark = case_when(
    type==mylvls[1]~ coef(mod_dark_f)[1] + coef(mod_dark_f)[2]*avLM + resid_CHO_dark,
    type==mylvls[2]~ (coef(mod_dark_f)[1]+ coef(mod_dark_f)[3]) + coef(mod_dark_f)[2]*avLM + resid_CHO_dark),
    late_light = case_when(
    type==mylvls[1]~ coef(mod_late_f)[1] + coef(mod_late_f)[2]*avLM + resid_CHO_late,
    type==mylvls[2]~ (coef(mod_late_f)[1]+ coef(mod_late_f)[3]) + coef(mod_late_f)[2]*avLM + resid_CHO_late))

cho_tbl3_m <- cho_tbl3_m %>%
  mutate(early_light = case_when(
    type==mylvls[3]~ coef(mod_early_m)[1] + coef(mod_early_m)[2]*avLM + resid_CHO_early,
    type==mylvls[4]~ (coef(mod_early_m)[1]+ coef(mod_early_m)[3]) + coef(mod_early_m)[2]*avLM + resid_CHO_early),
  	dark = case_when(
    type==mylvls[3]~ coef(mod_dark_m)[1] + coef(mod_dark_m)[2]*avLM + resid_CHO_dark,
    type==mylvls[4]~ (coef(mod_dark_m)[1]+ coef(mod_dark_m)[3]) + coef(mod_dark_m)[2]*avLM + resid_CHO_dark),
    late_light = case_when(
    type==mylvls[3]~ coef(mod_late_m)[1] + coef(mod_late_m)[2]*avLM + resid_CHO_late,
    type==mylvls[4]~ (coef(mod_late_m)[1]+ coef(mod_late_m)[3]) + coef(mod_late_m)[2]*avLM + resid_CHO_late))

cho_tbl3_combo <- rbind(cho_tbl3_f, cho_tbl3_m)
cho_tbl3_combo_adj <- melt.data.table(cho_tbl3_combo[,c("type", "early_light", "dark", "late_light")], id.vars = c("type"))
cho_tbl3_combo_adj[, sex := "male"]
cho_tbl3_combo_adj[type == mylvls[1] | type == mylvls[2], sex := "female"]



compare <- list(c(mylvls[1], mylvls[2]), c(mylvls[3], mylvls[4]))
min_val <- min(cho_tbl3_combo_adj$value)
max_val <- max(cho_tbl3_combo_adj$value)


adj_cho_mean_plt <- 
ggplot(cho_tbl3_combo_adj, aes(x = type, y = value, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(width = 0.1) +
	facet_wrap(.~variable) +
	stat_compare_means(comparisons = compare, size = 3, method = "t.test") +
	ylab(expression(atop("Lean mass adjusted","mean CHO oxidation [mL/h]"))) +
	ylim(min_val, max_val*1.3) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white"))

ind_calor_CHO <- 
ggarrange(cho_raw_plt, adj_cho_mean_plt, widths = c(1, 1))











####################################################################
# RER


colnames(master4)
# create table to calculate means and sds per time point (not per mouse)
start_rer <- 144
end_rer <- 206
rer_tbl <- master4[,c(6, start_rer:end_rer), with = FALSE]
colnames(rer_tbl)
setnames(rer_tbl, c("type", seq(0, by = 1/3, length.out = (length(rer_tbl)-1))))
rer_tbl2 <- melt.data.table(rer_tbl) %>% group_by(type, variable) %>% summarize(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE))
rer_tbl2 <- data.table(rer_tbl2)
rer_tbl2$variable <- as.numeric(as.character(rer_tbl2$variable))
rer_tbl2[, sex := ifelse(type == mylvls[1] | type == mylvls[2], "female", "male")]



# plot variables
# indicate starting point of cold exposure
light_cycle <- c(5, 17)
lights_off <- light_cycle[1]
lights_on <- light_cycle[2]
top_value <- max(rer_tbl2$mean)+max(rer_tbl2$sd)
min_value <- min(rer_tbl2$mean)-max(rer_tbl2$sd)


rer_raw_plt <- 
ggplot(rer_tbl2, aes(x = variable, y = mean, color = type)) +
	annotate(geom = "rect", xmin = lights_on, xmax = lights_off, ymin = min_value, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
	geom_point(size = 1) +
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




# create table to calculate means per animal (grouped by three phases)
rer_tbl_early <- master4[,c(start_rer:(start_rer+lights_off*3)), with = FALSE]
rer_tbl_early1 <- rer_tbl_early %>% rowwise() %>% summarize("early_light" = mean(c_across(where(is.numeric))))

rer_tbl_dark <- master4[,c((start_rer+lights_off*3):(start_rer+lights_on*3)), with = FALSE]
rer_tbl_dark1 <- rer_tbl_dark %>% rowwise() %>% summarize(dark = mean(c_across(where(is.numeric))))

rer_tbl_late <- master4[,c((start_rer+lights_on*3):end_rer), with = FALSE]
rer_tbl_late1 <- rer_tbl_late %>% rowwise() %>% summarize("late_light" = mean(c_across(where(is.numeric))))


rer_tbl3 <- cbind(rer_tbl_early1, rer_tbl_dark1, rer_tbl_late1, master4$typeReal)
setnames(rer_tbl3, c("early_light", "dark", "late_light", "type"))
rer_tbl3 <- data.table(rer_tbl3)
rer_tbl3_melt <- melt.data.table(rer_tbl3, id.vars = "type")


#######
# don't correct RER

compare <- list(c(mylvls[1], mylvls[2]), c(mylvls[3], mylvls[4]))
min_val <- min(rer_tbl3_melt$value, na.rm = TRUE)
max_val <- max(rer_tbl3_melt$value, na.rm = TRUE)


adj_rer_mean_plt <- 
ggplot(rer_tbl3_melt, aes(x = type, y = value, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(width = 0.1) +
	facet_wrap(.~variable) +
	stat_compare_means(comparisons = compare, size = 3, method = "t.test") +
	ylab(expression("Mean RER"~"[VCO"[2]/VO[2]*"]")) +
	ylim(min_val, max_val*1.05) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white"))

ind_calor_RER <- 
ggarrange(rer_raw_plt, adj_rer_mean_plt, widths = c(1, 1))













####################################################################
# DISTANCE


colnames(master4)
# create table to calculate means and sds per time point (not per mouse)
start_dist <- 648
end_dist <- 710
dist_tbl <- master4[,c(6, start_dist:end_dist), with = FALSE]
colnames(dist_tbl)
setnames(dist_tbl, c("type", seq(0, by = 1/3, length.out = (length(dist_tbl)-1))))
dist_tbl2 <- melt.data.table(dist_tbl) %>% group_by(type, variable) %>% summarize(mean = mean(value), sd = sd(value))
dist_tbl2 <- data.table(dist_tbl2)
dist_tbl2$variable <- as.numeric(as.character(dist_tbl2$variable))
dist_tbl2[, sex := ifelse(type == mylvls[1] | type == mylvls[2], "female", "male")]



# plot variables
# indicate starting point of cold exposure
light_cycle <- c(5, 17)
lights_off <- light_cycle[1]
lights_on <- light_cycle[2]
top_value <- max(dist_tbl2$mean)+max(dist_tbl2$sd)
min_value <- min(dist_tbl2$mean)-max(dist_tbl2$sd)


dist_raw_plt <- 
ggplot(dist_tbl2, aes(x = variable, y = mean, color = type)) +
	annotate(geom = "rect", xmin = lights_on, xmax = lights_off, ymin = min_value, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
	geom_point(size = 1) +
	annotate(geom = "segment", x = lights_on, xend = lights_on, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "segment", x = lights_off, xend = lights_off, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "text", x = lights_on, y = top_value*1.1, label = "on", angle = 90, size = 3) +
	annotate(geom = "text", x = lights_off, y = top_value*1.1, label = "off", angle = 90, size = 3) +
	ylab("Distance [cm]") +
	xlab("Time [h]") +
	facet_wrap(.~sex) +
	scale_color_manual(values = mypal) +
	theme_pubr() +
	theme(legend.title = element_blank(), legend.position = "none", strip.background = element_blank())




# create table to calculate means per animal (grouped by three phases)
dist_tbl_early <- master4[,c(start_dist:(start_dist+lights_off*3)), with = FALSE]
dist_tbl_early1 <- dist_tbl_early %>% rowwise() %>% summarize("early_light" = mean(c_across(where(is.numeric))))

dist_tbl_dark <- master4[,c((start_dist+lights_off*3):(start_dist+lights_on*3)), with = FALSE]
dist_tbl_dark1 <- dist_tbl_dark %>% rowwise() %>% summarize(dark = mean(c_across(where(is.numeric))))

dist_tbl_late <- master4[,c((start_dist+lights_on*3):end_dist), with = FALSE]
dist_tbl_late1 <- dist_tbl_late %>% rowwise() %>% summarize("late_light" = mean(c_across(where(is.numeric))))


dist_tbl3 <- cbind(dist_tbl_early1, dist_tbl_dark1, dist_tbl_late1, master4$typeReal)
setnames(dist_tbl3, c("early_light", "dark", "late_light", "type"))
dist_tbl3 <- data.table(dist_tbl3)
dist_tbl3_melt <- melt.data.table(dist_tbl3, id.vars = "type")


#######
# don't correct DIST

compare <- list(c(mylvls[1], mylvls[2]), c(mylvls[3], mylvls[4]))
min_val <- min(dist_tbl3_melt$value, na.rm = TRUE)
max_val <- max(dist_tbl3_melt$value, na.rm = TRUE)


adj_dist_mean_plt <- 
ggplot(dist_tbl3_melt, aes(x = type, y = value, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(width = 0.1) +
	facet_wrap(.~variable) +
	stat_compare_means(comparisons = compare, size = 3, method = "t.test") +
	ylab("Mean distance [cm]") +
	ylim(min_val, max_val*1.2) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white"))

ind_calor_DIST <- 
ggarrange(dist_raw_plt, adj_dist_mean_plt, widths = c(1, 1))












####################################################################
# HEAT


colnames(master4)
# create table to calculate means and sds per time point (not per mouse)
start_heat <- 207
end_heat <- 269
heat_tbl <- master4[,c(6, start_heat:end_heat), with = FALSE]
colnames(heat_tbl)
setnames(heat_tbl, c("type", seq(0, by = 1/3, length.out = (length(heat_tbl)-1))))
heat_tbl2 <- melt.data.table(heat_tbl) %>% group_by(type, variable) %>% summarize(mean = mean(value), sd = sd(value))
heat_tbl2 <- data.table(heat_tbl2)
heat_tbl2$variable <- as.numeric(as.character(heat_tbl2$variable))
heat_tbl2[, sex := ifelse(type == mylvls[1] | type == mylvls[2], "female", "male")]



# plot variables
# indicate starting point of cold exposure
light_cycle <- c(5, 17)
lights_off <- light_cycle[1]
lights_on <- light_cycle[2]
top_value <- max(heat_tbl2$mean)+max(heat_tbl2$sd)
min_value <- min(heat_tbl2$mean)-max(heat_tbl2$sd)


heat_raw_plt <- 
ggplot(heat_tbl2, aes(x = variable, y = mean, color = type)) +
	annotate(geom = "rect", xmin = lights_on, xmax = lights_off, ymin = min_value, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
	geom_point(size = 1) +
	annotate(geom = "segment", x = lights_on, xend = lights_on, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "segment", x = lights_off, xend = lights_off, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "text", x = lights_on, y = top_value*1.1, label = "on", angle = 90, size = 3) +
	annotate(geom = "text", x = lights_off, y = top_value*1.1, label = "off", angle = 90, size = 3) +
	ylab("Heat [kJ/h]") +
	xlab("Time [h]") +
	facet_wrap(.~sex) +
	scale_color_manual(values = mypal) +
	theme_pubr() +
	theme(legend.title = element_blank(), legend.position = "none", strip.background = element_blank())




# create table to calculate means per animal (grouped by three phases)
heat_tbl_early <- master4[,c(start_heat:(start_heat+lights_off*3)), with = FALSE]
heat_tbl_early1 <- heat_tbl_early %>% rowwise() %>% summarize("early_light" = mean(c_across(where(is.numeric))))

heat_tbl_dark <- master4[,c((start_heat+lights_off*3):(start_heat+lights_on*3)), with = FALSE]
heat_tbl_dark1 <- heat_tbl_dark %>% rowwise() %>% summarize(dark = mean(c_across(where(is.numeric))))

heat_tbl_late <- master4[,c((start_heat+lights_on*3):end_heat), with = FALSE]
heat_tbl_late1 <- heat_tbl_late %>% rowwise() %>% summarize("late_light" = mean(c_across(where(is.numeric))))


heat_tbl3 <- cbind(heat_tbl_early1, heat_tbl_dark1, heat_tbl_late1, master4$lean_mass, master4$typeReal)
setnames(heat_tbl3, c("early_light", "dark", "late_light", "leanMass", "type"))
heat_tbl3 <- data.table(heat_tbl3)

ggplot(heat_tbl3, aes(x = leanMass, y = dark, color = type)) +
	geom_point() +
	geom_smooth(method = "lm")




#######
# correct heat with lean mass

# split the sexes
heat_tbl3_f <- heat_tbl3[type == mylvls[1] | type == mylvls[2], ]
heat_tbl3_m <- heat_tbl3[type != mylvls[1] & type != mylvls[2], ]

# average lean mass
heat_tbl3_f$avLM <- mean(heat_tbl3$leanMass, na.rm = TRUE)
heat_tbl3_m$avLM <- mean(heat_tbl3$leanMass, na.rm = TRUE)

# linear model for heat
mod_early_f <- lm(early_light ~ leanMass + type, data = heat_tbl3_f)
mod_early_m <- lm(early_light ~ leanMass + type, data = heat_tbl3_m)
summary(mod_early_f)
summary(mod_early_m)
mod_dark_f <- lm(dark ~ leanMass + type, data = heat_tbl3_f)
mod_dark_m <- lm(dark ~ leanMass + type, data = heat_tbl3_m)
summary(mod_dark_f)
summary(mod_dark_m)
mod_late_f <- lm(late_light ~ leanMass + type, data = heat_tbl3_f)
mod_late_m <- lm(late_light ~ leanMass + type, data = heat_tbl3_m)
summary(mod_late_f)
summary(mod_late_m)

# add residuals to data.table
heat_tbl3_f$resid_HEAT_early <- resid(mod_early_f)
heat_tbl3_m$resid_HEAT_early <- resid(mod_early_m)
heat_tbl3_f$resid_HEAT_dark <- resid(mod_dark_f)
heat_tbl3_m$resid_HEAT_dark <- resid(mod_dark_m)
heat_tbl3_f$resid_HEAT_late <- resid(mod_late_f)
heat_tbl3_m$resid_HEAT_late <- resid(mod_late_m)

# calculate ajdusted values
heat_tbl3_f <- heat_tbl3_f %>%
  mutate(early_light = case_when(
    type==mylvls[1]~ coef(mod_early_f)[1] + coef(mod_early_f)[2]*avLM + resid_HEAT_early,
    type==mylvls[2]~ (coef(mod_early_f)[1]+ coef(mod_early_f)[3]) + coef(mod_early_f)[2]*avLM + resid_HEAT_early),
  	dark = case_when(
    type==mylvls[1]~ coef(mod_dark_f)[1] + coef(mod_dark_f)[2]*avLM + resid_HEAT_dark,
    type==mylvls[2]~ (coef(mod_dark_f)[1]+ coef(mod_dark_f)[3]) + coef(mod_dark_f)[2]*avLM + resid_HEAT_dark),
    late_light = case_when(
    type==mylvls[1]~ coef(mod_late_f)[1] + coef(mod_late_f)[2]*avLM + resid_HEAT_late,
    type==mylvls[2]~ (coef(mod_late_f)[1]+ coef(mod_late_f)[3]) + coef(mod_late_f)[2]*avLM + resid_HEAT_late))

heat_tbl3_m <- heat_tbl3_m %>%
  mutate(early_light = case_when(
    type==mylvls[3]~ coef(mod_early_m)[1] + coef(mod_early_m)[2]*avLM + resid_HEAT_early,
    type==mylvls[4]~ (coef(mod_early_m)[1]+ coef(mod_early_m)[3]) + coef(mod_early_m)[2]*avLM + resid_HEAT_early),
  	dark = case_when(
    type==mylvls[3]~ coef(mod_dark_m)[1] + coef(mod_dark_m)[2]*avLM + resid_HEAT_dark,
    type==mylvls[4]~ (coef(mod_dark_m)[1]+ coef(mod_dark_m)[3]) + coef(mod_dark_m)[2]*avLM + resid_HEAT_dark),
    late_light = case_when(
    type==mylvls[3]~ coef(mod_late_m)[1] + coef(mod_late_m)[2]*avLM + resid_HEAT_late,
    type==mylvls[4]~ (coef(mod_late_m)[1]+ coef(mod_late_m)[3]) + coef(mod_late_m)[2]*avLM + resid_HEAT_late))

heat_tbl3_combo <- rbind(heat_tbl3_f, heat_tbl3_m)
heat_tbl3_combo_adj <- melt.data.table(heat_tbl3_combo[,c("type", "early_light", "dark", "late_light")], id.vars = c("type"))
heat_tbl3_combo_adj[, sex := "male"]
heat_tbl3_combo_adj[type == mylvls[1] | type == mylvls[2], sex := "female"]




compare <- list(c(mylvls[1], mylvls[2]), c(mylvls[3], mylvls[4]))
min_val <- min(heat_tbl3_combo_adj$value, na.rm = TRUE)
max_val <- max(heat_tbl3_combo_adj$value, na.rm = TRUE)


adj_heat_mean_plt <- 
ggplot(heat_tbl3_combo_adj, aes(x = type, y = value, color = type, fill = type)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
	geom_jitter(width = 0.1) +
	facet_wrap(.~variable) +
	stat_compare_means(comparisons = compare, size = 3, method = "t.test") +
	ylab(expression(atop("Lean mass adjusted","mean heat production [kJ/h]"))) +
	ylim(min_val, max_val*1.2) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white"))

ind_calor_HEAT <- 
ggarrange(heat_raw_plt, adj_heat_mean_plt, widths = c(1, 1), align = "hv")








everyother_label <- c("a", "", "b", "", "c", "", "d", "", "e", "", "f", "", "g", "", "h", "", "i")


plt4 <- 
plot_grid(o2_raw_plt, adj_o2_mean_plt, rer_raw_plt, adj_rer_mean_plt, cho_raw_plt, adj_cho_mean_plt, lipid_raw_plt, adj_lipid_mean_plt, heat_raw_plt, adj_heat_mean_plt, dist_raw_plt, adj_dist_mean_plt, food_raw_plt, adj_food_mean_plt, align = "vh", axis = "bl", nrow = 4, ncol = 4, labels = everyother_label)

# plt4 <- 
# ggarrange(ind_calor_O2+margin, ind_calor_RER+margin, ind_calor_CHO+margin, ind_calor_LIPID+margin, ind_calor_HEAT+margin, ind_calor_DIST+margin,ind_calor_FOOD+margin, nrow = 4, ncol = 2, labels = "auto", common.legend = TRUE, align = "hv")


ggsave(paste(fig_path, "Fig4.png", sep = ""), plt4, device = png(), width = 15, height = 12, bg = "white")
dev.off()

ggsave(paste(fig_path, "Fig4.pdf", sep = ""), plt4, device = "pdf", width = 15, height = 12, bg = "white")
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






# theme & info:
# Fig. 4A is info panel
# b: energy intake and expenditure
# c: O2 consumption
# d: RER
# e: mean heat / mean weight
# f: mean VO2 / lean mass


####################################################################
# DATA IMPORT AND TIDY
####################################################################


# b: energy intake and expenditure

tbl4b <- data.table(read_excel("Data/Fig4/PF_Fig4B_FoodCumulative.xlsx"))
colnames(tbl4b)


mkoki4b00 <- tbl4b[, c(1, 2:16)]
mkoki4b0 <- mkoki4b00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
mkoki4b <- cbind(data.table(mkoki4b0[, c("time", "mean", "sd")]), 
	type = rep("m_Mmut-ko/ki", length(mkoki4b0$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkoki4b0$mean)))

mkiwt4b00 <- tbl4b[, c(1, 32:43)]
mkiwt4b0 <- mkiwt4b00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
mkiwt4b <- cbind(data.table(mkiwt4b0[, c("time", "mean", "sd")]), 
	type = rep("m_Mmut-ki/wt", length(mkiwt4b0$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkiwt4b0$mean)))

m4b <- rbind(mkiwt4b, mkoki4b)


fkoki4b00 <- tbl4b[, c(1, 17:31)]
fkoki4b0 <- fkoki4b00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
fkoki4b <- cbind(data.table(fkoki4b0[, c("time", "mean", "sd")]), 
	type = rep("f_Mmut-ko/ki", length(fkoki4b0$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkoki4b0$mean)))

fkiwt4b00 <- tbl4b[, c(1, 47:60)]
fkiwt4b0 <- fkiwt4b00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
fkiwt4b <- cbind(data.table(fkiwt4b0[, c("time", "mean", "sd")]), 
	type = rep("f_Mmut-ki/wt", length(fkiwt4b0$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkiwt4b0$mean)))

f4b <- rbind(fkiwt4b, fkoki4b)

m4b[, gender := "male"]
f4b[, gender := "female"]


#### add phases and reset mean for each phase

# male

m4b[, phase := ifelse(timemod<lights_off, "Phase 1", ifelse(timemod>=lights_on, "Phase 3", "Phase 2"))]
m4b$phase <- factor(m4b$phase)

diff_phase1_ko <- m4b[phase == "Phase 1" & type == mylvls[4] & timemod == 0, ]$mean
diff_phase1_wt <- m4b[phase == "Phase 1" & type == mylvls[3] & timemod == 0, ]$mean
m4b[phase == "Phase 1" & type == mylvls[4], phase_mean := mean- diff_phase1_ko]
m4b[phase == "Phase 1" & type == mylvls[3], phase_mean := mean-diff_phase1_wt]

diff_phase2_ko <- m4b[phase == "Phase 2" & type == mylvls[4] & timemod == lights_off, ]$mean
diff_phase2_wt <- m4b[phase == "Phase 2" & type == mylvls[3] & timemod == lights_off, ]$mean
m4b[phase == "Phase 2" & type == mylvls[4], phase_mean := mean- diff_phase2_ko]
m4b[phase == "Phase 2" & type == mylvls[3], phase_mean := mean-diff_phase2_wt]

diff_phase3_ko <- m4b[phase == "Phase 3" & type == mylvls[4] & timemod == lights_on, ]$mean
diff_phase3_wt <- m4b[phase == "Phase 3" & type == mylvls[3] & timemod == lights_on, ]$mean
m4b[phase == "Phase 3" & type == mylvls[4], phase_mean := mean- diff_phase3_ko]
m4b[phase == "Phase 3" & type == mylvls[3], phase_mean := mean-diff_phase3_wt]


# female

f4b[, phase := ifelse(timemod<lights_off, "Phase 1", ifelse(timemod>=lights_on, "Phase 3", "Phase 2"))]
f4b$phase <- factor(f4b$phase)

diff_phase1_ko <- f4b[phase == "Phase 1" & type == mylvls[2] & timemod == 0, ]$mean
diff_phase1_wt <- f4b[phase == "Phase 1" & type == mylvls[1] & timemod == 0, ]$mean
f4b[phase == "Phase 1" & type == mylvls[2], phase_mean := mean- diff_phase1_ko]
f4b[phase == "Phase 1" & type == mylvls[1], phase_mean := mean-diff_phase1_wt]

diff_phase2_ko <- f4b[phase == "Phase 2" & type == mylvls[2] & timemod == lights_off, ]$mean
diff_phase2_wt <- f4b[phase == "Phase 2" & type == mylvls[1] & timemod == lights_off, ]$mean
f4b[phase == "Phase 2" & type == mylvls[2], phase_mean := mean- diff_phase2_ko]
f4b[phase == "Phase 2" & type == mylvls[1], phase_mean := mean-diff_phase2_wt]

diff_phase3_ko <- f4b[phase == "Phase 3" & type == mylvls[2] & timemod == lights_on, ]$mean
diff_phase3_wt <- f4b[phase == "Phase 3" & type == mylvls[1] & timemod == lights_on, ]$mean
f4b[phase == "Phase 3" & type == mylvls[2], phase_mean := mean- diff_phase3_ko]
f4b[phase == "Phase 3" & type == mylvls[1], phase_mean := mean-diff_phase3_wt]





tbl4b_merge <- rbind(m4b, f4b)

tbl4b_merge[, gender := "male"]
tbl4b_merge[type == mylvls[1] | type == mylvls[2], gender := "female"]



# last time point for final overall food intake

final_food <- data.table(read_excel("Data/Fig4/Mut Ko KI Data Indirect Calorimetry_15042019(002)_PF.xlsx"))
final_food$Mouse_ID

# mean weight
final_food_bw <- final_food[c(8:9), 2:ncol(final_food)]
final_food_bw <- as.data.table(sapply(final_food_bw, as.numeric))
final_food_bw <- t(colMeans(final_food_bw))
final_food_bw <- melt(as.data.table(final_food_bw))
setnames(final_food_bw, c("id", "bw"))

# mean heat
final_food_food <- final_food[10, 2:ncol(final_food)]
final_food_food <- sapply(final_food_food, as.numeric)
final_food_food<- t(final_food_food)
final_food_food <- melt(as.data.table(final_food_food))
setnames(final_food_food, c("id", "food"))

# extract types and ids
final_food_type <- final_food[3, 2:ncol(final_food)]
final_food_type <- t(final_food_type)
final_food_type <- data.table(id = rownames(final_food_type), typemod = as.vector(final_food_type[,1]))


final_food_merge <- merge(merge(final_food_bw, final_food_food, by = "id"), final_food_type, by = "id")
unique(final_food_merge$typemod)
final_food_merge[typemod == "f_[KO/KI]", type := mylvls[2] ]
final_food_merge[typemod == "m_[KI/WT]", type := mylvls[3] ]
final_food_merge[typemod == "m_[KO/KI]", type := mylvls[4] ]
final_food_merge[typemod == "f_[KI/WT]", type := mylvls[1] ]


final_food_merge[, genotype := "ko_ki"]
final_food_merge[type == mylvls[1] | type == mylvls[3], genotype := "ki_wt"]
final_food_merge[, gender := "male"]
final_food_merge[type == mylvls[1] | type == mylvls[2], gender := "female"]


mean(final_food_merge[type == mylvls[4], ]$bw)

summary(lm(food ~ bw + genotype + gender + gender:genotype, final_food_merge))








# c: O2 consumption

tbl4c <- data.table(read_excel("Data/Fig4/PF_Fig4C_O2Consumption.xlsx"))
colnames(tbl4c)


mkoki4c00 <- tbl4c[, c(1, 2:16)]
mkoki4c0 <- mkoki4c00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
mkoki4c <- cbind(data.table(mkoki4c0[, c("time", "mean", "sd")]), 
	type = rep("m_Mmut-ko/ki", length(mkoki4c0$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkoki4c0$mean)))

mkiwt4c00 <- tbl4c[, c(1, 32:43)]
mkiwt4c0 <- mkiwt4c00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
mkiwt4c <- cbind(data.table(mkiwt4c0[, c("time", "mean", "sd")]), 
	type = rep("m_Mmut-ki/wt", length(mkiwt4c0$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkiwt4c0$mean)))

m4c <- rbind(mkiwt4c, mkoki4c)


fkoki4c00 <- tbl4c[, c(1, 17:31)]
fkoki4c0 <- fkoki4c00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
fkoki4c <- cbind(data.table(fkoki4c0[, c("time", "mean", "sd")]), 
	type = rep("f_Mmut-ko/ki", length(fkoki4c0$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkoki4c0$mean)))

fkiwt4c00 <- tbl4c[, c(1, 47:60)]
fkiwt4c0 <- fkiwt4c00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
fkiwt4c <- cbind(data.table(fkiwt4c0[, c("time", "mean", "sd")]), 
	type = rep("f_Mmut-ki/wt", length(fkiwt4c0$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkiwt4c0$mean)))

f4c <- rbind(fkiwt4c, fkoki4c)

m4c[, gender := "male"]
f4c[, gender := "female"]

tbl4c_merge <- rbind(m4c, f4c)


tbl4c_merge[, phase := ifelse(timemod<lights_off, "Phase 1", ifelse(timemod>=lights_on, "Phase 3", "Phase 2"))]
tbl4c_merge$phase <- factor(tbl4c_merge$phase)




# d: RER

tbl4d <- data.table(read_excel("Data/Fig4/PF_Fig4D_RER.xlsx"))
colnames(tbl4d)


mkoki4d00 <- tbl4d[, c(1, 2:16)]
mkoki4d0 <- mkoki4d00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
mkoki4d <- cbind(data.table(mkoki4d0[, c("time", "mean", "sd")]), 
	type = rep("m_Mmut-ko/ki", length(mkoki4d0$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkoki4d0$mean)))

mkiwt4d00 <- tbl4d[, c(1, 32:43)]
mkiwt4d0 <- mkiwt4d00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
mkiwt4d <- cbind(data.table(mkiwt4d0[, c("time", "mean", "sd")]), 
	type = rep("m_Mmut-ki/wt", length(mkiwt4d0$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkiwt4d0$mean)))

m4d <- rbind(mkiwt4d, mkoki4d)


fkoki4d00 <- tbl4d[, c(1, 17:31)]
fkoki4d0 <- fkoki4d00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
fkoki4d <- cbind(data.table(fkoki4d0[, c("time", "mean", "sd")]), 
	type = rep("f_Mmut-ko/ki", length(fkoki4d0$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkoki4d0$mean)))

fkiwt4d00 <- tbl4d[, c(1, 47:60)]
fkiwt4d0 <- fkiwt4d00 %>% rowwise() %>% mutate(mean = mean(c_across(where(is.numeric))), sd = sd(c_across(where(is.numeric))))
fkiwt4d <- cbind(data.table(fkiwt4d0[, c("time", "mean", "sd")]), 
	type = rep("f_Mmut-ki/wt", length(fkiwt4d0$mean)),
	timemod = seq(0, by = 1/3, length.out = length(mkiwt4d0$mean)))

f4d <- rbind(fkiwt4d, fkoki4d)

m4d[, gender := "male"]
f4d[, gender := "female"]

tbl4d_merge <- rbind(m4d, f4d)


tbl4d_merge[, phase := ifelse(timemod<lights_off, "Phase 1", ifelse(timemod>=lights_on, "Phase 3", "Phase 2"))]
tbl4d_merge$phase <- factor(tbl4d_merge$phase)




# e: mean heat / mean weight

tbl4e <- data.table(read_excel("Data/Fig4/Mut Ko KI Data Indirect Calorimetry_15042019(002)_PF.xlsx"))
tbl4e$Mouse_ID

# mean weight
tbl4e_bw <- tbl4e[c(8:9), 2:ncol(tbl4e)]
tbl4e_bw <- as.data.table(sapply(tbl4e_bw, as.numeric))
tbl4e_bw <- t(colMeans(tbl4e_bw))
tbl4e_bw <- melt(as.data.table(tbl4e_bw))
setnames(tbl4e_bw, c("id", "bw"))

# mean heat
tbl4e_heat <- tbl4e[c(202:264), 2:ncol(tbl4e)]
tbl4e_heat <- as.data.table(sapply(tbl4e_heat, as.numeric))
tbl4e_heat <- t(colMeans(tbl4e_heat))
tbl4e_heat <- melt(as.data.table(tbl4e_heat))
setnames(tbl4e_heat, c("id", "heat"))

# extract types and ids
tbl4e_type <- tbl4e[3, 2:ncol(tbl4e)]
tbl4e_type <- t(tbl4e_type)
tbl4e_type <- data.table(id = rownames(tbl4e_type), typemod = as.vector(tbl4e_type[,1]))


tbl4e_merge <- merge(merge(tbl4e_bw, tbl4e_heat, by = "id"), tbl4e_type, by = "id")
unique(tbl4e_merge$typemod)
tbl4e_merge[typemod == "f_[KO/KI]", type := mylvls[2] ]
tbl4e_merge[typemod == "m_[KI/WT]", type := mylvls[3] ]
tbl4e_merge[typemod == "m_[KO/KI]", type := mylvls[4] ]
tbl4e_merge[typemod == "f_[KI/WT]", type := mylvls[1] ]


tbl4e_merge[, genotype := "ko_ki"]
tbl4e_merge[type == mylvls[1] | type == mylvls[3], genotype := "ki_wt"]

summary(lm(heat ~ bw + genotype, tbl4e_merge))




# f: mean VO2 / lean mass

tbl4f <- data.table(read_excel("Data/Fig4/PF_Fig4f_heatLeanRatio.xlsx"))
tbl4f <- melt.data.table(tbl4f, id.vars = "lean")
tbl4f <- tbl4f[!is.na(value), ]
setnames(tbl4f, c("lean", "type", "heat"))


tbl4f[, genotype := "ko_ki"]
tbl4f[type == mylvls[1] | type == mylvls[3], genotype := "ki_wt"]
tbl4f[, gender := "male"]
tbl4f[type == mylvls[1] | type == mylvls[2], gender := "female"]

summary(lm(heat ~ lean + genotype, tbl4f))







# further variables for plotting
light_cycle <- c(5, 17)
lights_off <- light_cycle[1]
lights_on <- light_cycle[2]

margs = c(0.5, 0.5, 0.5, 0.5)
margin = theme(plot.margin = unit(margs, "cm"))


####################################################################
# PLOTS AND ARRANGE AND SAVE
####################################################################


# b: energy intake and expenditure

# plt4b2 <- 
# ggplot(m4b, aes(x = timemod, y = mean, color = type)) +
# 	geom_point() +
# 	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
# 	ylab("Cumulative food intake [g]") +
# 	xlab("Time [h]") +
# 	theme_pubr() +
# 	scale_color_manual(values = mypal[c(3,4)]) +
# 	theme(legend.title = element_blank())

# plt4b1 <- 
# ggplot(f4b, aes(x = timemod, y = mean, color = type)) +
# 	geom_point() +
# 	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
# 	ylab("Cumulative food intake [g]") +
# 	xlab("Time [h]") +
# 	theme_pubr() +
# 	scale_color_manual(values = mypal[c(1,2)]) +
# 	theme(legend.title = element_blank())

# plt4b <-
# ggarrange(plt4b1, plt4b2, nrow = 2)

top_value <- max(tbl4b_merge$mean)+max(tbl4b_merge$sd)
min_value <- min(tbl4b_merge$mean)-max(tbl4b_merge$sd)

plt4b <-
ggplot(tbl4b_merge, aes(x = timemod, y = mean, color = type)) +
	annotate(geom = "rect", xmin = lights_on, xmax = lights_off, ymin = min_value, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
	geom_point(size = 1) +
	annotate(geom = "segment", x = lights_on, xend = lights_on, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "segment", x = lights_off, xend = lights_off, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "text", x = lights_on, y = top_value*1.1, label = "on", angle = 90, size = 3) +
	annotate(geom = "text", x = lights_off, y = top_value*1.1, label = "off", angle = 90, size = 3) +
	facet_wrap(~gender, nrow = 2) +
	ylim(min_value, top_value*1.15) +
	ylab("Cumulative food intake [g]") +
	xlab("Time [h]") +
	theme_pubr() +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank()) +
	margin



# c: O2 consumption

top_value <- max(tbl4c_merge$mean)+max(tbl4c_merge$sd)
min_value <- min(tbl4c_merge$mean)-max(tbl4c_merge$sd)

plt4c <-
ggplot(tbl4c_merge, aes(x = timemod, y = mean, color = type)) +
	annotate(geom = "rect", xmin = lights_on, xmax = lights_off, ymin = min_value, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
	geom_point(size = 1) +
	annotate(geom = "segment", x = lights_on, xend = lights_on, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "segment", x = lights_off, xend = lights_off, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "text", x = lights_on, y = top_value*1.1, label = "on", angle = 90, size = 3) +
	annotate(geom = "text", x = lights_off, y = top_value*1.1, label = "off", angle = 90, size = 3) +
	facet_wrap(~gender, nrow = 2) +
	ylim(min_value, top_value*1.13) +
	ylab(expression("O"[2]~"consumption"~"[mL/h]")) +
	xlab("Time [h]") +
	theme_pubr() +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank()) +
	margin



# d: RER

top_value <- max(tbl4d_merge$mean)+max(tbl4d_merge$sd)
min_value <- min(tbl4d_merge$mean)-max(tbl4d_merge$sd)

plt4d <-
ggplot(tbl4d_merge, aes(x = timemod, y = mean, color = type)) +
	annotate(geom = "rect", xmin = lights_on, xmax = lights_off, ymin = min_value, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
	geom_point(size = 1) +
	annotate(geom = "segment", x = lights_on, xend = lights_on, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "segment", x = lights_off, xend = lights_off, y = min_value, yend = top_value, linetype = "dashed") +
	annotate(geom = "text", x = lights_on, y = top_value*1.03, label = "on", angle = 90, size = 3) +
	annotate(geom = "text", x = lights_off, y = top_value*1.03, label = "off", angle = 90, size = 3) +
	facet_wrap(~gender, nrow = 2) +
	ylim(min_value, top_value*1.04) +
	ylab(expression("RER"~"[VCO"[2]/VO[2]*"]")) +
	xlab("Time [h]") +
	theme_pubr() +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank()) +
	margin



# e: mean heat / mean weight

plt4e <-
ggplot(tbl4e_merge, aes(x = bw, y = heat, color = type)) +
	geom_point() +
	geom_smooth(method = "lm", se = FALSE) +
 	stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, size = 3, aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")))+
	ylab("Mean heat [kJ/h]") +
	xlab("Mean body weight [g]") +
	theme_pubr() +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.position = "none") +
	margin


# library(ggrepel)
# ggplot(tbl4e_merge[type == "m_Mmut-ki/wt", ], aes(x = bw, y = heat, color = type, label = id)) +
# 	geom_point() +
# 	geom_text_repel() +
#  	stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, size = 3, aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")))+
# 	ylab("Mean heat [kJ/h]") +
# 	xlab("Mean body weight [g]") +
# 	theme_pubr() +
# 	scale_fill_manual(values = mypal) +
# 	scale_color_manual(values = mypal) +
# 	theme(legend.position = "none") +
# 	margin



# f: mean VO2 / lean mass

plt4f <-
ggplot(tbl4f, aes(x = lean, y = heat, color = type)) +
	geom_point() +
	geom_smooth(method = "lm", se = FALSE) +
 	stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, size = 3, aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")))+
	ylab("Mean heat [kJ/h]") +
	xlab("Mean lean mass [g]") +
	theme_pubr() +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.position = "none") +
	margin





plt4top <- 
ggarrange(plt4b, plt4c, plt4d, nrow = 1, labels = c("a", "b", "c"), font.label = list(size = 18), common.legend = TRUE, legend = "bottom")
plt4bottom <- 
ggarrange(plt4e, plt4f, labels = c("d", "e"), font.label = list(size = 18))

plt4 <- 
ggarrange(plt4top, plt4bottom, nrow = 2, heights = c(1, 0.7))

ggsave(paste(fig_path, "Fig4.png", sep = ""), plt4, device = png(), width = 8, height = 8)
dev.off()








####################################################################
# alternative design with boxplots only (to be moved to some supplementary fig)
####################################################################

light_cycle <- c(5, 17)
lights_off <- light_cycle[1]
lights_on <- light_cycle[2]

compare <- list(c(mylvls[1], mylvls[2]), c(mylvls[3], mylvls[4]))
compare_f <- list(c(mylvls[1], mylvls[2]))
compare_m <- list(c(mylvls[3], mylvls[4]))

gender.labs <- c("", "")
names(gender.labs) <- c("female", "male")

######
# cumulative food intake

# max_val <- max(tbl4b_merge$phase_mean)

# boxFood_f <- 
# ggplot(tbl4b_merge[gender == "female", ], aes(x = type, y = phase_mean, color = type, fill = type)) +
# 	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
# 	geom_jitter(width = 0.1) +
# 	facet_wrap(.~phase) +
# 	stat_compare_means(comparisons = compare_f, label.y = max_val*1.05) +
# 	ylab("Mean food intake [g]") +
# 	theme_pubr() +
# 	scale_fill_manual(values = mypal[c(1,2)]) +
# 	scale_color_manual(values = mypal[c(1,2)]) +
# 	ylim(0, max_val*1.2) +
# 	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank())


# boxFood_m <- 
# ggplot(tbl4b_merge[gender == "male", ], aes(x = type, y = phase_mean, color = type, fill = type)) +
# 	geom_boxplot(alpha = 0.6, outlier.shape = NA) +
# 	geom_jitter(width = 0.1) +
# 	facet_wrap(.~phase) +
# 	stat_compare_means(comparisons = compare_m, label.y = max_val*1.05) +
# 	ylab("Mean food intake [g]") +
# 	theme_pubr() +
# 	scale_fill_manual(values = mypal[c(3,4)]) +
# 	scale_color_manual(values = mypal[c(3,4)]) +
# 	ylim(0, max_val*1.2) +
# 	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank())

# boxFood <- 
# ggarrange(boxFood_f, boxFood_m, nrow = 2) + margin






