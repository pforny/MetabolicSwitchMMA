### METABOLIC SWITCH IN A MOUSE MODEL OF METHYLMALONIC ACIDURIA
# FIGURE 4
# + Supp. Fig. 3

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
mylvls = c("f_Mmut-ki/wt", "f_Mmut-ko/ki", "m_Mmut-ki/wt", "m_Mmut-ko/ki")

# create figures path
system("mkdir Figs")
system("mkdir Figs/v19")
fig_path <- c("Figs/v19/")

system("mkdir Figs")
system("mkdir Figs/tablesv19")
fig_tbl_path <- c("Figs/tablesv19/")




light_cycle <- c(5, 17)
lights_off <- light_cycle[1]
lights_on <- light_cycle[2]



####################################################################
####################################################################
# FIGURE 4
####################################################################
####################################################################

# theme & info:
# a: O2 consumption 
# b: RER
# c: carbohydrate oxidation
# d: lipid oxidation (based on O2 and CHO oxidation)
# e: distance
# f: food intake

####################################################################




margs = c(0.5, 0.5, 0.5, 0.5)
margin = theme(plot.margin = unit(margs, "cm"))

phases = as_labeller(c('early_light' = "light 1", 'dark' = "dark", 'late_light' = "light 2"))




# master file

master4 <- data.table(read_excel("Data/Fig4/indirectCalorimetry_summaryFile_PFedition.xlsx"))



# add mean body mass
colnames(master4)
master4[, bodyMass := (master4[, 13]+master4[, 14])/2]








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


o2_tbl3 <- cbind(o2_tbl_early1, o2_tbl_dark1, o2_tbl_late1, master4$bodyMass, master4$typeReal)
setnames(o2_tbl3, c("early_light", "dark", "late_light", "bodyMass", "type"))
o2_tbl3 <- data.table(o2_tbl3)
o2_tbl3_melt <- melt.data.table(o2_tbl3, id.vars = c("type", "bodyMass"))


supp_o2 <- 
ggplot(o2_tbl3_melt, aes(x = bodyMass, y = value, color = type)) +
	geom_point() +
	geom_smooth(method = "lm", se = FALSE) +
	stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, size = 3, aes(label = paste(..p.label.., sep = "~`,`~")), cor.coef.name = "rho", show.legend = FALSE)+
	ylab(expression("Mean O"[2]*" consumption [mL/min]")) +
	xlab("Body mass [g]") +
	facet_wrap(.~variable, labeller = phases) +
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
mod_early_f <- lm(early_light ~ bodyMass + type, data = o2_tbl3_f)
mod_early_m <- lm(early_light ~ bodyMass + type, data = o2_tbl3_m)
summary(mod_early_f)
summary(mod_early_m)
mod_dark_f <- lm(dark ~ bodyMass + type, data = o2_tbl3_f)
mod_dark_m <- lm(dark ~ bodyMass + type, data = o2_tbl3_m)
summary(mod_dark_f)
summary(mod_dark_m)
mod_late_f <- lm(late_light ~ bodyMass + type, data = o2_tbl3_f)
mod_late_m <- lm(late_light ~ bodyMass + type, data = o2_tbl3_m)
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
	facet_wrap(.~variable, labeller = phases) +
	stat_compare_means(comparisons = compare, size = 3, method = "t.test") +
	ylab(expression(atop("Body mass adjusted","mean"~"O"[2]*" cons. [mL/min]"))) +
	ylim(min_val, max_val*1.2) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white"))

ind_calor_O2 <- 
o2_raw_plt + adj_o2_mean_plt









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


food_tbl3 <- cbind(food_tbl_early1, food_tbl_dark1, food_tbl_late1, master4$bodyMass, master4$typeReal)
setnames(food_tbl3, c("early_light", "dark", "late_light", "bodyMass", "type"))
food_tbl3 <- data.table(food_tbl3)
food_tbl3_melt <- melt.data.table(food_tbl3, id.vars = c("type", "bodyMass"))


supp_food <- 
ggplot(food_tbl3_melt, aes(x = bodyMass, y = value, color = type)) +
	geom_point() +
	geom_smooth(method = "lm", se = FALSE) +
	stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, size = 3, aes(label = paste(..p.label.., sep = "~`,`~")), cor.coef.name = "rho", show.legend = FALSE)+
	ylab("Mean food intake [g]") +
	xlab("Body mass [g]") +
	facet_wrap(.~variable, labeller = phases) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme_pubr() +
	theme(strip.background = element_rect("white"))




#######
# correct food intake with body mass

# split the sexes
food_tbl3_f <- food_tbl3[type == mylvls[1] | type == mylvls[2], ]
food_tbl3_m <- food_tbl3[type != mylvls[1] & type != mylvls[2], ]

# average body mass
food_tbl3_f$avLM <- mean(food_tbl3$bodyMass, na.rm = TRUE)
food_tbl3_m$avLM <- mean(food_tbl3$bodyMass, na.rm = TRUE)

# linear model for food intake
mod_early_f <- lm(early_light ~ bodyMass + type, data = food_tbl3_f)
mod_early_m <- lm(early_light ~ bodyMass + type, data = food_tbl3_m)
summary(mod_early_f)
summary(mod_early_m)
mod_dark_f <- lm(dark ~ bodyMass + type, data = food_tbl3_f)
mod_dark_m <- lm(dark ~ bodyMass + type, data = food_tbl3_m)
summary(mod_dark_f)
summary(mod_dark_m)
mod_late_f <- lm(late_light ~ bodyMass + type, data = food_tbl3_f)
mod_late_m <- lm(late_light ~ bodyMass + type, data = food_tbl3_m)
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
	facet_wrap(.~variable, labeller = phases) +
	stat_compare_means(comparisons = compare, size = 3, method = "t.test") +
	ylab(expression(atop("Body mass adjusted","mean food intake [g]"))) +
	ylim(min_val, max_val*1.3) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white"))

ind_calor_FOOD <- 
food_raw_plt + adj_food_mean_plt










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


lipid_tbl3 <- cbind(lipid_tbl_early1, lipid_tbl_dark1, lipid_tbl_late1, master4$bodyMass, master4$typeReal)
setnames(lipid_tbl3, c("early_light", "dark", "late_light", "bodyMass", "type"))
lipid_tbl3 <- data.table(lipid_tbl3)
lipid_tbl3_melt <- melt.data.table(lipid_tbl3, id.vars = c("type", "bodyMass"))


supp_lox <- 
ggplot(lipid_tbl3_melt, aes(x = bodyMass, y = value, color = type)) +
	geom_point() +
	geom_smooth(method = "lm", se = FALSE) +
	stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, size = 3, aes(label = paste(..p.label.., sep = "~`,`~")), cor.coef.name = "rho", show.legend = FALSE)+
	ylab("Mean lipid oxidation [mL/h]") +
	xlab("Body mass [g]") +
	facet_wrap(.~variable, labeller = phases) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme_pubr() +
	theme(strip.background = element_rect("white"))




#######
# correct lipid intake with body mass

# split the sexes
lipid_tbl3_f <- lipid_tbl3[type == mylvls[1] | type == mylvls[2], ]
lipid_tbl3_m <- lipid_tbl3[type != mylvls[1] & type != mylvls[2], ]

# average body mass
lipid_tbl3_f$avLM <- mean(lipid_tbl3$bodyMass, na.rm = TRUE)
lipid_tbl3_m$avLM <- mean(lipid_tbl3$bodyMass, na.rm = TRUE)

# linear model for lipid intake
mod_early_f <- lm(early_light ~ bodyMass + type, data = lipid_tbl3_f)
mod_early_m <- lm(early_light ~ bodyMass + type, data = lipid_tbl3_m)
summary(mod_early_f)
summary(mod_early_m)
mod_dark_f <- lm(dark ~ bodyMass + type, data = lipid_tbl3_f)
mod_dark_m <- lm(dark ~ bodyMass + type, data = lipid_tbl3_m)
summary(mod_dark_f)
summary(mod_dark_m)
mod_late_f <- lm(late_light ~ bodyMass + type, data = lipid_tbl3_f)
mod_late_m <- lm(late_light ~ bodyMass + type, data = lipid_tbl3_m)
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
	facet_wrap(.~variable, labeller = phases) +
	stat_compare_means(comparisons = compare, size = 3, method = "t.test") +
	ylab(expression(atop("Body mass adjusted","mean lipid oxidation [mL/h]"))) +
	ylim(min_val, max_val*1.3) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white"))



ind_calor_LIPID <- 
lipid_raw_plt + adj_lipid_mean_plt












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


cho_tbl3 <- cbind(cho_tbl_early1, cho_tbl_dark1, cho_tbl_late1, master4$bodyMass, master4$typeReal)
setnames(cho_tbl3, c("early_light", "dark", "late_light", "bodyMass", "type"))
cho_tbl3 <- data.table(cho_tbl3)
cho_tbl3_melt <- melt.data.table(cho_tbl3, id.vars = c("type", "bodyMass"))


supp_cho <- 
ggplot(cho_tbl3_melt, aes(x = bodyMass, y = value, color = type)) +
	geom_point() +
	geom_smooth(method = "lm", se = FALSE) +
	stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, size = 3, aes(label = paste(..p.label.., sep = "~`,`~")), cor.coef.name = "rho", show.legend = FALSE)+
	ylab("Mean CHO oxidation [mL/h]") +
	xlab("Body mass [g]") +
	facet_wrap(.~variable, labeller = phases) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme_pubr() +
	theme(strip.background = element_rect("white"))




#######
# correct cho intake with lean mass

# split the sexes
cho_tbl3_f <- cho_tbl3[type == mylvls[1] | type == mylvls[2], ]
cho_tbl3_m <- cho_tbl3[type != mylvls[1] & type != mylvls[2], ]

# average lean mass
cho_tbl3_f$avLM <- mean(cho_tbl3$bodyMass, na.rm = TRUE)
cho_tbl3_m$avLM <- mean(cho_tbl3$bodyMass, na.rm = TRUE)

# linear model for cho intake
mod_early_f <- lm(early_light ~ bodyMass + type, data = cho_tbl3_f)
mod_early_m <- lm(early_light ~ bodyMass + type, data = cho_tbl3_m)
summary(mod_early_f)
summary(mod_early_m)
mod_dark_f <- lm(dark ~ bodyMass + type, data = cho_tbl3_f)
mod_dark_m <- lm(dark ~ bodyMass + type, data = cho_tbl3_m)
summary(mod_dark_f)
summary(mod_dark_m)
mod_late_f <- lm(late_light ~ bodyMass + type, data = cho_tbl3_f)
mod_late_m <- lm(late_light ~ bodyMass + type, data = cho_tbl3_m)
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
	facet_wrap(.~variable, labeller = phases) +
	stat_compare_means(comparisons = compare, size = 3, method = "t.test") +
	ylab(expression(atop("Body mass adjusted","mean CHO oxidation [mL/h]"))) +
	ylim(min_val, max_val*1.3) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white"))

ind_calor_CHO <- 
cho_raw_plt + adj_cho_mean_plt











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
	facet_wrap(.~variable, labeller = phases) +
	stat_compare_means(comparisons = compare, size = 3, method = "t.test") +
	ylab(expression("Mean RER"~"[VCO"[2]/VO[2]*"]")) +
	ylim(min_val, max_val*1.05) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white"))

ind_calor_RER <- 
rer_raw_plt + adj_rer_mean_plt













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
	facet_wrap(.~variable, labeller = phases) +
	stat_compare_means(comparisons = compare, size = 3, method = "t.test") +
	ylab("Mean distance [cm]") +
	ylim(min_val, max_val*1.2) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal) +
	scale_color_manual(values = mypal) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.background = element_rect(fill = "white"))

ind_calor_DIST <- 
dist_raw_plt + adj_dist_mean_plt







plt4 <- 
ggarrange(ind_calor_O2+margin, ind_calor_RER+margin, ind_calor_CHO+margin, ind_calor_LIPID+margin, ind_calor_DIST+margin,ind_calor_FOOD+margin, nrow = 3, ncol = 2, labels = "auto", common.legend = TRUE, align = "hv")


ggsave(paste(fig_path, "Fig4.png", sep = ""), plt4, device = png(), width = 15, height = 10, bg = "white")
dev.off()

ggsave(paste(fig_path, "Fig4.pdf", sep = ""), plt4, device = "pdf", width = 15, height = 10, bg = "white")
dev.off()










####################################################################
####################################################################
# Supp. Fig. 3
####################################################################
####################################################################

# theme & info:
# plots to depict linear models used in Fig. 4 for:
	# O2 consumption
	# CHO oxidation
	# lipid oxidation
	# food intake


####################################################################
# panels are plotted above with the respective Fig. 3 panel


supp_fig3 <- 
ggarrange(supp_o2+margin, supp_cho+margin, supp_lox+margin, supp_food+margin, common.legend = TRUE, labels = "auto")


ggsave(paste(fig_path, "SuppFig3.png", sep = ""), supp_fig3, device = png(), width = 9, height = 6.5, bg = "white")
dev.off()

ggsave(paste(fig_path, "SuppFig3.pdf", sep = ""), supp_fig3, device = "pdf", width = 9, height = 6.5, bg = "white")
dev.off()


