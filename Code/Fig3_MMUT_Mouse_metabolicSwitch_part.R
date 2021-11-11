# wd

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


# master file

master3 <- data.table(read_excel("Data/Fig3/ParsedData_masterfile_cleaned.xlsx"))
colnames(master3)

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
	annotate(geom = "rect", xmin = start_cold, xmax = max(o2_tbl2$variable), ymin = top_value+30, ymax = top_value+50, fill = "lightgrey", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, max(o2_tbl2$variable))), y = mean(c(top_value+30, top_value+50)), color = "black", label = "cold", size = 3) +
	annotate(geom = "rect", xmin = min(o2_tbl2$variable), xmax = start_cold, ymin = top_value+30, ymax = top_value+50, fill = "white", color = "black") +
	annotate(geom = "text", x = mean(c(start_cold, min(o2_tbl2$variable))), y = mean(c(top_value+30, top_value+50)), color = "black", label = "neutral", size = 3) +
	annotate(geom = "rect", xmin = start_cold, xmax = max(o2_tbl2$variable), ymin = 0, ymax = top_value, fill = "lightgrey") +
	geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), alpha = 0.6) +
	geom_point(shape = 17, size = 1) +
	annotate(geom = "segment", x = lights_on, xend = lights_on, y = 0, yend = top_value, linetype = "dashed") +
	annotate(geom = "segment", x = lights_off, xend = lights_off, y = 0, yend = top_value, linetype = "dashed") +
	annotate(geom = "text", x = lights_on, y = top_value+15, label = "on", angle = 90, size = 3) +
	annotate(geom = "text", x = lights_off, y = top_value+15, label = "off", angle = 90, size = 3) +
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



compare <- list(c(mylvls[1], mylvls[2]), c(mylvls[3], mylvls[4]))
min_val <- min(o2_tbl3_combo_adj$value)
max_val <- max(o2_tbl3_combo_adj$value)


adj_o2_mean_plt <- 
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
	theme(legend.title = element_blank(), legend.position = "right", axis.title.x = element_blank(), axis.text.x = element_blank())

library(cowplot)
ggarrange(o2_raw_plt, adj_o2_mean_plt, widths = c(1, 1.2))

ggsave(paste(fig_path, "Fig3_part.png", sep = ""), device = png(), width = 9, height = 3)
dev.off()


