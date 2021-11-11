# wd
Dropbox/PF/Post_PhD/Zurich_2019_2022/MMUT_Mouse_metabolicSwitch

# libraries
require(data.table)
require(ggplot2)
require(tidyverse)
require(readxl)
require(ggpubr)
library(pheatmap)
library(viridis)
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
# SUPP. FIGURE 5
####################################################################
####################################################################

# theme & info:
# GSEA
# a. signal intensities
# b. Mmut expression
# c. GO summary
# d. GSEA KEGG pathways
# e. htmp fao all genes
# f. htmp glucose transporters
# g. htmp glycolysis / gluconeogenesis
# h. htmp ketone metabolism
# i. htmp glutathione metabolism

####################################################################
# DATA IMPORT AND TIDY
####################################################################


# import microarray data for some of the panels (big file, only load once)

tbl5_supp_microarray <- data.table(read_excel("Data/Fig8/Microarray_Raw_Data.xlsx"))
colnames(tbl5_supp_microarray)



# a. signal intensities

tbl5a_supp <- tbl5_supp_microarray[, 53:71]
tbl5a_supp <- melt.data.table(tbl5a_supp)



# b. Mmut expression

tbl5b_supp <- tbl5_supp_microarray[, c(1, 53:71)]
colnames(tbl5b_supp)
setnames(tbl5b_supp, c("gene", rep(mylvls[4], 10), rep(mylvls[3], 9)))
tbl5b_supp <- tbl5b_supp[gene == "Mut", ]
tbl5b_supp <- melt.data.table(tbl5b_supp, id.vars = "gene")
tbl5b_supp$variable <- factor(tbl5b_supp$variable, levels = c(mylvls[3], mylvls[4]))



# c. GO summary


#DATA missing





# d. GSEA KEGG pathways

tbl5d_supp <- data.table(read_excel("Data/SuppFig5/PF_SuppFig5_GSEA_KEGG.xlsx"))
setorder(tbl5d_supp, NES)
tbl5d_supp$Description <- factor(tbl5d_supp$Description, levels = tbl5d_supp$Description)



# e. htmp fao all genes

tbl5e_supp <- tbl5_supp_microarray[, c(1, 33:51)] # linear values
colnames(tbl5e_supp)[1] <- "gene"
fao_genes <- data.table(read_excel("Data/SuppFig5/PF_SuppFig5_faoGenesALL.xlsx"))
matcher = match(fao_genes$fao_genes_all, tbl5e_supp$gene)
tbl5e_supp <- tbl5e_supp[matcher, ]
tbl5e_supp <- tbl5e_supp[!is.na(gene), ]
tbl5e_supp_mat <- as.matrix(tbl5e_supp[,2:length(colnames(tbl5e_supp))])
rownames(tbl5e_supp_mat) <- tbl5e_supp$gene

df_types <- data.frame(type = factor(c(rep(mylvls[4], 10), rep(mylvls[3], 9)), ordered = TRUE))
rownames(df_types) = colnames(tbl5e_supp_mat)





# f. htmp glucose transporters

tbl5f_supp <- tbl5_supp_microarray[, c(1, 33:51)] # linear values
colnames(tbl5f_supp)[1] <- "gene"
glc_transp_genes <- data.table(read_excel("Data/SuppFig5/PF_SuppFig5_glucoseTransporterGenesALL.xlsx"))
matcher = match(glc_transp_genes$glc_transp_genes, tbl5f_supp$gene)
tbl5f_supp <- tbl5f_supp[matcher, ]
tbl5f_supp <- tbl5f_supp[!is.na(gene), ]
tbl5f_supp_mat <- as.matrix(tbl5f_supp[,2:length(colnames(tbl5f_supp))])
rownames(tbl5f_supp_mat) <- tbl5f_supp$gene

df_types <- data.frame(type = factor(c(rep(mylvls[4], 10), rep(mylvls[3], 9)), ordered = TRUE))
rownames(df_types) = colnames(tbl5f_supp_mat)




# g. htmp glycolysis / gluconeogenesis

tbl5g_supp <- tbl5_supp_microarray[, c(1, 33:51)] # linear values
colnames(tbl5g_supp)[1] <- "gene"
gylco_genes <- data.table(read_excel("Data/SuppFig5/PF_SuppFig5_glycogluconeoGenesALL.xlsx"))
matcher = match(gylco_genes$glyo_gluco_genes_all, tbl5g_supp$gene)
tbl5g_supp <- tbl5g_supp[matcher, ]
tbl5g_supp <- tbl5g_supp[!is.na(gene), ]
tbl5g_supp_mat <- as.matrix(tbl5g_supp[,2:length(colnames(tbl5g_supp))])
rownames(tbl5g_supp_mat) <- tbl5g_supp$gene

df_types <- data.frame(type = factor(c(rep(mylvls[4], 10), rep(mylvls[3], 9)), ordered = TRUE))
rownames(df_types) = colnames(tbl5g_supp_mat)




# h. htmp ketone metabolism

tbl5h_supp <- tbl5_supp_microarray[, c(1, 33:51)] # linear values
colnames(tbl5h_supp)[1] <- "gene"
keto_genes <- data.table(read_excel("Data/SuppFig5/PF_SuppFig5_ketonegenesALL.xlsx"))
matcher = match(keto_genes$ketone_genes_all, tbl5h_supp$gene)
tbl5h_supp <- tbl5h_supp[matcher, ]
tbl5h_supp <- tbl5h_supp[!is.na(gene), ]
tbl5h_supp_mat <- as.matrix(tbl5h_supp[,2:length(colnames(tbl5h_supp))])
rownames(tbl5h_supp_mat) <- tbl5h_supp$gene

df_types <- data.frame(type = factor(c(rep(mylvls[4], 10), rep(mylvls[3], 9)), ordered = TRUE))
rownames(df_types) = colnames(tbl5h_supp_mat)



# i. htmp glutathione metabolism

tbl5i_supp <- tbl5_supp_microarray[, c(1, 33:51)] # linear values
colnames(tbl5i_supp)[1] <- "gene"
gluta_genes <- data.table(read_excel("Data/SuppFig5/PF_SuppFig5_glutathioneGenes.xlsx"))
matcher = match(gluta_genes$glutathione_genes, tbl5i_supp$gene)
tbl5i_supp <- tbl5i_supp[matcher, ]
tbl5i_supp <- tbl5i_supp[!is.na(gene), ]
tbl5i_supp_mat <- as.matrix(tbl5i_supp[,2:length(colnames(tbl5i_supp))])
rownames(tbl5i_supp_mat) <- tbl5i_supp$gene

df_types <- data.frame(type = factor(c(rep(mylvls[4], 10), rep(mylvls[3], 9)), ordered = TRUE))
rownames(df_types) = colnames(tbl5i_supp_mat)





















# further variables for plotting

margs = c(0.2, 0.2, 0.2, 0.2)
margin = theme(plot.margin = unit(margs, "cm"))




####################################################################
# PLOTS AND ARRANGE AND SAVE
####################################################################


# a. signal intensities

plt5a_supp <-
ggplot(tbl5a_supp, aes(x = variable, y = value)) +
	geom_boxplot() +
	ylab("Expression") +
	xlab("Samples") +
	theme_pubr() +
	rotate_x_text(angle = 90) +
	theme(axis.text.x = element_blank())



# b. Mmut expression
plt5b_supp <- 
ggplot(tbl5b_supp, aes(x = variable, y = value, color = variable, fill = variable)) +
	geom_boxplot(alpha = 0.6) +
	geom_jitter(width = 0.1, aes(color = variable, fill = variable)) +
	stat_compare_means(comparisons = list(c(mylvls[c(3,4)]))) +
	ylab("Mmut expression") +
	ylim(0.9*min(tbl5b_supp$value), 1.1*max(tbl5b_supp$value)) +
	theme_pubr() +
	scale_fill_manual(values = mypal[c(3,4)]) +
	scale_color_manual(values = mypal[c(3,4)]) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank()) +
	margin



# c. GO summary




# d. GSEA KEGG pathways

plt5d_supp <-
ggplot(tbl5d_supp, aes(x = NES, y = Description, fill = Size)) +
	geom_bar(stat = "identity") +
	theme_pubr() +
	scale_fill_gradient2() +
	theme(axis.title.y = element_blank()) +
	margin



# e. htmp fao all genes

library(pheatmap)
library(viridis)
library(ggplotify)

a <- c(mypal[3], mypal[4])
names(a) <- c(mylvls[3], mylvls[4])
ann_colors = list(type = a)


plt5e_supp <- 
as.ggplot(pheatmap(tbl5e_supp_mat, scale = "row", annotation_col = df_types, cutree_cols = 1, cutree_rows = 1, show_colnames = FALSE, fontsize_row = 6, annotation_colors = ann_colors, border_color = NA, color = inferno(100))) + ggtitle("Fatty acid degradation") + margin + theme(plot.title = element_text(hjust = 0.5))




# f. htmp glucose transporters

library(pheatmap)
library(viridis)
library(ggplotify)

a <- c(mypal[3], mypal[4])
names(a) <- c(mylvls[3], mylvls[4])
ann_colors = list(type = a)


plt5f_supp <- 
as.ggplot(pheatmap(tbl5f_supp_mat, scale = "row", annotation_col = df_types, cutree_cols = 1, cutree_rows = 1, show_colnames = FALSE, fontsize_row = 6, annotation_colors = ann_colors, border_color = NA, color = inferno(100))) + ggtitle("Glucose transporters") + margin + theme(plot.title = element_text(hjust = 0.5))




# g. htmp glycolysis / gluconeogenesis

library(pheatmap)
library(viridis)
library(ggplotify)

a <- c(mypal[3], mypal[4])
names(a) <- c(mylvls[3], mylvls[4])
ann_colors = list(type = a)


plt5g_supp <- 
as.ggplot(pheatmap(tbl5g_supp_mat, scale = "row", annotation_col = df_types, cutree_cols = 1, cutree_rows = 1, show_colnames = FALSE, fontsize_row = 6, annotation_colors = ann_colors, border_color = NA, color = inferno(100))) + ggtitle("Glycolysis and Gluconeogenesis") + margin + theme(plot.title = element_text(hjust = 0.5))



# h. htmp ketone metabolism

library(pheatmap)
library(viridis)
library(ggplotify)

a <- c(mypal[3], mypal[4])
names(a) <- c(mylvls[3], mylvls[4])
ann_colors = list(type = a)


plt5h_supp <- 
as.ggplot(pheatmap(tbl5h_supp_mat, scale = "row", annotation_col = df_types, cutree_cols = 1, cutree_rows = 1, show_colnames = FALSE, fontsize_row = 6, annotation_colors = ann_colors, border_color = NA, color = inferno(100))) + ggtitle("Ketone body metabolism") + margin + theme(plot.title = element_text(hjust = 0.5))



# i. htmp glutathione metabolism

library(pheatmap)
library(viridis)
library(ggplotify)

a <- c(mypal[3], mypal[4])
names(a) <- c(mylvls[3], mylvls[4])
ann_colors = list(type = a)


plt5i_supp <- 
as.ggplot(pheatmap(tbl5i_supp_mat, scale = "row", annotation_col = df_types, cutree_cols = 1, cutree_rows = 1, show_colnames = FALSE, fontsize_row = 6, annotation_colors = ann_colors, border_color = NA, color = inferno(100))) + ggtitle("Glutathione metabolism") + margin + theme(plot.title = element_text(hjust = 0.5))




plt5_top <- 
ggarrange(plt5a_supp, plt5b_supp, plt5d_supp, nrow = 1, labels = c("a", "b", "c"), widths = c(2,1,2.5))


plt5_bottom_left <- 
ggarrange(plt5i_supp, plt5f_supp, plt5h_supp, labels = c("d", "f", "h"), nrow = 3, ncol = 1, heights = c(2,1.5, 1))

plt5_bottom_right <- 
ggarrange(plt5e_supp, plt5g_supp, labels = c("e", "g"), nrow = 2, ncol = 1)


plt5_bottom <- 
ggarrange(plt5_bottom_left, plt5_bottom_right, nrow = 1)



plt5_supp <- 
ggarrange(plt5_top, plt5_bottom, nrow = 2, heights = c(1, 4))


ggsave(paste(fig_path, "SuppFig4.png", sep = ""), plt5_supp, device = png(), width = 12, height = 14, bg = "white")
dev.off()

ggsave(paste(fig_path, "SuppFig4.pdf", sep = ""), plt5_supp, device = "pdf", width = 12, height = 14, bg = "white")
dev.off()



















