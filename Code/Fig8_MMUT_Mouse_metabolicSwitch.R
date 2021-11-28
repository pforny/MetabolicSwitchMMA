### METABOLIC SWITCH IN A MOUSE MODEL OF METHYLMALONIC ACIDURIA
# FIGURE 8

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
require(org.Mm.eg.db)
library(RColorBrewer)
library(pheatmap)
library(viridis)
library(ggplotify)

# color pallette (manual): order:
# female-KI/WT; female-KO/KI
# male_KI/WT; male-KO/KI
mypal = c("#ffa64c", "#b25a00", "#66b2b2", "#004c4c")
mylvls = c("f_Mmut-ki/wt", "f_Mmut-ko/ki", "m_Mmut-ki/wt", "m_Mmut-ko/ki")

# create figures path
system("mkdir Figs")
system("mkdir Figs/v20")
fig_path <- c("Figs/v20/")

system("mkdir Figs")
system("mkdir Figs/tablesv20")
fig_tbl_path <- c("Figs/tablesv20/")


fig_path_wiki <- c("Data/Fig8/Project_pathway_Wikipathway/")


####################################################################
####################################################################
# FIGURE 8
####################################################################
####################################################################

# theme & info:
# GSEA
# a. GSEA wikipathway
# b. fgf21 pparg
# c. fao
# d. fao htmp
# e. glycolysis
# f. ketones
# g. glutathione htmp

####################################################################
# DATA IMPORT AND TIDY
####################################################################


# import microarray data for some of the panels (big file, only load once)

tbl8_microarray <- data.table(read_excel("Data/Fig8/Microarray_Raw_Data.xlsx"))


## PPAR all genes list

# important wiki pathways to extract the gene names from
# WP431 = Nuclear receptors in lipid metabolism and toxicity
# WP1253 = Type II interferon signaling (IFNG)
# WP2316 = PPAR signaling pathway
# WP295 = Electron Transport Chain
# WP55 = Steroid Biosynthesis
# WP495 = Glucocorticoid & Mineralcorticoid Metabolism


wps <- c("WP431", "WP1253", "WP2316", "WP295", "WP55", "WP495")

wp_tbl <- data.table(read_excel("Data/Fig8/Project_pathway_Wikipathway/mmusculus_pathway_Wikipathway_entrezgene.xlsx", col_names = FALSE))
nrow(wp_tbl)
ncol(wp_tbl)

wp_tbl_trans <- dcast(melt(wp_tbl, id.vars = "...1"), variable ~ ...1)

wp_tbl_trans <- wp_tbl_trans[, 1 := NULL]
wp_tbl_trans <- wp_tbl_trans[-1, ]
colnames(wp_tbl_trans)

# load mouse genes database
mm <- org.Mm.eg.db

# load subset of table
my_ids00 <- wp_tbl_trans[, ..wps]


# WP431 = Nuclear receptors in lipid metabolism and toxicity
my_ids0 <- my_ids00[, WP431]
my_ids <- my_ids0[!is.na(my_ids0)]
df <- select(mm, keys = my_ids, columns = c("ENTREZID", "SYMBOL"), keytype = "ENTREZID")
write.csv(df, file = paste(fig_path_wiki, "WP431.csv", sep = ""))

# WP1253 = Type II interferon signaling (IFNG)
my_ids0 <- my_ids00[, WP1253]
my_ids <- my_ids0[!is.na(my_ids0)]
df <- select(mm, keys = my_ids, columns = c("ENTREZID", "SYMBOL"), keytype = "ENTREZID")
write.csv(df, file = paste(fig_path_wiki, "WP1253.csv", sep = ""))

# WP2316 = PPAR signaling pathway
my_ids0 <- my_ids00[, WP2316]
my_ids <- my_ids0[!is.na(my_ids0)]
df <- select(mm, keys = my_ids, columns = c("ENTREZID", "SYMBOL"), keytype = "ENTREZID")
write.csv(df, file = paste(fig_path_wiki, "WP2316.csv", sep = ""))

# WP295 = Electron Transport Chain
my_ids0 <- my_ids00[, WP295]
my_ids <- my_ids0[!is.na(my_ids0)]
df <- select(mm, keys = my_ids, columns = c("ENTREZID", "SYMBOL"), keytype = "ENTREZID")
write.csv(df, file = paste(fig_path_wiki, "WP295.csv", sep = ""))

# WP55 = Steroid Biosynthesis
my_ids0 <- my_ids00[, WP55]
my_ids <- my_ids0[!is.na(my_ids0)]
df <- select(mm, keys = my_ids, columns = c("ENTREZID", "SYMBOL"), keytype = "ENTREZID")
write.csv(df, file = paste(fig_path_wiki, "WP55.csv", sep = ""))

# WP495 = Glucocorticoid & Mineralcorticoid Metabolism
my_ids0 <- my_ids00[, WP495]
my_ids <- my_ids0[!is.na(my_ids0)]
df <- select(mm, keys = my_ids, columns = c("ENTREZID", "SYMBOL"), keytype = "ENTREZID")
write.csv(df, file = paste(fig_path_wiki, "WP495.csv", sep = ""))



# heatmap for entirety of PPAR 

tbl5e_supp <- tbl8_microarray[, c(1, 33:51)] # linear values
colnames(tbl5e_supp)[1] <- "gene"
fao_genes <- data.table(read.csv(paste(fig_path_wiki, "WP2316.csv", sep = "")))
matcher = match(fao_genes$SYMBOL, tbl5e_supp$gene)
tbl5e_supp <- tbl5e_supp[matcher, ]
tbl5e_supp <- tbl5e_supp[!is.na(gene), ]
tbl5_PPARall_supp_mat <- as.matrix(tbl5e_supp[,2:length(colnames(tbl5e_supp))])
rownames(tbl5_PPARall_supp_mat) <- tbl5e_supp$gene

df_types <- data.frame(type = factor(c(rep(mylvls[4], 10), rep(mylvls[3], 9)), ordered = TRUE))
rownames(df_types) = colnames(tbl5_PPARall_supp_mat)







# a. GSEA wikipathway

tbl8a <- data.table(read_excel("Data/Fig8/PF_Fig8A_GSEAWikipath.xlsx"))
setorder(tbl8a, NES)
tbl8a$Description <- factor(tbl8a$Description, levels = tbl8a$Description)



# b. fgk21 pparg

tbl8b <- data.table(read_excel("Data/Fig8/PF_Fig8B_qPCR.xlsx"))
tbl8b <- melt.data.table(tbl8b, id.vars = "gene")
setnames(tbl8b, c("gene", "type", "value"))
tbl8b$value <- as.numeric(tbl8b$value)
tbl8b <- tbl8b[!is.na(tbl8b$value), ]



# c. fao

tbl8c <- data.table(read_excel("Data/Fig8/PF_Fig8C_FAOqPCR.xlsx"))
tbl8c <- melt.data.table(tbl8c, id.vars = c("gene", "location"))
setnames(tbl8c, c("gene", "location", "type", "value"))
tbl8c$value <- as.numeric(tbl8c$value)
tbl8c <- tbl8c[!is.na(tbl8c$value), ]
tbl8c$gene <- factor(tbl8c$gene, levels = c("Ehhadh", "Acaa1b", "Cyp4a10", "Cyp4a31", "Acadm", "Acadl"))
tbl8c$location <- factor(tbl8c$location, levels = c("peroxisomal", "microsomal", "mitochondrial"))



# d. fao htmp

tbl8d <- tbl8_microarray[, c(1, 33:51)] # linear values
colnames(tbl8d)[1] <- "gene"
fao_genes <- data.table(read_excel("Data/Fig8/PF_Fig8D_faoGenes.xlsx"))
matcher = match(fao_genes$fao_genes, tbl8d$gene)
tbl8d <- tbl8d[matcher, ]
tbl8d_mat <- as.matrix(tbl8d[,2:length(colnames(tbl8d))])
rownames(tbl8d_mat) <- tbl8d$gene

df_types <- data.frame(type = factor(c(rep(mylvls[4], 10), rep(mylvls[3], 9)), ordered = TRUE))
rownames(df_types) = colnames(tbl8d_mat)



# e. glycolysis

tbl8e <- data.table(read_excel("Data/Fig8/PF_Fig8E_glycolysisqPCR.xlsx"))
tbl8e <- melt.data.table(tbl8e, id.vars = "gene")
setnames(tbl8e, c("gene", "type", "value"))
tbl8e$value <- as.numeric(tbl8e$value)
tbl8e <- tbl8e[!is.na(tbl8e$value), ]



# f. ketones

tbl8f <- data.table(read_excel("Data/Fig8/PF_Fig8F_ketonesqPCR.xlsx"))
tbl8f <- melt.data.table(tbl8f, id.vars = "gene")
setnames(tbl8f, c("gene", "type", "value"))
tbl8f$value <- as.numeric(tbl8f$value)
tbl8f <- tbl8f[!is.na(tbl8f$value), ]





# g. glutathione htmp

tbl8g <- tbl8_microarray[, c(1, 33:51)] # linear values
colnames(tbl8g)[1] <- "gene"

glut_genes <- data.table(read_excel("Data/Fig8/PF_Fig8D_glutathioneGenes.xlsx"))
matcher = match(glut_genes$glutathione_genes, tbl8g$gene)
tbl8g <- tbl8g[matcher, ]
tbl8g_mat <- as.matrix(tbl8g[,2:length(colnames(tbl8g))])
rownames(tbl8g_mat) <- tbl8g$gene

df_types <- data.frame(type = factor(c(rep(mylvls[4], 10), rep(mylvls[3], 9)), ordered = TRUE))
rownames(df_types) = colnames(tbl8g_mat)



# gg. ETC htmp

tbl8gg <- tbl8_microarray[, c(1, 33:51)] # linear values
colnames(tbl8gg)[1] <- "gene"

etc_genes <- data.table(read_excel("Data/Fig8/PF_ETC_genes.xlsx"))
matcher = match(etc_genes$etc_genes, tbl8gg$gene)
tbl8gg <- tbl8gg[matcher, ]
newmatcher <- match(tbl8gg$gene, etc_genes$etc_genes)
tbl8gg[, complex := etc_genes[newmatcher, ]$complex]
tbl8gg <- tbl8gg[!is.na(gene), ]
setorder(tbl8gg, complex)
tbl8gg[, complex := NULL]
tbl8gg_mat <- as.matrix(tbl8gg[,2:length(colnames(tbl8gg))])
rownames(tbl8gg_mat) <- tbl8gg$gene








####################################################################
# PLOTS AND ARRANGE AND SAVE
####################################################################

margs = c(0.5, 0.5, 0.5, 0.5)
margin = theme(plot.margin = unit(margs, "cm"))




compare <- list(c("m_Mmut-ki/wt", "m_Mmut-ko/ki"))

# a. GSEA wikipathway

plt8a <-
ggplot(tbl8a, aes(x = NES, y = Description, fill = Size)) +
	geom_bar(stat = "identity") +
	theme_pubr() +
	scale_fill_gradient2() +
	theme(axis.title.y = element_blank()) +
	margin



# b. fgf21

plt8b <-
ggplot(tbl8b, aes(x = type, y = value)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA, aes(color = type, fill = type)) +
	geom_jitter(width = 0.1, aes(color = type, fill = type)) +
	facet_wrap(.~gene) +
	stat_compare_means(comparisons = compare) +
	ylab("Normalised expression") +
	ylim(0, (1.1*max(tbl8b$value))) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal[c(3,4)]) +
	scale_color_manual(values = mypal[c(3,4)]) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text = element_text(face = "italic"), strip.background = element_rect(fill = "white")) +
	margin




# xx. PPAR all genes

a <- c(mypal[3], mypal[4])
names(a) <- c(mylvls[3], mylvls[4])
ann_colors = list(type = a)


plt5_PPARall_supp <- 
as.ggplot(pheatmap(tbl5_PPARall_supp_mat, fontsize_row = 6, scale = "row", annotation_col = df_types, cutree_cols = 2, cutree_rows = 1, annotation_legend = FALSE, treeheight_col = 0, show_colnames = FALSE, annotation_colors = ann_colors, border_color = NA, color = inferno(100))) + ggtitle("PPAR") + margin + theme(plot.title = element_text(hjust = 0.5))




# c. fao

plt8c <-
ggplot(tbl8c, aes(x = type, y = value)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA, aes(color = type, fill = type)) +
	geom_jitter(width = 0.1, aes(color = type, fill = type)) +
	facet_grid(.~location+gene) +
	stat_compare_means(comparisons = compare) +
	ylim(0,7.9) +
	ylab("Normalised expression") +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal[c(3,4)]) +
	scale_color_manual(values = mypal[c(3,4)]) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text = element_text(face = "italic"), strip.background = element_rect(fill = "white")) +
	margin



# d. fao htmp

a <- c(mypal[3], mypal[4])
names(a) <- c(mylvls[3], mylvls[4])
ann_colors = list(type = a)


plt8d <- 
as.ggplot(pheatmap(tbl8d_mat, fontsize_row = 6, scale = "row", annotation_col = df_types, cutree_cols = 2, cutree_rows = 2, show_colnames = FALSE, annotation_legend = FALSE, treeheight_col = 0, annotation_colors = ann_colors, border_color = NA, color = inferno(100))) + ggtitle("Fatty acid degradation") + margin + theme(plot.title = element_text(hjust = 0.5))





# e. glycolysis

plt8e <-
ggplot(tbl8e, aes(x = type, y = value)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA, aes(color = type, fill = type)) +
	geom_jitter(width = 0.1, aes(color = type, fill = type)) +
	facet_wrap(.~gene) +
	stat_compare_means(comparisons = compare) +
	ylab("Normalised expression") +
	ylim(0, (1.1*max(tbl8e$value))) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal[c(3,4)]) +
	scale_color_manual(values = mypal[c(3,4)]) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text = element_text(face = "italic"), strip.background = element_rect(fill = "white")) +
	margin



# f. ketones

plt8f <-
ggplot(tbl8f, aes(x = type, y = value)) +
	geom_boxplot(alpha = 0.6, outlier.shape = NA, aes(color = type, fill = type)) +
	geom_jitter(width = 0.1, aes(color = type, fill = type)) +
	facet_wrap(.~gene) +
	stat_compare_means(comparisons = compare) +
	ylab("Normalised expression") +
	ylim(0, (max(tbl8f$value)+0.1*max(tbl8f$value))) +
	theme_pubr() +
	rotate_x_text(angle = 45) +
	scale_fill_manual(values = mypal[c(3,4)]) +
	scale_color_manual(values = mypal[c(3,4)]) +
	theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), strip.text = element_text(face = "italic"), strip.background = element_rect(fill = "white")) +
	margin




# gg. ETC htmp

colnames(tbl8gg)
df_types <- data.frame(type = factor(c(rep(mylvls[4], 10), rep(mylvls[3], 9)), ordered = TRUE))
rownames(df_types) = colnames(tbl8gg_mat)

newmatcher2 <- match(rownames(tbl8gg_mat), etc_genes$etc_genes)
complex <- etc_genes[newmatcher2, ]$complex
df_complex <- data.frame(complex = complex)
rownames(df_complex) = rownames(tbl8gg_mat)
df_complex <- df_complex[order(df_complex$complex), , drop = FALSE]



color_type <- c(mypal[3], mypal[4])
names(color_type) <- c(mylvls[3], mylvls[4])
color_complex <- brewer.pal(n = 5, name = 'Purples')
names(color_complex) <- sort(unique(df_complex$complex))
ann_colors = list(type = color_type, complex = color_complex)


plt8gg <- 
as.ggplot(pheatmap(tbl8gg_mat, fontsize_row = 6, scale = "row", cluster_cols = TRUE, cluster_rows = FALSE, annotation_col = df_types, annotation_row = df_complex, annotation_names_row = TRUE, cutree_cols = 2, cutree_rows = 1, show_colnames = FALSE, annotation_colors = ann_colors, border_color = NA, color = inferno(100))) + ggtitle("Electron transport chain") + margin + theme(plot.title = element_text(hjust = 0.5))






plt8_1 <- 
ggarrange(plt8a, plt8gg, widths = c(1, 1), labels = c("a", "b"), nrow = 1, font.label = list(size = 18))

plt8_left <- 
ggarrange(plt5_PPARall_supp, plt8d, nrow = 2, heights = c(3.2,2), labels = c("c", "e"), font.label = list(size = 18))

plt8_right <- 
ggarrange(plt8b, plt8c, ggarrange(plt8e, plt8f, labels = c("g", "h")), labels = c("d", "f", ""), nrow = 3, font.label = list(size = 18))


plt8_bottom <-
ggarrange(plt8_left, plt8_right, nrow = 1)

plt8 <- 
ggarrange(plt8_1, plt8_bottom, nrow = 2, heights = c(1,2))



ggsave(paste(fig_path, "Fig8.png", sep = ""), plt8, device = png(), width = 12, height = 16, bg = "white")
dev.off()

ggsave(paste(fig_path, "Fig8.pdf", sep = ""), plt8, device = "pdf", width = 12, height = 16, bg = "white")
dev.off()



















