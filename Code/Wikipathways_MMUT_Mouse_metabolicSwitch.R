# wd
Dropbox/PF/Post_PhD/Zurich_2019_2022/MMUT_Mouse_metabolicSwitch
setwd("/Volumes/PFT7/Dropbox/PF/Post_PhD/Zurich_2019_2022/MMUT_Mouse_metabolicSwitch")


# libraries
require(data.table)
require(ggplot2)
require(tidyverse)
require(readxl)
require(org.Mm.eg.db)
require(ggpubr)

# color pallette (manual): order:
# female-KI/WT; female-KO/KI
# male_KI/WT; male-KO/KI
# middle color female: #FF8100
# middle color male: #008080
mypal = c("#ffa64c", "#b25a00", "#66b2b2", "#004c4c")
mylvls = c("f_Mmut-ki/wt", "f_Mmut-ko/ki", "m_Mmut-ki/wt", "m_Mmut-ko/ki")

# create figures path
fig_path <- c("Data/Fig8/Project_pathway_Wikipathway/")




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
write.csv(df, file = paste(fig_path, "WP431.csv", sep = ""))

# WP1253 = Type II interferon signaling (IFNG)
my_ids0 <- my_ids00[, WP1253]
my_ids <- my_ids0[!is.na(my_ids0)]
df <- select(mm, keys = my_ids, columns = c("ENTREZID", "SYMBOL"), keytype = "ENTREZID")
write.csv(df, file = paste(fig_path, "WP1253.csv", sep = ""))

# WP2316 = PPAR signaling pathway
my_ids0 <- my_ids00[, WP2316]
my_ids <- my_ids0[!is.na(my_ids0)]
df <- select(mm, keys = my_ids, columns = c("ENTREZID", "SYMBOL"), keytype = "ENTREZID")
write.csv(df, file = paste(fig_path, "WP2316.csv", sep = ""))

# WP295 = Electron Transport Chain
my_ids0 <- my_ids00[, WP295]
my_ids <- my_ids0[!is.na(my_ids0)]
df <- select(mm, keys = my_ids, columns = c("ENTREZID", "SYMBOL"), keytype = "ENTREZID")
write.csv(df, file = paste(fig_path, "WP295.csv", sep = ""))

# WP55 = Steroid Biosynthesis
my_ids0 <- my_ids00[, WP55]
my_ids <- my_ids0[!is.na(my_ids0)]
df <- select(mm, keys = my_ids, columns = c("ENTREZID", "SYMBOL"), keytype = "ENTREZID")
write.csv(df, file = paste(fig_path, "WP55.csv", sep = ""))

# WP495 = Glucocorticoid & Mineralcorticoid Metabolism
my_ids0 <- my_ids00[, WP495]
my_ids <- my_ids0[!is.na(my_ids0)]
df <- select(mm, keys = my_ids, columns = c("ENTREZID", "SYMBOL"), keytype = "ENTREZID")
write.csv(df, file = paste(fig_path, "WP495.csv", sep = ""))




####################################################################
####################################################################
# make some more heatmaps of the above pathways
####################################################################
####################################################################


# import microarray data for some of the panels (big file, only load once)

tbl5_supp_microarray <- data.table(read_excel("Data/Fig8/Microarray_Raw_Data.xlsx"))
colnames(tbl5_supp_microarray)





# XX. heatmap of glucocorticoid an mineralocorticoid

tbl5e_supp <- tbl5_supp_microarray[, c(1, 33:51)] # linear values
colnames(tbl5e_supp)[1] <- "gene"
fao_genes <- data.table(read.csv(paste(fig_path, "WP495.csv", sep = "")))
matcher = match(fao_genes$SYMBOL, tbl5e_supp$gene)
tbl5e_supp <- tbl5e_supp[matcher, ]
tbl5e_supp <- tbl5e_supp[!is.na(gene), ]
tbl5_mineral_supp_mat <- as.matrix(tbl5e_supp[,2:length(colnames(tbl5e_supp))])
rownames(tbl5_mineral_supp_mat) <- tbl5e_supp$gene

df_types <- data.frame(type = factor(c(rep(mylvls[4], 10), rep(mylvls[3], 9)), ordered = TRUE))
rownames(df_types) = colnames(tbl5_mineral_supp_mat)



# XX. heatmap for entirety of PPAR 

tbl5e_supp <- tbl5_supp_microarray[, c(1, 33:51)] # linear values
colnames(tbl5e_supp)[1] <- "gene"
fao_genes <- data.table(read.csv(paste(fig_path, "WP2316.csv", sep = "")))
matcher = match(fao_genes$SYMBOL, tbl5e_supp$gene)
tbl5e_supp <- tbl5e_supp[matcher, ]
tbl5e_supp <- tbl5e_supp[!is.na(gene), ]
tbl5_PPARall_supp_mat <- as.matrix(tbl5e_supp[,2:length(colnames(tbl5e_supp))])
rownames(tbl5_PPARall_supp_mat) <- tbl5e_supp$gene

df_types <- data.frame(type = factor(c(rep(mylvls[4], 10), rep(mylvls[3], 9)), ordered = TRUE))
rownames(df_types) = colnames(tbl5_PPARall_supp_mat)



# XX. heatmap for entirety of ETC 

tbl5e_supp <- tbl5_supp_microarray[, c(1, 33:51)] # linear values
colnames(tbl5e_supp)[1] <- "gene"
fao_genes <- data.table(read.csv(paste(fig_path, "WP295.csv", sep = "")))
matcher = match(fao_genes$SYMBOL, tbl5e_supp$gene)
tbl5e_supp <- tbl5e_supp[matcher, ]
tbl5e_supp <- tbl5e_supp[!is.na(gene), ]
tbl5_ETCall_supp_mat <- as.matrix(tbl5e_supp[,2:length(colnames(tbl5e_supp))])
rownames(tbl5_ETCall_supp_mat) <- tbl5e_supp$gene

df_types <- data.frame(type = factor(c(rep(mylvls[4], 10), rep(mylvls[3], 9)), ordered = TRUE))
rownames(df_types) = colnames(tbl5_ETCall_supp_mat)





####################################################################
# PLOTS AND ARRANGE AND SAVE
####################################################################

margs = c(0.5, 0.5, 0.5, 0.5)
margin = theme(plot.margin = unit(margs, "cm"))






# XX. heatmap of glucocorticoid an mineralocorticoid

library(pheatmap)
library(viridis)
library(ggplotify)

a <- c(mypal[3], mypal[4])
names(a) <- c(mylvls[3], mylvls[4])
ann_colors = list(type = a)


plt5_MinearlGluco_supp <- 
as.ggplot(pheatmap(tbl5_mineral_supp_mat, scale = "row", annotation_col = df_types, cutree_cols = 1, cutree_rows = 1, show_colnames = FALSE, annotation_colors = ann_colors, border_color = NA, color = inferno(100))) + ggtitle("Mineral and glucocorticoid (all genes)") + margin + theme(plot.title = element_text(hjust = 0.5))



# XX. heatmap for entirety of PPAR 

library(pheatmap)
library(viridis)
library(ggplotify)

a <- c(mypal[3], mypal[4])
names(a) <- c(mylvls[3], mylvls[4])
ann_colors = list(type = a)


plt5_PPARall_supp <- 
as.ggplot(pheatmap(tbl5_PPARall_supp_mat, scale = "row", annotation_col = df_types, cutree_cols = 1, cutree_rows = 1, show_colnames = FALSE, annotation_colors = ann_colors, border_color = NA, color = inferno(100))) + ggtitle("PPAR (all genes)") + margin + theme(plot.title = element_text(hjust = 0.5))



# XX. heatmap for entirety of ETC 


library(pheatmap)
library(viridis)
library(ggplotify)

a <- c(mypal[3], mypal[4])
names(a) <- c(mylvls[3], mylvls[4])
ann_colors = list(type = a)


plt5_ETCall_supp <- 
as.ggplot(pheatmap(tbl5_ETCall_supp_mat, scale = "row", annotation_col = df_types, cutree_cols = 1, cutree_rows = 1, show_colnames = FALSE, annotation_colors = ann_colors, border_color = NA, color = inferno(100))) + ggtitle("ETC (all genes)") + margin + theme(plot.title = element_text(hjust = 0.5))





plt_addon_HTMPS <-
ggarrange(plt5_MinearlGluco_supp, plt5_PPARall_supp, plt5_ETCall_supp, nrow = 1, labels = c("XX", "XX", "XX"), font.label = list(size = 18))

ggsave("Figs/v1/zplt_addon_htmps.png", plt_addon_HTMPS, device = png(), width = 20, height = 10)
dev.off()





















