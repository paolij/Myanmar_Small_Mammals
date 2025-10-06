setwd("/Users/julia/Dropbox/MAVIAN_Lab/Manuscripts/Myanmar/Data_Scripts_Figures/Figure_3_AlphaCoV_Concatenated_ML/")
library("ggtree")
library("phytools")
library("ggplot2")
library("ape")
library("ggbreak")

# Load the tree
ml_tree <- read.tree("AlphaCoV_Concatenated_ML.treefile")

#show node numberss
ggtree(ml_tree,ladderize = TRUE) +
  geom_text(aes(label = node))

#Finding Nodes
which(ml_tree$tip.label == "Hlawga-17|WESV")


# Visualize the rooted tree with bootstrap values as text labels
q <- ggtree(ml_tree, ladderize = TRUE) +
  geom_nodepoint(aes(subset = !is.na(as.numeric(label)) & as.numeric(label) > 90), 
                 size = 13, shape = 18, color = "black") +
  geom_treescale(x=0, y=15, color='black', fontsize = 15, linesize = 3) +
  geom_tiplab(aes(subset = node %in% c(28), label = label), size = 10, align = TRUE, offset = 0.1, fontface = 'bold', color = "black") +  
  geom_tiplab(aes(subset = !node %in% c(28), label = label), size = 10, align = TRUE, offset = 0.1) + # Regular for others
  coord_cartesian(xlim = c(0, max(nodeHeights(ml_tree)) * 1.9))
q


# Highlight a clade by MRCA
WESV <- getMRCA(ml_tree, c("Hlawga-17|WESV", "KY967734.1|WESV"))

Luchacovirus <- getMRCA(ml_tree, c("KF294380.2|LRNV"))

Setracovirus <- getMRCA(ml_tree, c("AY567487.2|HCoV_NL63"))

Minacovirus <- getMRCA(ml_tree, c("HM245925.1|MinkCoV"))

Soracovirus <- getMRCA(ml_tree, c("KY370053.1|Sa-CoV_T14"))

Rhinacovirus <- getMRCA(ml_tree, c("KJ473808.1|BtRf-AlphaCoV/YN2012", 
                         "EF203064.1|Rh-BatCoV_HKU2"))

Tegacovirus <- getMRCA(ml_tree, c("DQ848678.1|FelineCoV", 
                                   "KC175340.1|CanineCoV"))

Duvinacovirus <- getMRCA(ml_tree, c("AF304460.1|HCoV_229E", 
                                   "JQ410000.1|AlpacaCoV"))

Decacovirus <- getMRCA(ml_tree, c("JQ989270.1|BtCoV_HKU10", 
                                   "KJ473807.1|BtRf-AlphaCoV"))

Minunacovirus <- getMRCA(ml_tree, c("EU420138.1|Mi-BatCoV_1A", 
                                   "EU420139.1|Mi-BatCoV_HKU8"))

Nyctacovirus <- getMRCA(ml_tree, c("KJ473809.1|BtNv-AlphaCoV", 
                                   "MK472068.1|ACoV-WA2028"))

Amalacovirus <- getMRCA(ml_tree, c("MT663548.1|BtCoV-AMA-L-F"))


Colacovirus <- getMRCA(ml_tree, c("KF430219.1|BtCoV"))

Pedacovirus <- getMRCA(ml_tree, c("KU664503.1|PEDV", 
                                  "DQ648858.1|Sc-BatCoV_512"))

Myotacovirus <- getMRCA(ml_tree, c("KJ473806.1|BtMr-AlphaCoV"))




# Get node numbers for single tips
Luchacovirus   <- which(ml_tree$tip.label == "KF294380.2|LRNV")
Setracovirus   <- which(ml_tree$tip.label == "AY567487.2|HCoV_NL63")
Minacovirus    <- which(ml_tree$tip.label == "HM245925.1|MinkCoV")
Soracovirus    <- which(ml_tree$tip.label == "KY370053.1|Sa-CoV_T14")
Myotacovirus   <- which(ml_tree$tip.label == "KJ473806.1|BtMr-AlphaCoV")
Amalacovirus   <- which(ml_tree$tip.label == "MT663548.1|BtCoV-AMA-L-F")
Colacovirus    <- which(ml_tree$tip.label == "KF430219.1|BtCoV_CDPHE15")

# Add clade-style labels with visible bars
q <- q +
  geom_cladelabel(node = Luchacovirus, label = "Luchacovirus",
                  align = TRUE, offset = 1.5, extend = 0.5,
                  barsize = 3, fontsize = 11) +
  geom_cladelabel(node = Setracovirus, label = "Setracovirus",
                  align = TRUE, offset = 1.5, extend = 0.5,
                  barsize = 3, fontsize = 11) +
  geom_cladelabel(node = Minacovirus, label = "Minacovirus",
                  align = TRUE, offset = 1.5, extend = 0.5,
                  barsize = 3, fontsize = 11) +
  geom_cladelabel(node = Soracovirus, label = "Soracovirus",
                  align = TRUE, offset = 1.5, extend = 0.5,
                  barsize = 3, fontsize = 11) +
  geom_cladelabel(node = Myotacovirus, label = "Myotacovirus",
                  align = TRUE, offset = 1.5, extend = 0.5,
                  barsize = 3, fontsize = 11) +
  geom_cladelabel(node = Amalacovirus, label = "Amalacovirus",
                  align = TRUE, offset = 1.5, extend = 0.5,
                  barsize = 3, fontsize = 11) +
  geom_cladelabel(node = Colacovirus, label = "Colacovirus",
                  align = TRUE, offset = 1.5, extend = 0.5,
                  barsize = 3, fontsize = 11)


# Highlight and label
q <- q +
  geom_hilight(node = WESV, fill = "#4DBBD5", alpha = 0.4) +
  geom_cladelabel(node = WESV, label = "Sunacovirus", align = TRUE, offset = 1.5, barsize = 3, fontsize = 11) +
  geom_cladelabel(node = Luchacovirus, label = "Luchacovirus", align = TRUE, offset = 1.5, barsize = 3, fontsize = 11) +
  geom_cladelabel(node = Setracovirus, label = "Setracovirus", align = TRUE, offset = 1.5, barsize = 3, fontsize = 11) +
  geom_cladelabel(node = Minacovirus, label = "Minacovirus", align = TRUE, offset = 1.5, barsize = 3, fontsize = 11) +
  geom_cladelabel(node = Soracovirus, label = "Soracovirus", align = TRUE, offset = 1.5, barsize = 3, fontsize = 11) +
  geom_cladelabel(node = Rhinacovirus, label = "Rhinacovirus", align = TRUE, offset = 1.5, barsize = 3, fontsize = 11) +
  geom_cladelabel(node = Tegacovirus, label = "Tegacovirus", align = TRUE, offset = 1.5, barsize = 3, fontsize = 11) +
  geom_cladelabel(node = Duvinacovirus, label = "Duvinacovirus", align = TRUE, offset = 1.5, barsize = 3, fontsize = 11) +
  geom_cladelabel(node = Decacovirus, label = "Decacovirus", align = TRUE, offset = 1.5, barsize = 3, fontsize = 11) +
  geom_cladelabel(node = Minunacovirus, label = "Minunacovirus", align = TRUE, offset = 1.5, barsize = 3, fontsize = 11) +
  geom_cladelabel(node = Nyctacovirus, label = "Nyctacovirus", align = TRUE, offset = 1.5, barsize = 3, fontsize = 11) +
  geom_cladelabel(node = Amalacovirus, label = "Amalacovirus", align = TRUE, offset = 1.5, barsize = 3, fontsize = 11) +
  geom_cladelabel(node = Colacovirus, label = "Colacovirus", align = TRUE, offset = 1.5, barsize = 3, fontsize = 11) +
  geom_cladelabel(node = Pedacovirus, label = "Pedacovirus", align = TRUE, offset = 1.5, barsize = 3, fontsize = 11) 
  geom_cladelabel(node = Myotacovirus, label = "Myotacovirus", align = TRUE, offset = 1.5, barsize = 3, fontsize = 11) 


# Save the plot
ggsave("AlphaCoV_Concatenated_ML.pdf", width = 55, height = 15, units = "in", dpi = 600, limitsize = FALSE)