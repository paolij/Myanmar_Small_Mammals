setwd("/Users/julia/Dropbox/MAVIAN_Lab/Manuscripts/Myanmar/Data_Scripts_Figures/Figure_S1_WESV_EMN_ML/")
library("ggtree")
library("phytools")
library("ggplot2")
library("ape")
library("ggbreak")

# Load the tree
ml_tree <- read.tree("E_fixed.nwk")

#show node numberss
ggtree(ml_tree,ladderize = TRUE) +
  geom_text(aes(label = node))

#Finding Nodes
which(ml_tree$tip.label == "Hlawga-17|Crocidura_fuliginosa|2017")

# Visualize the rooted tree with bootstrap values as text labels
q <- ggtree(ml_tree, ladderize = TRUE) +
  geom_nodepoint(aes(subset = !is.na(as.numeric(label)) & as.numeric(label) > 90), 
                 size = 15, shape = 18, color = "black") +
  geom_treescale(x=0, y=10, color='black', fontsize = 18, linesize = 3) +
  geom_tiplab(aes(subset = node %in% c(1), label = label), size = 15, align = TRUE, offset = 0.1, fontface = 'bold', color = "black") +  
  geom_tiplab(aes(subset = !node %in% c(1), label = label), size = 15, align = TRUE, offset = 0.1)  # Regular for others
q

# Load metadata
metadata <- read.csv('WESV.csv', header = TRUE)
unique(metadata$isolate)
unique(metadata$Country)
unique(metadata$host)
unique(metadata$date)
unique(metadata$accession)
unique(metadata$accession_full)

# Define colors for continent
mycolor <- c("Myanmar" = "#4DBBD5", "China" = "#E64B35")

# Plot with metadata
p <- q %<+% metadata + 
  geom_tippoint(aes(color = Country), size = 17.0) + 
  scale_color_manual(values = mycolor) + 
  theme(legend.position = 'left', 
        legend.text = element_text(size = 60),  # Adjust text size
        legend.title = element_text(size = 60)) + # Adjust title size
  guides(color = guide_legend(override.aes = list(size = 10))) +
  coord_cartesian(xlim = c(0, max(nodeHeights(ml_tree)) * 1.9))

# Adjust the x-axis limit if needed
#p + ggplot2::xlim(0, 1)

# Save the plot
ggsave("WESV_E_shrewtibet_July2.pdf", width = 55, height = 15, units = "in", dpi = 600, limitsize = FALSE)