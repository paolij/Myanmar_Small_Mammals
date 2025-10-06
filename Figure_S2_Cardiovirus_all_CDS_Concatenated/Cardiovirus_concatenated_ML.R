setwd("/Users/julia/Dropbox/MAVIAN_Lab/Manuscripts/Myanmar/Data_Scripts_Figures/Figure_S2_Cardiovirus_concatenated_ML/")
library("ggtree")
library("phytools")
library("ggplot2")
library("ape")
  
# Load the tree
ml_tree <- read.tree("Cardiovirus_concatenated_ML.treefile")

#show node numberss
ggtree(ml_tree, ladderize = TRUE) + 
  geom_text(aes(label = node))

#Finding Nodes
which(ml_tree$tip.label == "Hlawaga-18|Myanmar|2018")

# Visualize the rooted tree with bootstrap values as text labels
q <- ggtree(ml_tree, ladderize = TRUE) +
  geom_nodepoint(aes(subset = !is.na(as.numeric(label)) & as.numeric(label) > 90), 
                 size = 15.5, shape = 18, color = "black") +
  geom_treescale(x=0, y=55, color='black', fontsize = 40, linesize = 3) +
  geom_tiplab(aes(subset = node %in% c(101), label = label), size = 25, align = TRUE, offset = 0.08, fontface = 'bold', color = "black") +  # Bold node 7
  geom_tiplab(aes(subset = !node %in% c(101), label = label), size = 12, align = TRUE, offset = 0.08, color = "white")  # Regular for others
q

  # Load metadata
  metadata <- read.csv('Cardiovirus_concatenated_ML.csv', header = TRUE)
  unique(metadata$Country)
  unique(metadata$Date)
  unique(metadata$Species)
  unique(metadata$Virus_Name)
  unique(metadata$Accession)
  
  
  # Define colors for continent
  mycolor <- c("Cardiovirus theileri" = "#AC3EC1", "Cardiovirus ranori" = "#920000", "Cardiovirus saffoldi" = "#477BD1", "Cardiovirus dhusarah" = "#46B298", "Cardiovirus rueckerti" = "#DE478E", "Cardiovirus rudhira" = "#00BB00", "Study Sample" = "gold", "Unclassified"= "#FF8E32")

# Plot with metadata
p <- q %<+% metadata + 
  geom_tippoint(aes(color = Species), size = 20.0) + 
  scale_color_manual(values = mycolor) + 
  theme(legend.position = 'left', 
        legend.text = element_text(size = 80),  # Adjust text size
        legend.title = element_text(size = 80)) + # Adjust title size 
        coord_cartesian(xlim = c(0, max(nodeHeights(ml_tree)) * 1.4))


# Save the plot
ggsave("Cardiovirus_concatenated_ML.pdf", width = 65, height = 75, units = "in", dpi = 600, limitsize = FALSE)