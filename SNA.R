library(tidyverse)
library(igraph)
library(ggraph)
library(ggrepel)
library(shadowtext)

# Read csv
d <- as_tibble(read.csv("BiadjacencyMatrix.csv"))

# Convert to matrix and assign row names
matrix <- as.matrix(d[, 2:ncol(d)])
rownames(matrix) <- d[[colnames(d)[1]]]
matrix[is.na(matrix)] <- 0

# Graph
g_bipartite <- graph_from_biadjacency_matrix(matrix)

set.seed(123)

# Plot
p <- ggraph(g_bipartite, layout = "fr") +
  geom_edge_link(edge_colour = "grey") +
  geom_node_point(aes(fill = type, size = degree(g_bipartite)),
                  shape = 21, color = "white", stroke = 0.5) +
  scale_fill_manual(values = c("FALSE" = "#0d0dbd", "TRUE" = "#067906"),
                    labels = c("Entity", "Group"), name = "Type") +
  scale_size_continuous(range = c(2, 10), name = "Degree") +
  guides(fill = guide_legend(override.aes = list(size = 5)),
         size = guide_legend(override.aes = list(fill = "black"))) +
  geom_text_repel(aes(label = name, color = "#e00000", x = x, y = y),
                  data = . %>% filter(type == FALSE),
                  size = 2, show.legend = FALSE,
                  box.padding = 0.1, max.overlaps = 5) +
  geom_shadowtext(aes(label = name, color = "#3490af", x = x, y = y),
                  data = . %>% filter(type == TRUE),
                  repel = TRUE, size = 5, show.legend = FALSE,
                  bg.colour = "white", nudge_x = 0.1) +
  theme_graph()
print(p, width = 16, height = 8, units = "in", dpi = 300)

ggsave("SNA.png", plot = p, width = 16, height = 8, units = "in", dpi = 300)