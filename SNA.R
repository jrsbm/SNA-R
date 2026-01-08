library(tidyverse)
library(igraph)
library(ggraph)

# Read csv
d <- as_tibble(read.csv('biadjacency matrix.csv'))

# Convert to matrix and assign row names
matrix <- as.matrix(d[,2:ncol(d)])
rownames(matrix) <- d[[colnames(d)[1]]]
matrix[is.na(matrix)] <- 0

# Graph
g_bipartite <- graph_from_biadjacency_matrix(matrix)
# Map size as degree or betweenness
V(g_bipartite)$betweenness <- betweenness(g_bipartite,
                                          weights = E(g_bipartite)$weight)

set.seed(123)

V(g_bipartite)$type2 <- ifelse(V(g_bipartite)$type, "red", "lightblue")

# Plot
p <- ggraph(g_bipartite, layout = "fr") +
  geom_edge_link(edge_colour = "grey") +
  geom_node_point(aes(fill = type, size = degree(g_bipartite)),
                  shape = 21, color = "white", stroke = 0.5) +
  geom_node_text(aes(label = name, color = type),
                 repel = TRUE, size = 2, show.legend = FALSE) +
  scale_fill_manual(values = c("FALSE" = "#0d0dbd", "TRUE" = "#067906"),
                    labels = c("Entity", "Group")) +
  scale_colour_manual(values = c("FALSE" = "#3490af", "TRUE" = "#e21111")) +
  theme_graph() +
  labs(title = "Network Graph")
print(p, width = 16, height = 8, units = "in", dpi = 300)

ggsave("SNA.png", plot = p, width = 16, height = 8, units = "in", dpi = 300)