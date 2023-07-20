nodes <- read.csv(
    "~/mnt/hpf/Users/pvelayudhan/port/cpu_nodes.csv",
    header = FALSE
)

colnames(nodes) <- "num_nodes"

nodes |>
    dplyr::group_by("num_nodes") |>
    dplyr::summarize(n = dplyr::n())

count(nodes)

table(nodes)

library(plyr)

nnode_sum <- count(nodes, "num_nodes")

library(ggplot2)

nnode_sum$num_nodes <- as.numeric(nnode_sum$num_nodes)

ggplot(nnode_sum, aes(x = num_nodes, y = freq)) +
    geom_point(
        size = 4
    ) +
    xlim(1, 80) +
    ylim(0, 35) +
    theme_bw()

hist(nodes)
