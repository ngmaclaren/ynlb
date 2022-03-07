library(igraph)
library(ynlb)

yn <- list(
    CM = make_meals_network("yn"),
    EL = make_equipment_network("yn")
)

lb <- list(
    CM = make_meals_network("lb"),
    EL = make_equipment_network("lb")
)

degs <- lapply(c(yn, lb), degree, mode = "in")
names(degs) <- paste0(c(rep("yn.", 2), rep("lb.", 2)), names(degs))

newcols <- c("degree", "network")
degsdfs <- list()
for(j in 1:length(degs)) {
    deg <- degs[[j]]
    net <- names(degs)[j]
    df <- data.frame(deg, net)
    colnames(df) <- newcols
    degsdfs[[j]] <- df
}
degs <- do.call(rbind, degsdfs)

kruskal.test(degree ~ network, data = degs)
pairwise.wilcox.test(degs$degree, degs$network, "hochberg")
