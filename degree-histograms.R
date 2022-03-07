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

degs <- lapply(c(yn, lb), degree, mode = "in") # what happens if we only look at in-degree?
names(degs) <- paste0(c(rep("yn.", 2), rep("lb.", 2)), names(degs))

max.k <- max(unlist(degs))
ks <- 0:max.k
degmat <- matrix(nrow = length(ks), ncol = length(degs))
j <- 1
for(d in degs) {
    degmat[, j] <- sapply(ks, function(x) sum(d == x))
    j <- j + 1
}; rm(j)
colnames(degmat) <- names(degs)

filenames <- paste0("./img/", c("YN-CM", "YN-EL", "LB-CM", "LB-EL"), ".svg")
h <- 4
w <- 7
c <- "#606061"
margins <- c(1, .5, 0, 0)
for(j in 1:ncol(degmat)) {
    svg(filenames[j], width = w, height = h)
    par(mai = margins)
    barplot(degmat[, j], col = c, names.arg = ks, xlab = "In-Degree", ylab = "",
            cex.lab = 1.5, cex.axis = 1.5, cex.names = 1.5)
    dev.off()
}
