library(igraph)
library(ynlb)
library(ergm)
library(sna)
skewness <- moments::skewness

set.seed(1234)
save_plots <- FALSE

yn <- list(CM = make_meals_network("yn"))
yn$CMs <- make_sanguinal_network("yn", yn$CM)
yn$CMa <- make_ep_affinal_network("yn", yn$CM)
yn$CMd <- make_distance_network("yn", yn$CM)
yn$EL <- make_equipment_network("yn")
yn$ELs <- make_sanguinal_network("yn", yn$EL)
yn$ELa <- make_ep_affinal_network("yn", yn$EL)
yn$ELd <- make_distance_network("yn", yn$EL)
yn <- lapply(yn, intergraph::asNetwork)
lb <- list(CM = make_meals_network("lb"))
lb$CMs <- make_sanguinal_network("lb", lb$CM)
lb$CMa <- make_ep_affinal_network("lb", lb$CM)
lb$CMd <- make_distance_network("lb", lb$CM)
lb$EL <- make_equipment_network("lb")
lb$ELs <- make_sanguinal_network("lb", lb$EL)
lb$ELa <- make_ep_affinal_network("lb", lb$EL)
lb$ELd <- make_distance_network("lb", lb$EL)
lb <- lapply(lb, intergraph::asNetwork)

for(i in c(4, 8)) {
    yn[[i]] %e% "weight" <- yn[[i]] %e% "weight"/1000
    lb[[i]] %e% "weight" <- lb[[i]] %e% "weight"/100
}

## "Fundamental" Models
models <- list(
    ynCM = ergm(yn$CM ~ edges + edgecov(yn$CMs, "weight") + edgecov(yn$CMa, "weight") +
                    edgecov(yn$CMd, "weight")),
    lbCM = ergm(lb$CM ~ edges + edgecov(lb$CMs, "weight") + edgecov(lb$CMa, "weight") +
                    edgecov(lb$CMd, "weight")),
    ynEL = ergm(yn$EL ~ edges + edgecov(yn$ELs, "weight") + edgecov(yn$ELa, "weight") +
                    edgecov(yn$ELd, "weight")),
    lbEL = ergm(lb$EL ~ edges + edgecov(lb$ELs, "weight") + edgecov(lb$ELa, "weight") +
                    edgecov(lb$ELd, "weight"))
)

lapply(models, summary)

### Simulations ###
ttriads <- c("030T", "120U", "120D", "300")

## Empirical results
graphlist <- list(MCM = yn$CM, PCM = lb$CM, MEL = yn$EL, PEL = lb$EL)
emp_deg <-degree(graphlist, g = 1:4, cmode = "indegree")

emp <- list(
    ## skew in the degree distribution
    skew = sapply(emp_deg, skewness),
    ## mutuality
    mutual = mutuality(graphlist),
    ## transitivity # sum the transitive triads
    trans = rowSums(triad.census(graphlist, mode = "digraph")[, ttriads]),
    ## gender homophily
    ## "When diff=FALSE, this term adds one network statistic to the model, which counts the number of edges (i, j) for which attr(i)==attr(j)."
    homoph_gender = sapply(graphlist, function(g) summary(g ~ nodematch("Gender"))),
    ## age homophily
    ## This term adds one network statistic to the model equaling the sum of abs(attr[i]-attr[j])^pow for all edges (i,j) in the network."
    homoph_age = sapply(graphlist, function(g) summary(g ~ absdiff("Age")))
)

## Simulation results

## default values of burnin and interval are acceptable (10,000 and 1000, respectively)
## produces a list of four, each element of which is a list of 2500 networks
simulations <- lapply(models, simulate, nsim = 2500, output = "network")

sim_deg <- lapply(simulations, function(x) lapply(x, degree, g = 1:length(x), cmode = "indegree"))

sim <- list(
    skew = lapply(sim_deg, function(x) sapply(x, skewness)),
    mutual = lapply(simulations, function(x) mutuality(x)),
    trans = lapply(simulations, function(x) rowSums(triad.census(x, mode = "digraph")[, ttriads])),
    homoph_gender = lapply(simulations, function(x) {
        sapply(x, function(g) summary(g ~ nodematch("Gender")))}),
    homoph_age = lapply(simulations, function(x) {
        sapply(x, function(g) summary(g ~ absdiff("Age")))})
)

## Print summary statistics
emp

lapply(sim, function(x) sapply(x, mean))
ci <- lapply(sim, function(x) lapply(x, quantile, probs = c(0.025, 0.975)))

test95 <- function(x, y) {
    if(y <= x[1]) {
        return("Less")
    } else if(y >= x[2]) {
        return("Greater")
    } else return("N.S.")
}
for(i in 1:length(emp)) {
    print(names(emp)[i])
    print("One sided p, greater than")
    print(mapply(function(x, y) sum(x >= y)/length(x), sim[[i]], emp[[i]]))
    print("Two sided p")
    print(mapply(test95, ci[[i]], emp[[i]])) # function(x, y) y <= x[1] | y >= x[2]
}

## Make a figure
## rows are variables, columns are networks
nrows <- length(emp) - 2 # to remove the homophily rows from the plot
ncols <- length(graphlist)
subplotscale <- 4
ht <- nrows*subplotscale - 5
wd <- ncols*subplotscale 
rownomen <- c("Degree Skewness", "# Reciprocal Dyads", "# Transitive Triads",
              "Homophily (Gender)", "Homophily (Age)") # "Avg. Deg.", 
if(save_plots) {
    ##pdf("./img/simresults.pdf", height = ht, width = wd)
    png("./img/simresults.png", height = ht, width = wd, units = "in", res = 90, type = "cairo-png")
} else dev.new(height = ht, width = wd)
par(mfrow = c(nrows, ncols))
for(i in 1:nrows) {# rows
    for(j in 1:ncols) {# columns

        bottompad <- 2
        if(j == 1) leftpad <- 6 else leftpad <- 4
        if(i == 1) toppad <- 4 else toppad <- 2
        rightpad <- 2
        par(mar = c(bottompad, leftpad, toppad, rightpad) + 0.1)

        if(j == 1) ylab <- "Frequency" else ylab <- ""

        nunique <- length(unique(sim[[i]][[j]]))
        if(nunique <= 10) breaks <- nunique else breaks <- "Sturges"

        h <- hist(sim[[i]][[j]], breaks = breaks, plot = FALSE)

        if(emp[[i]][[j]] > max(sim[[i]][[j]])) {
            xlim <- c(min(sim[[i]][[j]]), emp[[i]][[j]])
        } else xlim <- range(sim[[i]][[j]])
        ylim <- range(h$counts)
        
        plot(h, main = "", xlab = "", ylab = ylab, col = "snow2", ylim = ylim, xlim = xlim,
             cex.axis = 1.5, cex.lab = 1.5)
        abline(v = emp[[i]][j], col = "darkmagenta", lwd = 4, lty = 1)
        abline(v = ci[[i]][[j]], col = "dodgerblue", lwd = 4, lty = 2)
        
        ##if(i == 1) main <- names(graphlist)[j] else main <- ""
        if(i == 1) mtext(names(graphlist)[j], line = 2, font = 2)
        if(j == 1) mtext(rownomen[i], side = 2, line = 4.5, font = 2)
    }
}
if(save_plots) dev.off()
