\name{make_distance_network}
\alias{make_distance_network}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Given a social network, make the corresponding distance network.
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make_distance_network(loc, g)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{loc}{
%%     ~~Describe \code{loc} here~~
}
  \item{g}{
%%     ~~Describe \code{g} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (loc, g) 
{
    "Return an igraph object representing the distances (in km) between all the houses in the survey at the given location. This function is written to provide distance between households connected by edges in another network, such as a community meals or equipment lending network."
    require(igraph)
    dl <- load_ynlb_data(loc)
    indiv <- dl$indiv
    dist <- dl$dist
    inds <- V(g)$name
    houses <- V(g)$HHID
    D <- matrix(nrow = length(inds), ncol = length(inds), dimnames = list(inds, 
        inds))
    for (i in 1:nrow(D)) {
        for (j in 1:nrow(D)) {
            if (is.na(houses[i]) | is.na(houses[j])) {
                d <- NA
            }
            else d <- dist$distance[dist$ui == houses[i] & dist$uj == 
                houses[j]]
            D[i, j] <- d
        }
    }
    d <- graph_from_adjacency_matrix(D, mode = "undirected", 
        weighted = TRUE, diag = FALSE)
    dropthese <- E(d)[which(is.na(E(d)$weight))]
    d <- d - dropthese
    UIDs <- substr(sapply(V(d)$name, function(x) indiv$IndivID[which(indiv$IndivID == 
        x)], USE.NAMES = FALSE), 3, 6)
    HHIDs <- sapply(V(d)$name, function(x) indiv$SharingUnitID[which(indiv$IndivID == 
        x)], USE.NAMES = FALSE)
    Ages <- sapply(V(d)$name, function(x) indiv$Age[which(indiv$IndivID == 
        x)], USE.NAMES = FALSE)
    Genders <- sapply(V(d)$name, function(x) indiv$Gender[which(indiv$IndivID == 
        x)], USE.NAMES = FALSE)
    vertex_attr(d) <- list(name = V(d)$name, UID = UIDs, HHID = HHIDs, 
        Age = Ages, Gender = Genders)
    return(d)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
