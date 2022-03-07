\name{make_ep_affinal_network}
\alias{make_ep_affinal_network}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Given a social network, make the corresponding affinal network.
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make_ep_affinal_network(loc, g, cutoff = 0.125)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{loc}{
%%     ~~Describe \code{loc} here~~
}
  \item{g}{
%%     ~~Describe \code{g} here~~
}
  \item{cutoff}{
%%     ~~Describe \code{cutoff} here~~
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
function (loc, g, cutoff = 0.125) 
{
    "Return an igraph object representing affinal relatedness between nodes in another network, `g`. The `dichotomize` option is not currently used."
    require(kinship2)
    require(igraph)
    dl <- load_ynlb_data(loc)
    indiv <- dl$indiv
    kin <- dl$kin
    IND <- read.csv("./data/Ind.csv", na.strings = c("NA", "", 
        "NULL", "null", ".", ".a", ".b", ".c"))
    P <- pedigree(id = kin$Ego, dadid = kin$Father, momid = kin$Mother, 
        sex = kin$Sex, missid = "0")
    K <- kinship(P)
    K <- K * 2
    indiv$UID <- as.integer(substr(indiv$IndivID, 3, 6))
    indiv$Spouse <- sapply(indiv$UID, function(x) {
        if (x \%in\% IND$UID) {
            IND$PartnerUID[which(IND$UID == x)]
        }
        else NA
    })
    inds <- V(g)$name
    M <- K
    M[which(M != 0.5)] <- 0
    M[which(M == 0.5)] <- 1
    for (i in 1:nrow(indiv)) {
        if (is.na(indiv$Spouse[i])) {
            next
        }
        else {
            spouse <- paste0(toupper(loc), indiv$Spouse[i])
            if (!(spouse \%in\% colnames(M))) 
                next
            M[indiv$IndivID[i], spouse] <- 1
        }
    }
    m <- graph_from_adjacency_matrix(M, mode = "undirected")
    md <- distances(m)
    md <- 2^-md
    A <- md - K
    A <- A[which(rownames(M) \%in\% inds), which(colnames(M) \%in\% 
        inds)]
    A[which(A < cutoff)] <- 0
    a <- graph_from_adjacency_matrix(A, mode = "undirected", 
        weighted = TRUE, diag = FALSE)
    dropthese <- E(a)[which(is.na(E(a)$weight))]
    a <- a - dropthese
    UIDs <- substr(sapply(V(a)$name, function(x) indiv$IndivID[which(indiv$IndivID == 
        x)], USE.NAMES = FALSE), 3, 6)
    HHIDs <- sapply(V(a)$name, function(x) indiv$SharingUnitID[which(indiv$IndivID == 
        x)], USE.NAMES = FALSE)
    Ages <- sapply(V(a)$name, function(x) indiv$Age[which(indiv$IndivID == 
        x)], USE.NAMES = FALSE)
    Genders <- sapply(V(a)$name, function(x) indiv$Gender[which(indiv$IndivID == 
        x)], USE.NAMES = FALSE)
    vertex_attr(a) <- list(name = V(a)$name, UID = UIDs, HHID = HHIDs, 
        Age = Ages, Gender = Genders)
    return(a)
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
