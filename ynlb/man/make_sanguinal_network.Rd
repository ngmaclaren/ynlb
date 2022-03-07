\name{make_sanguinal_network}
\alias{make_sanguinal_network}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Given a social network, make the corresponding sanguinal network.
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make_sanguinal_network(loc, g, cutoff = 0.125, dichotomize = FALSE)
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
  \item{dichotomize}{
%%     ~~Describe \code{dichotomize} here~~
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
function (loc, g, cutoff = 0.125, dichotomize = FALSE) 
{
    "Return an igraph object representing sanguinal relatedness between nodes in another network, `g`. The `dichotomize` option is not currently used."
    require(kinship2)
    require(igraph)
    dl <- load_ynlb_data(loc)
    indiv <- dl$indiv
    kin <- dl$kin
    P <- pedigree(id = kin$Ego, dadid = kin$Father, momid = kin$Mother, 
        sex = kin$Sex, missid = "0")
    K <- kinship(P)
    K <- K * 2
    K[which(K < cutoff)] <- 0
    inds <- V(g)$name
    S <- K[which(rownames(K) \%in\% inds), which(colnames(K) \%in\% 
        inds)]
    s <- graph_from_adjacency_matrix(S, mode = "undirected", 
        weighted = TRUE, diag = FALSE)
    missing_inds <- inds[which(!(inds \%in\% rownames(S)))]
    s <- s + missing_inds
    UIDs <- substr(sapply(V(s)$name, function(x) indiv$IndivID[which(indiv$IndivID == 
        x)], USE.NAMES = FALSE), 3, 6)
    HHIDs <- sapply(V(s)$name, function(x) indiv$SharingUnitID[which(indiv$IndivID == 
        x)], USE.NAMES = FALSE)
    Ages <- sapply(V(s)$name, function(x) indiv$Age[which(indiv$IndivID == 
        x)], USE.NAMES = FALSE)
    Genders <- sapply(V(s)$name, function(x) indiv$Gender[which(indiv$IndivID == 
        x)], USE.NAMES = FALSE)
    vertex_attr(s) <- list(name = V(s)$name, UID = UIDs, HHID = HHIDs, 
        Age = Ages, Gender = Genders)
    return(s)
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
