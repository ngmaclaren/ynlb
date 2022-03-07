\name{make_friendship_network}
\alias{make_friendship_network}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Make a friendship network for a location.
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make_friendship_network(loc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{loc}{
%%     ~~Describe \code{loc} here~~
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
function (loc) 
{
    "Return an igraph object representing who is friends with whom. This is an undirected network for reasons explained in Mattison et al. (2021)."
    require(igraph)
    dl <- load_ynlb_data(loc)
    indiv <- dl$indiv
    el <- dl$el
    egokey <- dl$egokey
    ties <- c("WomenHangOutAfterDinner", "MenHangOutAfterDinner")
    el <- el[el$Network \%in\% ties, ]
    egos <- c(egokey$Ego1, na.omit(egokey$Ego2))
    for (i in 1:nrow(el)) {
        q <- el[i, "Network"]
        u <- el[i, "Ego"]
        if (!(u \%in\% egokey$Ego1)) 
            next
        alter <- el[i, "Alter"]
        keyrow <- subset(egokey, Ego1 == u)
        v <- keyrow$Ego2
        stopifnot(!is.na(v) | is.na(v))
        if (is.na(v)) 
            next
        if (alter == v | alter == u) 
            next
        if (q == "WomenHangOutAfterDinner") {
            if (keyrow$Ego1M == 1) 
                el[i, "Ego"] <- v
        }
        else if (q == "MenHangOutAfterDinner") {
            if (keyrow$Ego1M == 0) 
                el[i, "Ego"] <- v
        }
        else rowvec[[2]] <- NA
    }
    friendship <- el[el$Ego \%in\% egos, c("Ego", "Alter")]
    nodevars <- c("IndivID", "SharingUnitID", "Age", "Gender")
    nodedata <- indiv[indiv$IndivID \%in\% unlist(friendship), 
        nodevars]
    colnames(nodedata) <- c("UID", "HHID", "Age", "Gender")
    nodedata$Respondent <- ifelse(nodedata$UID \%in\% egokey$Ego1, 
        TRUE, FALSE)
    g <- graph_from_data_frame(friendship, vertices = nodedata, 
        directed = FALSE)
    dropthese <- V(g)[which(is.na(V(g)$HHID) | is.na(V(g)$Age) | 
        is.na(V(g)$Gender))]
    g <- g - dropthese
    return(g)
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
