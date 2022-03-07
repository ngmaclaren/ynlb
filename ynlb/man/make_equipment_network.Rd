\name{make_equipment_network}
\alias{make_equipment_network}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Make an equipment lending network for a location.
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make_equipment_network(loc)
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
    "Return an igraph object representing who lends farm equipment to whom. There are edges derived from *two* survey questions in this network: respondents were asked whom they leant farm equipment to *and* from whom did they borrow farm equipment. Both edges are included in these networks, and edges point towards the source of the farm equipment (i.e., towards the respondent if the response was to the question about who they leant equipment to, away from the respondent otherwise)."
    require(igraph)
    dl <- load_ynlb_data(loc)
    indiv <- dl$indiv
    el <- dl$el
    equipto <- el[el$Network == "FarmEquipTo", c("Ego", "Alter")]
    equipfrom <- el[el$Network == "FarmEquipFrom", c("Ego", "Alter")]
    nodevars <- c("IndivID", "SharingUnitID", "Age", "Gender")
    nodedata <- indiv[indiv$IndivID \%in\% unlist(rbind(equipto, 
        equipfrom)), nodevars]
    colnames(nodedata) <- c("UID", "HHID", "Age", "Gender")
    nodedata$Respondent <- ifelse(nodedata$UID \%in\% c(equipto$Ego, 
        equipfrom$Ego), TRUE, FALSE)
    colnames(equipto) <- c("Alter", "Ego")
    equipto <- equipto[, c("Ego", "Alter")]
    equip <- rbind(equipto, equipfrom)
    equip <- equip[!(equip$Ego == equip$Alter), ]
    g <- graph_from_data_frame(equip, vertices = nodedata)
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
