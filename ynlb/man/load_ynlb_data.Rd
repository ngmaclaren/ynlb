\name{load_ynlb_data}
\alias{load_ynlb_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Needs to be altered now that the .rda are available
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
load_ynlb_data(loc)
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
    "Primarily meant to be used within the make_*_network functions. Reads the CSV files that hold the project information, does some cleaning, and returns a list of data frames for further use."
    if (loc == "yn") {
        indiv <- read.csv("./data/YN_Indiv_ngm.csv")
        el <- read.csv("./data/YN_Edgelist_ngm.csv")
        el <- subset(el, (Alter \%in\% indiv$IndivID) & (Ego \%in\% 
            indiv$IndivID))
        el$Alter[which(el$Alter == "YN9076")] <- "YN2000"
        el$Ego[which(el$Ego == "YN2016")] <- "YN2017"
        kin <- read.csv("./data/YN_Kinship_ngm.csv")
        ego2 <- read.csv("./data/YN_Ego2.csv")
        distance <- read.csv("./data/YN_Distance.csv")
    }
    else if (loc == "lb") {
        indiv <- read.csv("./data/LB_Indiv.csv")
        el <- read.csv("./data/LB_Edgelist.csv")
        el <- subset(el, (Alter \%in\% indiv$IndivID) & (Ego \%in\% 
            indiv$IndivID))
        kin <- read.csv("./data/LB_Kinship.csv")
        kin[kin$Ego == "LB2812", "Sex"] <- "M"
        kin[kin$Ego == "LB8061", "Sex"] <- "F"
        ego2 <- read.csv("./data/LB_Ego2.csv")
        distance <- read.csv("./data/LB_Distance.csv")
    }
    return(list(indiv = indiv, el = el, kin = kin, dist = distance, 
        egokey = ego2))
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
