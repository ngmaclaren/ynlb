\name{affinal_rules}
\alias{affinal_rules}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Not currently used.
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
affinal_rules(x, y, indiv, K)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{indiv}{
%%     ~~Describe \code{indiv} here~~
}
  \item{K}{
%%     ~~Describe \code{K} here~~
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
function (x, y, indiv, K) 
{
    "Given an individual, `x`, and another individual, `y`, implement the rules for affinal relatedness (i.e., relatedness by marriage rather than by blood) found in Power & Ready (2019). Returns a relatedness value for the affinal relationship between `x` and `y` that is a value between 0 and 1 or NA.\n\nThis function is meant to be called only inside of `make_affinal_network()`: if called outside of that environment, must have indiv, K, and loc in the namespace. Further, `indiv` should have individual IDs for `x` and `y` ('IndivID') as well as IDs for the spouses/partners of `x` and `y` ('Spouse'). `K` is a kinship matrix (see the 'kinship2' package), and `loc` is either 'yn' or 'lb'.\n\nThe rules from Power & Ready (2019) are:\n    = 1 for current spouses\n    = 0.5 for spouse's parents/siblings; own siblings' spouses\n    = 0.25 for spouses’ grandparents, aunts and uncles, nieces and nephews, half siblings; spouses of aunts/uncles, nieces/nephews, half-siblings, and grandchildren\n    = 0.125 for Spouses’ great-grandparents, great-aunts/uncles, great-nieces/nephews, and cousins; spouses of great-aunt/uncles, great-nieces/nephews, cousins, and great-grandchildren"
    prefix <- substr(x, 1, 2)
    if (x == y) 
        return(0)
    if (x \%in\% indiv$IndivID) {
        xspouse <- indiv$Spouse[which(indiv$IndivID == x)]
    }
    else return(NA)
    if (y \%in\% indiv$IndivID) {
        yspouse <- indiv$Spouse[which(indiv$IndivID == y)]
    }
    else return(NA)
    a <- rep(NA, 2)
    if (is.na(xspouse)) {
        a[1] <- NA
    }
    else {
        yUID <- as.integer(substr(y, 3, 6))
        if (xspouse == yUID) {
            a[1] <- 1
        }
        else {
            xspouse_loc <- paste0(prefix, xspouse)
            if (xspouse_loc \%in\% rownames(K) & y \%in\% colnames(K)) 
                a[1] <- K[xspouse_loc, y]
            else NA
        }
    }
    if (is.na(yspouse)) {
        a[2] <- NA
    }
    else {
        yspouse_loc <- paste0(prefix, yspouse)
        if (x \%in\% rownames(K) & yspouse_loc \%in\% colnames(K)) 
            a[2] <- K[x, yspouse_loc]
        else NA
    }
    a <- if (sum(is.na(a)) == 2) 
        NA
    else max(a, na.rm = TRUE)
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
