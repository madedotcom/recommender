\name{userProductHitsToMatrix}
\alias{userProductHitsToMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Product recommendations for given product history
}
\description{
Converts product hits data.table to matrix.
}
\usage{
## convert user hits to matrix
userProductHitsToMatrix(product.hits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{product.hits}{
    Hits data.table with columns:
      \itemize{
        \item {
         visitor.id - unique identifier of a user.
        }
        \item {
         sku - unique identifier of a product.
        }
      }
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  Returns a sparse matrix for interactions between products within user sessions. Each cell represents a number of visitors that had interaction with products together. Column and row names of the marix contain product identifier.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  bulat.yapparov@made.com,
  lazaros.mavridis@made.com
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
%%  ~TODO~
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~recommendation }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~similarity }% __ONLY ONE__ keyword per line
