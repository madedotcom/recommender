\name{getSimilarProducts}
\alias{getSimilarProducts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Product recommendations for given product history
}
\description{
From the similarity matrix recommendations are extracted for the given product history.
}
\usage{
## get similar products via a given similarity matrix
getSimilarProducts(sim.matrix, skus, values, exclude.same, groups)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sim.matrix}{
    Similarity matrix calculatd from of product hits
  }
   \item{skus}{
    List of skus from product history of a given user.
  }
   \item{values}{
    Number of products to be included in the result.
  }
   \item{exclude.same}{
    If set to TRUE values from the skus vector will be excluded from the result.
  }
  \item{groups}{
    Named vector of groups that if provided will force result to contain a single product per group.
    Names represent product identifier that is used in column and row names of the sim.matrix.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Vector of similar products.
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
sim.matrix <- c(1.00, 0.52, 0.41, 0.52,
                     0.60, 0.60, 0.23, 0.26,
                     0.40, 0.30, 0.83, 0.26,
                     0.62, 0.80, 0.25, 0.16 )

sim.matrix <- matrix(sim.matrix, nrow = 4, ncol = 4)
colnames(sim.matrix) <- rownames(sim.matrix) <- c("a", "b", "c", "d")
viewed.skus <- c("a", "b")
sku.groups <- c("a" = "p1", "b" = "p2", "c" = "p3", "d" = "p1")
res <- getSimilarProducts(sim.matrix , viewed.skus, 2, exclude.same = FALSE, sku.groups)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~recommendation }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~similarity }% __ONLY ONE__ keyword per line
