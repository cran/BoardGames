% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xy2index.R
\name{xy2index}
\alias{xy2index}
\title{Converts a set of x,y coordinates into a matrix index.}
\usage{
xy2index(data, x, y)
}
\arguments{
\item{data}{Matrix or data frame.}

\item{x}{x-coordinate}

\item{y}{y-coordinate}
}
\description{
This function converts a set of unit x,y coordinates into a matrix index.
}
\examples{
M = matrix(1:20,4,5)
xy2index(data=M, x=3, y=2)
}
\keyword{convert}
\keyword{coordinate}
\keyword{element}
\keyword{index}
\keyword{matrix}

