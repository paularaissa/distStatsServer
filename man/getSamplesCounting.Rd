% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getSamplesCounting.R
\name{getSamplesCounting}
\alias{getSamplesCounting}
\title{Samples Counting}
\usage{
getSamplesCounting(x = NULL, formula = NULL, xREF = NULL,
  cVar = NULL, relation = NULL, threshold = NULL, relation2 = NULL,
  threshold2 = NULL)
}
\arguments{
\item{xREF}{a numeric value, given reference do calculate the sample size.}

\item{varName}{a study variable, it must be a numeric vector.}
}
\value{
The numeric value corresponding to a percentile.
}
\description{
Computes the x value corresponding to a given percentile.
}
\details{
Returns the x value corresponding to a given percentile \link{0..1}
}
\section{Dependencies}{

\code{\link{getVarByName}}
}

\author{
Rui Camacho, Paula Raissa
}
