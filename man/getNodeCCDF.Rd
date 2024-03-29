% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getNodeCCDF.R
\name{getNodeCCDF}
\alias{getNodeCCDF}
\title{Conditional CDf for Node}
\usage{
getNodeCCDF(varName, catName, fileName)
}
\arguments{
\item{varName}{a character, the name of study variable.}

\item{catName}{a character, categorical/conditional variable}

\item{datasources}{a list of parameters to access files sytems or databases.}
}
\description{
Computes the x value from given percentile.
}
\details{
Read each data file (node) and compute the local cumDistFunc for each data node.
}
\author{
Rui Camacho, Paula Raissa
}
