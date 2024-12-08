% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_disp.R
\name{calc_idf}
\alias{calc_idf}
\title{Calculate Inverse Document Frequency}
\usage{
calc_idf(tdm)
}
\arguments{
\item{tdm}{A term-document matrix}
}
\value{
A numeric vector of inverse document frequencies for each term
}
\description{
Computes the inverse document frequency (IDF) for each term in a term-document matrix.
IDF is calculated as log(N/df) where N is the total number of documents and df is
the document frequency of the term.
}
\keyword{internal}
