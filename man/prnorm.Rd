% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PRNG.R
\name{prnorm}
\alias{prnorm}
\title{Generating numbers form    Normal distribution
here we use  Box Muler transform to obtain normal random variable}
\usage{
prnorm(n)
}
\arguments{
\item{n}{number required}
}
\value{
a list of pseudo random numbers from normal distribution
}
\description{
Generating numbers form    Normal distribution
here we use  Box Muler transform to obtain normal random variable
}
\examples{
prnorm(10)
 prnorm(100)

}
