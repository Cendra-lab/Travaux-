\name{numeric derivation}
\alias{numericdev}
\title{numeric derivation of functions}
\usage{
limit(f,x0, h=10e-15)
}
\description{
This function allows us to numerically derive functions in R. By having a function f, we can determine its derivative by calculating its limit when x tends to a given value for a very small step h.
}

\examples{

#calculating the derivative numerically

f <- function(x){sin(x)}
limit(f,0.2)
#[1] 0.9797718

#calculation of rounding error

f <- function(x){sin(x)}
Ear(f,0.2)
#[1] 0.0003973387

#calculating truncation error

f <- function(x){sin(x)}
Etr(f,0.2)
#[1] 1.387779e-08

