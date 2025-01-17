% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myboot2.R
\name{myboot2}
\alias{myboot2}
\title{myboot2}
\usage{
myboot2(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...)
}
\arguments{
\item{iter}{Number of iterations to be made}

\item{x}{A sample to create a bootstrap from}

\item{fun}{The function to perform on the dataframe}

\item{alpha}{The alpha variable, using to find the confidence interval}

\item{cx}{The amount by which plotting text and symbols should be scaled relative to the default}

\item{...}{}
}
\value{
A list of the confidence interval, the function, the sample, the experimental dataframe created and formatted according to the function, and the experimental dataframe created.
}
\description{
Creates a Bootstrap sample from sample x and plots its statistics
}
\examples{
myboot2(iter = 10000,x = c(0,1,2,3), fun = "mean", alpha = 0.05, cx = 1.5)
}
