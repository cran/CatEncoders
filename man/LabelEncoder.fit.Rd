% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basic.R
\name{LabelEncoder.fit}
\alias{LabelEncoder.fit}
\title{LabelEncoder.fit
fits a LabelEncoder object}
\usage{
LabelEncoder.fit(y)
}
\arguments{
\item{y}{A vector of characters, factors, or numerics, which can include NA as well}
}
\value{
Returns an object of S4 class LabelEncoder.
}
\description{
LabelEncoder.fit
fits a LabelEncoder object
}
\examples{
# factor y
y <- factor(c('a','d','e',NA),exclude=NULL)
lenc <- LabelEncoder.fit(y)
# new values are transformed to NA
z <- transform(lenc,factor(c('d','d',NA,'f')))
print(z)

# character y
y <- c('a','d','e',NA)
lenc <- LabelEncoder.fit(y)
# new values are transformed to NA
z <- transform(lenc,c('d','d',NA,'f'))
print(z)

# numeric y
set.seed(123)
y <- sample(c(1:10,NA),5)
lenc <- LabelEncoder.fit(y)
# new values are transformed to NA
z <-transform(lenc,sample(c(1:10,NA),5))
print(z)
}

