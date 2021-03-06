% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkFuncs.R
\name{checkNumVec}
\alias{checkNumVec}
\title{Checking if all elements of a list are all numeric vectors}
\usage{
checkNumVec(listNum, namesListElements = NULL)
}
\arguments{
\item{listNum}{A list of the vectors of which one wishes to check if their data type is numeric}

\item{namesListElements}{Character vector containing the names of the variables of which the data type is checked. Optional parameter, with as default value NULL. This argument should be used when the variable of which the data type is checked is not an object that was provided as an argument to the function, or when the list elements of the first argument do not have a name attached to it.}
}
\value{
No value is returned if all vectors have the numeric data type. If not, an error message is thrown for each element of the list that does not pertain to the numeric data type.
}
\description{
Checking if all elements of a list are all numeric vectors
}
\examples{
arg1 <- 2
checkNumVec(list(arg1))

\donttest{checkNumVec(list(TRUE, T, 2))
checkNumVec(list(TRUE, T, 2), c('Var1', 'Var2', 'Var3'))

arg2 <- 0.8
checkNumVec(list(arg2))
checkNumVec(list(arg2, 'T', 2))}
}
