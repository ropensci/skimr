% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dplyr.R
\name{mutate.skim_df}
\alias{mutate.skim_df}
\title{Mutate a skim_df}
\usage{
\method{mutate}{skim_df}(.data, ...)
}
\arguments{
\item{.data}{A \code{skim_df}, which behaves like a \code{tbl.}}

\item{...}{Name-value pairs of expressions, each with length 1 or the same
length as the number of rows in the group, if using \code{\link[dplyr:group_by]{dplyr::group_by()}}, or
in the entire input (if not using groups). The name of each argument will
be the name of a new variable, and the value will be its corresponding
value. Use \code{NULL} value in \code{\link[dplyr:mutate]{dplyr::mutate()}} to drop a variable. New
variables overwrite existing variables of the same name.

The arguments in \code{...} are automatically quoted with \code{\link[rlang:defusing-advanced]{rlang::quo()}} and
evaluated with \code{\link[rlang:eval_tidy]{rlang::eval_tidy()}} in the context of the data frame. They
support unquoting \link[rlang:topic-inject]{rlang::quasiquotation} and splicing. See
\code{vignette("programming", package = "dplyr")} for an introduction to these
concepts.}
}
\value{
A \code{skim_df} object, which also inherits the class(es) of the input
data. In many ways, the object behaves like a \code{\link[tibble:tibble]{tibble::tibble()}}.
}
\description{
\code{\link[dplyr:mutate]{dplyr::mutate()}} currently drops attributes, but we need to keep them around
for other skim behaviors. Otherwise the behavior is exactly the same. For
more information, see \url{https://github.com/tidyverse/dplyr/issues/3429}.
}
\seealso{
\code{\link[dplyr:mutate]{dplyr::mutate()}} for the function's expected behavior.
}
