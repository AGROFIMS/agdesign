% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_fctlvl_values}
\alias{get_fctlvl_values}
\title{Get factor and levels from design interface and type of factorial}
\usage{
get_fctlvl_values(input, designVars, tf = c("yes", "no"))
}
\arguments{
\item{input}{shinyInput Variable defined in \code{server(input,output)}}

\item{designVars}{reactiveValues  Reactive expression that contains all the ID's related to add, delete and remove buttons.}

\item{tf}{character type of factorial. \code{yes} equivalent to full-factorial arragement, otherwise, \code{no} is equivalent to non-full factorial}
}
\description{
Get factor and levels from design interface and type of factorial
}
