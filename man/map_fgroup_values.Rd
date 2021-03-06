% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{map_fgroup_values}
\alias{map_fgroup_values}
\title{Mimic the functionality of map functions and transform factor-group inputs into data frame}
\usage{
map_fgroup_values(input, id_chr = "sel_factor_", id_rand,
  lbl = "Factor ")
}
\arguments{
\item{input}{shinyInput input values from server side}

\item{id_chr}{character character pattern id}

\item{id_rand}{character vector character random id}

\item{lbl}{characer Optional argument, in case format = "data.frame"}
}
\description{
Mimic the functionality of map functions and transform factor-group inputs into data frame
}
