% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_seroincidence_module.R
\name{estimate_seroincidence_server}
\alias{estimate_seroincidence_server}
\title{Server Logic for Seroincidence Estimation}
\usage{
estimate_seroincidence_server(
  id,
  pop_data,
  curve_data,
  noise_data,
  imported_data
)
}
\arguments{
\item{id}{A string to identify a namespace}

\item{pop_data}{Reactive expression for population data}

\item{curve_data}{Reactive expression for curve data}

\item{noise_data}{Reactive expression for noise data}

\item{imported_data}{data returned by import_data_module}
}
\description{
Server Logic for Seroincidence Estimation
}
