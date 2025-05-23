% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uhero_colors.R
\name{uhero_pal}
\alias{uhero_pal}
\title{A palette generator for UHERO colors.}
\usage{
uhero_pal(palette = "all", discrete = TRUE, reverse = FALSE, ...)
}
\arguments{
\item{palette}{A string that defaults to "primary". This can also be set to either "secondary" or "all".}

\item{discrete}{A boolean that defaults to TRUE. Set to FALSE for a continuous scale.}

\item{reverse}{A boolean that defaults to FALSE. Set to TRUE to reverse the color scale.}

\item{...}{Additional parameters that can be passed to \code{colorRampPalette}}
}
\description{
A palette generator for UHERO colors.
}
\examples{
uhero_pal()
uhero_pal("secondary", discrete = TRUE, reverse = TRUE)
}
