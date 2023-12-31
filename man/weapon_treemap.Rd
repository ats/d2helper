% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{weapon_treemap}
\alias{weapon_treemap}
\title{Build treemap of weapon usage}
\usage{
weapon_treemap(
  data = NULL,
  type = NULL,
  tier_weapon = NULL,
  mode = NULL,
  name_contains = NULL,
  limit = 200,
  title = NULL,
  subtitle = NULL
)
}
\arguments{
\item{data}{data set to use for plot}

\item{type}{weapon type to filter}

\item{tier_weapon}{weapon tier to filter (e.g. "legendary")}

\item{mode}{game mode to filter for}

\item{name_contains}{string to filter activity names}

\item{limit}{maximum number of weapons to include in plot}

\item{title}{plot title}

\item{subtitle}{plot subtitle}
}
\description{
Build treemap of weapon usage
}
