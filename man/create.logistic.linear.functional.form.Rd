% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SPECIFICATION_functional_forms.R
\name{create.logistic.linear.functional.form}
\alias{create.logistic.linear.functional.form}
\title{Create a Functional Form that is Linear on the Logistic Scale}
\usage{
create.logistic.linear.functional.form(
  intercept,
  slope,
  anchor.year,
  min = 0,
  max = 1,
  parameters.are.on.logit.scale = T,
  overwrite.parameters.with.alphas = F,
  error.prefix = "Cannot create logistic-linear functional.form: "
)
}
\arguments{
\item{intercept, slope}{Arrays or scalar values representing the intercept and slope on the logistic scale}

\item{anchor.year}{The year at which the functional form evaluates to the intercept}

\item{min, max}{The minimum and maximum values this functional form can create. The default (NA) sets to the min/max for the link specified (eg, 0-Inf for a log link, 0-1 for a logistic link)}

\item{parameters.are.on.logit.scale}{Logical indicating whether the intercept and slope are already on the logistic scale}

\item{overwrite.parameters.with.alphas}{Logical indicating whether, when alphas are applied, they OVERWRITE values. If F, alphas are added to the transformed value (on the transformed scale)}

\item{error.prefix}{String to prepend to any error message in case of an error}
}
\description{
Create a Functional Form that is Linear on the Logistic Scale
}
\seealso{
create.static.functional.form, create.linear.functional.form, create.log.linear.functional.form
}
