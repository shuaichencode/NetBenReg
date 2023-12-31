% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NetBenReg.R
\name{plotNetBenRegDR}
\alias{plotNetBenRegDR}
\title{Create CEAC plot based on fitted net-benefit regression with doubly robust method}
\usage{
plotNetBenRegDR(
  object,
  add = FALSE,
  xlab = "Cost-effectiveness threshold",
  ylab = "Probability Treatment 1 is cost-effective",
  pch = 20,
  cex = 1,
  lty = 1,
  lwd = 1,
  type = "o",
  ylim = NULL,
  ...
)
}
\arguments{
\item{object}{a fitted "NetBenRegDR" model object.}

\item{add}{logicial. If TRUE, will add the curve to the existing plot, instead of creating a new plot. Defaults to FALSE.}

\item{xlab}{label given to the x-axis. Defaults to "Cost-effectiveness threshold".}

\item{ylab}{label given to the y-axis. Defaults to "Probability Treatment 1 is cost-effective".}

\item{pch}{a numeric value specifying the point to label the curve. The 'points' help file contains examples of the possible marks. Defaults to 20.}

\item{cex}{a numeric value specifying the size of the marks. Defaults to 1.}

\item{lty}{a vector of integers specifying line types for each curve. Defaults to 1.}

\item{lwd}{a vector of numeric values for line widths. Defaults to 1.}

\item{type}{character indicating the type of plotting. The 'plot.default' help file contains more explanatuib. Defaults to 'o'.}

\item{...}{other arguments that will be passed forward to the underlying plot.default method.}
}
\description{
This function creates cost-effectiveness acceptability curve (CEAC) plot based on fitted net-benefit regression with doubly robust method.
}
\keyword{acceptability}
\keyword{cost-effectiveness}
\keyword{curve,}
\keyword{doubly}
\keyword{net-benefit}
\keyword{regression,}
\keyword{robust}
