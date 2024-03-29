% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roc.R
\name{roc_cut}
\alias{roc_cut}
\title{Optimal cut for ROC curve}
\usage{
roc_cut(pred, actual, plot = T)
}
\arguments{
\item{pred}{A vector of prediction probabilities.}

\item{actual}{A vector of outcome classes corresponding to \code{pred}.}

\item{plot}{Logical indicating whether to plot a ROC curve together with distance from the optimal corner.}
}
\description{
Calculates the "optimal" cutoff for a classifier from a ROC curve. This is the cutoff that minimises the
distance from the point (FPR = 0, TPR = 1). Also returns sensitivity and specitivity for this cutoff, the AUC
of the ROC curve, and optionally plots the curve together with the distance measure.
}
\details{
Adapted from \url{https://www.r-bloggers.com/a-small-introduction-to-the-rocr-package/}.
}
