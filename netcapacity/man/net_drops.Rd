% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sldtool.R
\name{net_drops}
\alias{net_drops}
\title{Create Network graph from cables as inputs (only network cables on the graph)}
\usage{
net_drops(x, y, w, z, directed = TRUE)
}
\arguments{
\item{x}{= cables}

\item{y}{= premises}

\item{w}{= boundaries}

\item{z}{= closures}

\item{directed}{TRUE if you want a directed graph(default)}
}
\value{
graph Network as tidygraph graph of drop cables (as edges)
}
\description{
Create Network graph from cables as inputs (only network cables on the graph)
}
