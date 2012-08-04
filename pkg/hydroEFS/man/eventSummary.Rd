\name{eventSummary}
\alias{eventSummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
event Summary
}
\description{
A function to summarise data during events.
}
\usage{
eventSummary(eventCount, data)
}
\arguments{
  \item{eventCount}{
A vector indicating events. The \code{eventCounts} of \code{\link{findEvents}}.
}
  \item{data}{
A vector of data (e.g rainfall, stream flow, temperature) of length(eventCounts)
}
}
\details{
This function provides a summary of a vector during events.
}
\value{
\item{minimum}{The minumum of the event}
\item{mean}{The mean of the event}
\item{median}{The median of the event}
\item{std_dev}{The standard deviation of the event}
\item{variance}{The variance of the event}
\item{length}{The length of the event}
\item{steps_till_max}{The steps from the start of the event until the maxmimum value of the event}
}
\author{
Jason Lessels \email{jlessels@gmail.com}
}
