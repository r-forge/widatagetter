\name{preEventSummary}
\alias{preEventSummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pre-event summary
}
\description{
This function provides a summary of data of a given period before each event.
}
\usage{
preEventSummary(eventCount, data, stepsBefore)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eventCount}{
A vector indicating events. The \code{eventCounts} of \code{\link{findEvents}}.
}
  \item{data}{
A vector of data (e.g rainfall, stream flow, temperature) of length(eventCounts)
}
  \item{stepsBefore}{
The amount of temporal steps as the begining of the window to summarise (\code{start_of_event-stepsBefore:start_of_event}).
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{minimum}{The minumum of the event}
\item{mean}{The mean of the event}
\item{median}{The median of the event}
\item{std_dev}{The standard deviation of the event}
\item{variance}{The variance of the event}
}
\author{
Jason Lessels \email{jlessels@gmail.com}
}
