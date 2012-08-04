\name{beforeAfterMax}
\alias{beforeAfterMax}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Before, after event maximum.
}
\description{
A function to determine which observations within an event are before and after the maximum of the event.
}
\usage{
beforeAfterMax(eventCount, event_steps_till_max, event_length)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eventCount}{
A vector indicating events. The \code{eventCounts} of \code{\link{findEvents}}.
}
  \item{event_steps_till_max}{
A vector of length \code{max(eventCount)} with the number of time steps until the maximum of each event. See \code{\link{eventSummary}}.
}
  \item{event_length}{
A vector of length \code{max(eventCount)} with the length of each event. See \code{\link{eventSummary}}.
}
}
%\details{
%}
\value{
A vector indicating if samples are before or after the maximum of each event. Non event flow is represnted by 0's.
}
\author{
Jason Lessels \email{jlessels@gmail.com}
}
%\examples{
%}
