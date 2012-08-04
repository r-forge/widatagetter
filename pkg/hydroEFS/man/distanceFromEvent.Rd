\name{distanceFromEvent}
\alias{distanceFromEvent}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Distance from event
}
\description{
A function which retrieves the distance of each step from the previous event.
}
\usage{
distanceFromEvent(eventCount)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eventCount}{
A vector indicating events. The \code{eventCounts} of \code{\link{findEvents}}.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A vector indicating the steps since the start of the previous event.
}
\author{
Jason Lessels \email{jlessels@gmail.com}
}
%\examples{
%}