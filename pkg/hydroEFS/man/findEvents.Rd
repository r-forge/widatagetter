\name{findEvents}
\alias{findEvents}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to find storm events.
}
\description{
This function is designed to find storm events based on two optional conditions. The two conditions are currently based on rate of rise, and height.
}
\usage{
findEvents(streamProperty, steps, rate, height, lengthOfEvent, eventType)
}
\arguments{
  \item{streamProperty}{
A numeric vector of either stream discharge or height.
}
  \item{steps}{
The amount of steps (iterations) that the rise must occur within.
}
  \item{rate}{
The rate of which the stream must rise.
}
  \item{height}{
The height that triggers an event. In case of event types 2,5 the height that it must be above during an event. See details for more information.
}
  \item{lengthOfEvent}{
The maximum amount of steps (iterations) the event contains.
}
  \item{eventType}{
The type of event condition, wanting to be applied. See details for more information.
}
}
\details{
This function currently has 5 condition types. It has been designed to met the purpose of the simple use of an bubble meter and automatic sampler. The current event trigger conditions include;

eventType 1: Uses both rate of rise and height to trigger an event.

eventType 2: Uses both rate of rise and height to trigger an event and streamProperty must be above height during the event.

eventType 3: Only uses rate of rise to trigger an event.

eventType 4: Only uses height to trigger an event.

eventType 5: Only uses height to trigger an event and streamProperty must be above height during the event.

}
\value{
\item{eventDirection}{A numeric vector containing 5 different values. 0 indicates no event, 1 indicates rising flow during event, 2; decreasing during an event, 3; indicates level during an event, and NA values are used if the streamProperty contains an NA value.}
\item{eventCount}{A numeric vector counting the number of events in the sequence.. Values of 0 indicate no event. NA values reflect the observed NA values from the streamProperty.}

}

\author{
Jason Lessels
}
\note{
The above methods were created due to demand, more methods could be added, by request.
}
