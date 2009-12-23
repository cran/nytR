\name{partyVotes}
\alias{partyVotes}
\title{Return the party totals from roll-call vote data...}
\usage{partyVotes(xml, party)}
\description{Return the party totals from roll-call vote data}
\details{\code{partyVotes} fully parses the output from roll-call vote data and returns the results for a particular party as a list}
\value{list of vote summary for supplied party}
\keyword{data}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://developer.nytimes.com/}}
\seealso{\code{\link{getNYTCongress}}}
\arguments{\item{xml}{The xml returned from \code{getNYTCongress} with type="roll.call" (default)}
\item{party}{Can by either "total", "independent", "democratic", or "republican".}}
\examples{\dontrun{
roll.call.xml <- getNYTCongress(110, "senate", 2, 194)
partyVotes(roll.call.xml, party="democratic")
}}
