\name{parseVotes}
\alias{parseVotes}
\title{Parse the output from roll-call vote data...}
\usage{parseVotes(xml)}
\description{Parse the output from roll-call vote data}
\details{\code{parseVotes} fully parses the output from roll-call vote data and returns the results as a list}
\value{list of vote details}
\keyword{data}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://developer.nytimes.com/}}
\seealso{\code{\link{getNYTCongress}}}
\arguments{\item{xml}{The xml returned from \code{getNYTCongress} with type="roll.call" (default)}}
\examples{\dontrun{
roll.call.xml <- getNYTCongress(110, "senate", 2, 194)
votes <- parseVotes(roll.call.xml)
vote.details <- voteDetail(xml)
}}
