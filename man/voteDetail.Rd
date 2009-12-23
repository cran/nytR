\name{voteDetail}
\alias{voteDetail}
\title{Return a data...}
\usage{voteDetail(xml)}
\description{Return a data.frame of roll-call votes}
\details{\code{voteDetail} shows the individual votes of members from a roll-call vote}
\value{data.frame containing the vote for each member}
\keyword{data}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://developer.nytimes.com/}}
\seealso{\code{\link{getNYTCongress}}}
\arguments{\item{xml}{The xml returned from \code{getNYTCongress} with type="roll.call" (default)}}
\examples{\dontrun{
roll.call.xml <- getNYTCongress(110, "senate", 2, 194)
x <- voteDetail(roll.call.xml)
head(x) # shows the id for each member and their vote
}}
