\name{voteTypeDetail}
\alias{voteTypeDetail}
\title{Return a data...}
\usage{voteTypeDetail(xml)}
\description{Return a data.frame of roll-call votes}
\details{\code{voteDetail} shows the individual votes of members from a roll-call vote}
\value{data.frame containing the vote for each member}
\keyword{data}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://developer.nytimes.com/}}
\seealso{\code{\link{getNYTCongress}}}
\arguments{\item{xml}{The xml returned from \code{getNYTCongress} with type="roll.call" (default)}}
\examples{\dontrun{
roll.call.xml <- getNYTCongress(type="party.votes", 110, "senate")
}
data(xml, package="nytR")
head(x <- voteTypeDetail(party.votes.xml))
x.summary <- do.call("rbind", x.summary <- by(x$party_votes_pct, x$party, summary))
barplot(x.summary[,"Mean"])

\dontrun{
xml <- getNYTCongress(type="missed.votes", 110, "senate")
head(voteTypeDetail(xml))

xml <- getNYTCongress(type="members", 110, "senate")
head(voteTypeDetail(xml))
}}
