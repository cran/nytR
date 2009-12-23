\name{voteNomDetail}
\alias{voteNomDetail}
\title{Return a data...}
\usage{voteNomDetail(xml)}
\description{Return a data.frame of roll-call votes}
\details{\code{voteNomDetail} shows the individual votes of members from a roll-call vote}
\value{data.frame containing the vote for each member}
\keyword{data}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://developer.nytimes.com/}}
\seealso{\code{\link{getNYTCongress}}}
\arguments{\item{xml}{The xml returned from \code{getNYTCongress} with type="roll.call" (default)}}
\examples{\dontrun{
xml <- getNYTCongress(type="nomination.votes", 110, "senate")
head(x <- voteNomDetail(xml))
x.dem <- do.call("rbind",x$democratic)
x.dem <- apply(x.dem, 2, function(x) type.convert(as.character(x)))
barplot(apply(x.dem, 2, sum))
}}
