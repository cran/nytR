\name{voteSummary}
\alias{voteSummary}
\title{Return a summary of a roll-call vote...}
\usage{voteSummary(xml)}
\description{Return a summary of a roll-call vote}
\details{\code{voteSummary} results a list containing a summary of a roll-call vote}
\value{list of details from the particular roll call}
\keyword{data}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://developer.nytimes.com/}}
\seealso{\code{\link{getNYTCongress}}}
\arguments{\item{xml}{The xml returned from \code{getNYTCongress} with type="roll.call" (default)}}
\examples{\dontrun{
roll.call.xml <- getNYTCongress(110, "senate", 2, 194)
x <- voteSummary(roll.call.xml)
x # view all the details
x$date # date of the vote
x$description # description of the vote question
}}
