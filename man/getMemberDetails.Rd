\name{getMemberDetails}
\alias{getMemberDetails}
\title{Pull congressional membership data from NY Times Congress API.}
\usage{getMemberDetails(id,
    api.key=as.character(Sys.getenv("NYTIMES_CONGRESS")))}
\description{Pull congressional membership data from NY Times Congress API.}
\details{\code{getMemberDetails} congressional membership data from NY Times Congress API.}
\value{XML results from NY Times API}
\keyword{data}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://developer.nytimes.com/}}
\seealso{\code{\link{getNYTCongress}}}
\arguments{\item{id}{The member id returned from calls to \code{getNYTCongress}}
\item{api.key}{The Times API requires an API}}
\examples{\dontrun{
members.xml <- getMemberDetails("W000779")
}}
