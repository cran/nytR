#
# nytR: An R package to pull data from the NY Times API
# author: Shane Conway <shane.conway@gmail.com>
#
# This is released under a GPL license.
#
# Any reproduction of this code must include attribution to the NY Times.
#
###############################################################################

#' Pull vote data from NY Times Congress API.
#'
#' \code{getNYTCongress} pulls vote data from the NY Times Congress API.
#'
#' @param congress.number 
#' @param chamber 
#' @param session.number 
#' @param roll.call.number 
#' @param type Specifies the type of data to retrieve 
#' @param api.key The Times API requires an API 
#' @return XML results from NY Times API
#' @keywords data
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://developer.nytimes.com/}
#' @seealso \code{\link{parseVotes}} which fully parse the output from this function
#' @examples
#' \dontrun{
#' roll.call.xml <- getNYTCongress(110, "senate", 2, 194)
#' }
#' data(xml, package="nytR")
#' votes <- parseVotes(roll.call.xml)
#' vote.details <- voteDetail(roll.call.xml)
getNYTCongress <- function(congress.number, chamber, session.number, roll.call.number, type="roll.call", api.key=as.character(Sys.getenv("NYTIMES_CONGRESS"))) {
	if(api.key == "") stop("Pulling data from the NY Times requires an API key, which can easily be created here: http://developer.nytimes.com/.  See the configuration details in vignette('nytR').")
	if(type=="roll.call") {
		url.string = paste("http://api.nytimes.com/svc/politics/v2/us/legislative/congress/", congress.number, "/", chamber, "/sessions/", session.number, "/votes/", roll.call.number, ".xml?api-key=", api.key, sep="")
	} else if(type=="missed.votes") {
		url.string = paste("http://api.nytimes.com/svc/politics/v2/us/legislative/congress/", congress.number, "/", chamber, "/missed_votes.xml?api-key=", api.key, sep="")
	} else if(type=="party.votes") {
		url.string = paste("http://api.nytimes.com/svc/politics/v2/us/legislative/congress/", congress.number, "/", chamber, "/party_votes.xml?api-key=", api.key, sep="")
	} else if(type=="nomination.votes") {
		url.string = paste("http://api.nytimes.com/svc/politics/v2/us/legislative/congress/", congress.number, "/nominations.xml?api-key=", api.key, sep="")
	} else if(type=="members") {
		url.string = paste("http://api.nytimes.com/svc/politics/v2/us/legislative/congress/", congress.number, "/", chamber, "/members.xml?api-key=", api.key, sep="")
	} else {
		stop(paste("unknown type: ", type))
	}
	return(tryCatch(xmlRoot(xmlTreeParse(url.string)), error = function(e) { print(paste("failed to contact URL:", url.string)); stop(e) }))
}

#' Pull congressional membership data from NY Times Congress API.
#'
#' \code{getMemberDetails} congressional membership data from NY Times Congress API.
#'
#' @param id The member id returned from calls to \code{getNYTCongress}
#' @param api.key The Times API requires an API 
#' @return XML results from NY Times API
#' @keywords data
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://developer.nytimes.com/}
#' @seealso \code{\link{getNYTCongress}} 
#' @examples
#' \dontrun{
#' members.xml <- getMemberDetails("W000779")
#' }
getMemberDetails <- function(id, api.key=as.character(Sys.getenv("NYTIMES_CONGRESS"))) {
	if(api.key == "") stop("Pulling data from the NY Times requires an API key, which can easily be created here: http://developer.nytimes.com/.  See the configuration details in vignette('nytR').")	
	url.string = paste("http://api.nytimes.com/svc/politics/v2/us/legislative/congress/members/",id,".xml?api-key=", api.key, sep="")
	return(tryCatch(xmlRoot(xmlTreeParse(url.string)), error = function(e) { print(paste("failed to contact URL:", url.string)); stop(e) }))
}

#' Parse the output from roll-call vote data 
#'
#' \code{parseVotes} fully parses the output from roll-call vote data and returns the results as a list
#'
#' @param xml The xml returned from \code{getNYTCongress} with type="roll.call" (default)
#' @return list of vote details
#' @keywords data
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://developer.nytimes.com/}
#' @seealso \code{\link{getNYTCongress}} 
#' @examples
#' \dontrun{
#' roll.call.xml <- getNYTCongress(110, "senate", 2, 194)
#' votes <- parseVotes(roll.call.xml)
#' vote.details <- voteDetail(xml)
#' }
parseVotes <- function(xml) {
	x <- list(
		vote.summary=voteSummary(xml),
		total.votes=partyVotes(xml, party="total"),
		democratic.votes=partyVotes(xml, party="democratic"),
		republican.votes=partyVotes(xml, party="republican"),
		independent.votes=partyVotes(xml, party="independent"),
		votes=voteDetail(xml)
		)
	return(x)
}

#' Return the party totals from roll-call vote data 
#'
#' \code{partyVotes} fully parses the output from roll-call vote data and returns the results for a particular party as a list
#'
#' @param xml The xml returned from \code{getNYTCongress} with type="roll.call" (default)
#' @param party Can by either "total", "independent", "democratic", or "republican".
#' @return list of vote summary for supplied party
#' @keywords data
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://developer.nytimes.com/}
#' @seealso \code{\link{getNYTCongress}} 
#' @examples
#' \dontrun{
#' roll.call.xml <- getNYTCongress(110, "senate", 2, 194)
#' partyVotes(roll.call.xml, party="democratic")
#' }
partyVotes <- function(xml, party) {
	parties <- c("total","independent","democratic","republican")
	if(!(party %in% parties)) {
		stop(paste("Invalid party option", party, "; should be: ", paste(parties, collapse=", ")))
	}
	return(xmlToList(getNodeSet(xml,paste("//", party, sep=""))[[1]]))
}

#' Return a summary of a roll-call vote  
#'
#' \code{voteSummary} results a list containing a summary of a roll-call vote 
#'
#' @param xml The xml returned from \code{getNYTCongress} with type="roll.call" (default)
#' @return list of details from the particular roll call
#' @keywords data
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://developer.nytimes.com/}
#' @seealso \code{\link{getNYTCongress}} 
#' @examples
#' \dontrun{
#' roll.call.xml <- getNYTCongress(110, "senate", 2, 194)
#' x <- voteSummary(roll.call.xml)
#' x # view all the details
#' x$date # date of the vote
#' x$description # description of the vote question
#' }
voteSummary <- function(xml) {
	x <- list(congress=as.numeric(xmlValue(getNodeSet(xml,"//congress")[[1]])),
			session=as.numeric(xmlValue(getNodeSet(xml,"//session")[[1]])),
			chamber=as.character(xmlValue(getNodeSet(xml,"//chamber")[[1]])),
			roll_call=as.numeric(xmlValue(getNodeSet(xml,"//roll_call")[[1]])),
			bill_number=as.character(xmlValue(getNodeSet(xml,"//bill_number")[[1]])),
			question=as.character(xmlValue(getNodeSet(xml,"//question")[[1]])),
			description=as.character(xmlValue(getNodeSet(xml,"//description")[[1]])),
			vote_type=as.character(xmlValue(getNodeSet(xml,"//vote_type")[[1]])),
			date=as.POSIXlt(xmlValue(getNodeSet(xml,"//date")[[1]])),
			time=as.character(xmlValue(getNodeSet(xml,"//time")[[1]])),
			result=as.character(xmlValue(getNodeSet(xml,"//result")[[1]])),
			total=as.numeric(xmlValue(getNodeSet(xml,"//total")[[1]]))
	)
	return(x)
}

#' Return a data.frame of roll-call votes  
#'
#' \code{voteDetail} shows the individual votes of members from a roll-call vote 
#'
#' @param xml The xml returned from \code{getNYTCongress} with type="roll.call" (default)
#' @return data.frame containing the vote for each member
#' @keywords data
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://developer.nytimes.com/}
#' @seealso \code{\link{getNYTCongress}} 
#' @examples
#' \dontrun{
#' roll.call.xml <- getNYTCongress(110, "senate", 2, 194)
#' x <- voteDetail(roll.call.xml)
#' head(x) # shows the id for each member and their vote
#' }
voteDetail <- function(xml) {
	xmlnames <- names.XMLNode(getNodeSet(xml,"//position")[[1]])
	x <- getNodeSet(xml,"//positions")[[1]]
	x <- do.call("rbind", lapply(xmlToList(x), function(x) as.data.frame(x)))
	colnames(x) <- xmlnames
	return(x)
}

#' Return a data.frame of roll-call votes  
#'
#' \code{voteDetail} shows the individual votes of members from a roll-call vote 
#'
#' @param xml The xml returned from \code{getNYTCongress} with type="roll.call" (default)
#' @return data.frame containing the vote for each member
#' @keywords data
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://developer.nytimes.com/}
#' @seealso \code{\link{getNYTCongress}} 
#' @examples
#' \dontrun{
#' roll.call.xml <- getNYTCongress(type="party.votes", 110, "senate")
#' }
#' data(xml, package="nytR")
#' head(x <- voteTypeDetail(party.votes.xml))
#' x.summary <- do.call("rbind", x.summary <- by(x$party_votes_pct, x$party, summary))
#' barplot(x.summary[,"Mean"])
#' 
#' \dontrun{
#' xml <- getNYTCongress(type="missed.votes", 110, "senate")
#' head(voteTypeDetail(xml))
#' 
#' xml <- getNYTCongress(type="members", 110, "senate")
#' head(voteTypeDetail(xml))
#' }
voteTypeDetail <- function(xml) {
	xmlnames <- names.XMLNode(getNodeSet(xml,"//member")[[1]])
	x <- getNodeSet(xml, "//members")[[1]]
	x <- ldply(xmlToList(x),function(x) { x <- data.frame(rbind(t(x))); colnames(x)=1:length(x); return(x)})[,-1]
	colnames(x) <- xmlnames
	for(i in 1:ncol(x)) 
		x[,i] <- type.convert(as.character(x[,i]))
	return(x)
}

#' Return a data.frame of roll-call votes  
#'
#' \code{voteNomDetail} shows the individual votes of members from a roll-call vote 
#'
#' @param xml The xml returned from \code{getNYTCongress} with type="roll.call" (default)
#' @return data.frame containing the vote for each member
#' @keywords data
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://developer.nytimes.com/}
#' @seealso \code{\link{getNYTCongress}} 
#' @examples
#' \dontrun{
#' xml <- getNYTCongress(type="nomination.votes", 110, "senate")
#' head(x <- voteNomDetail(xml))
#' x.dem <- do.call("rbind",x$democratic)
#' x.dem <- apply(x.dem, 2, function(x) type.convert(as.character(x)))
#' barplot(apply(x.dem, 2, sum))
#' }
voteNomDetail <- function(xml) {
	xmlnames <- names.XMLNode(getNodeSet(xml,"//vote")[[1]])
	x <- getNodeSet(xml, "//votes")[[1]]
	x <- lapply(xmlToList(x),function(x) {x <- as.data.frame(t(x)); colnames(x)=1:length(x); return(x)})
	x <- do.call(rbind.fill,x)
	colnames(x) <- xmlnames
	return(x)
}


#' Sample XML data for a roll call vote from the NY Times.
#'
#' Sample XML data for a roll call vote from the NY Times. 
#' 
#' @name roll.call.xml
#' @docType data
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @seealso \code{\link{getNYTCongress}} 
#' @keywords data
# roxygen()

#' Sample XML data for members from the NY Times.
#'
#' Sample XML data for members from the NY Times. 
#' 
#' @name members.xml
#' @docType data
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @seealso \code{\link{getNYTCongress}} 
#' @keywords data
# roxygen()

#' Sample XML data for party votes from the NY Times.
#'
#' Sample XML data for party votes from the NY Times. 
#' 
#' @name party.votes.xml
#' @docType data
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @seealso \code{\link{getNYTCongress}} 
#' @keywords data
# roxygen()

#' Pull data from the NY Times API
#'
#' \tabular{ll}{
#' Package: \tab nytR\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1\cr
#' Date: \tab 2009-12-23\cr
#' License: \tab GPL (>= 2)\cr
#' LazyLoad: \tab no\cr
#' }
#'
#' Pulls congressional data from NY Times API.  Currently only exposes the Congress API.  Requires an API key from \url{http://developer.nytimes.com/}.
#' 
#' @name nytR-package
#' @aliases nytR
#' @docType package
#' @title Pulls data from NY Times API.
#' @author Shane Conway \email{shane.conway@@gmail.com}
#' @references
#' \url{http://developer.nytimes.com/}
#' @keywords package
# roxygen()

# create sample data
#roll.call.xml <- getNYTCongress(110, "senate", 2, 194)
#members.xml <- getMemberDetails("W000779")
#party.votes.xml <- getNYTCongress(type="party.votes", 110, "senate")
#save(roll.call.xml, members.xml, party.votes.xml, file="C:/R/nytR/data/xml.Rda")