## function to return name ranked hospital given state and 30 day mortality outcome
## measure

rankhospital <- function (state, outcome, num = "best") {
	##import outcome measures 
	ao <- read.csv("outcome-of-care-measures.csv", colClasses = "character",
				na.strings="Not Available", stringsAsFactors=FALSE)
	
	
	##check validity of arguments
	if (state %in% unique(ao[,7]) == FALSE) {
                stop("invalid state")
	}
	if (outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE) {
                stop("invalid outcome")
	}
	
	## subset table based on state and outcome 
	if(outcome == "heart attack") {ocol <- 11}
	else if(outcome == "heart failure") {ocol <- 17}
	else {ocol <- 23}
	so <- ao[ao[,7] == state, c(2,7,ocol)]
	
	##clean up NAs then order by outcome ranking then hospital name alphabetical
	soc <- so[complete.cases(so),]
	sooc <- soc[order(as.numeric(soc[,3]),soc[,1]),]

	##set ranking criteria
	if(num == "best") {rank <- 1}
	else if(num == "worst") {rank <- nrow(sooc)}
	else {rank <- num}
	if (rank > nrow(sooc)) {return(NA)}

	##return 1st ranked in subset data
	print(sooc[rank,1])
}
