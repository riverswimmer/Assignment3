
## function to return name "best" hospital given state and 30 day mortality outcome
## measure

best <- function (state, outcome) {
	##import and order outcome measures allphabetically by 
	ao <- read.csv("outcome-of-care-measures.csv", colClasses = "character",
				na.strings="Not Available", stringsAsFactors=FALSE)
	
	
	##check validity of arguments
	if (state %in% unique(ao[,7]) == FALSE) {
                stop(print("invalid state"))
	}
	if (outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE) {
                stop(print("invalid outcome"))
	}
	
	## subset table based on state and outcome, order by outcome ranking
	## then hospital name alphabetical
	if(outcome == "heart attack") {scol <- 11}
	else if(outcome == "heart failure") {scol <- 17}
	else {scol <- 23}
	so <- ao[ao[,7] == state, c(2,7,scol)]
	soo <- so[order(as.numeric(so[,3]),so[,2]),]

	##return 1st ranked in subset data
	print(soo[1,1])
}
