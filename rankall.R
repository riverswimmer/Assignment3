## function to return data frame of hospital name and state for all states for given
## outcome and ranking arguments

rankall <- function (outcome, num = "best") {
	##import outcome measures 
	ao <- read.csv("outcome-of-care-measures.csv", colClasses = "character",
				na.strings="Not Available", stringsAsFactors=FALSE)
	
	
	##check validity of argument
		if (outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE) {
                stop("invalid outcome")
	}
	
	## subset table based outcome 
	if(outcome == "heart attack") {ocol <- 11}
	else if(outcome == "heart failure") {ocol <- 17}
	else {ocol <- 23}
	so <- ao[, c(2,7,ocol)]
	
	##clean up NAs then order by state alphabetical, outcome ranking then hospital name alphabetical
	soc <- so[complete.cases(so),]
	sooc <- soc[order(soc[,2],as.numeric(soc[,3]),soc[,1]),]

	##create list of states and blank data frame
	states <- unique(sooc[,2])
	ssooc <- data.frame()

	##for loop to construct list of results for each state
	for (i in 1:length(states)){
 		state.data <- sooc[sooc[,1] == states[i],]
		if(num == "best") {rank <- 1}
			else if(num == "worst") {rank <- nrow(state.data)}
			else {rank <- num}
		if (rank < nrow(state.data)) {ssooc <- rbind(ssooc, data.frame(state.data[rank,c(1,2)]))}
			else {ssooc <- rbind(ssooc, data.frame(c("<NA>",states[i])))}
	}

	##return 1st ranked in subset data
	print(ssooc)
}
