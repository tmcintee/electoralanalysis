LosingStates <- function(elecSheet, marginSort = "pct")
{
  elecVoteCols <- grep(pattern = "^Elec",x = names(elecSheet))
  popVoteCols <- grep(pattern = "^Pop",x = names(elecSheet))
  totalElecCol <- grep(pattern = "^Total[._ ]Elec",x = names(elecSheet))
  totalPopCol <- grep(names(elecSheet),pattern = "^Total[._ ]Pop")
  elecVotes <- elecSheet[elecVoteCols]
  popVotes <- elecSheet[popVoteCols]
  winnerIndex <- which.max(colSums(elecVotes))
  winnerElecName <- names(elecVotes)[[winnerIndex]]
  losers <- elecVotes[[totalElecCol]] > 2 * elecVotes[[winnerIndex]]
  return(elecSheet[losers,]$State)
}
