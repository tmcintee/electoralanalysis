NationalPopMargin <- function(elecSheet)
{
  elecVoteCols <- grep(pattern = "^Elec",x = names(elecSheet))
  popVoteCols <- grep(pattern = "^Pop",x = names(elecSheet))
  totalElecCol <- grep(pattern = "^Total[._ ]Elec",x = names(elecSheet))
  totalPopCol <- grep(names(elecSheet),pattern = "^Total[._ ]Pop")
  elecVotes <- elecSheet[elecVoteCols]
  popVotes <- elecSheet[popVoteCols]
  winnerIndex <- which.max(colSums(elecVotes))
  winnerElecName <- names(elecVotes)[[winnerIndex]]
  nameTail <- str_trunc(winnerElecName,3,"left",ellipsis = "")
  winnerPopName <- names(popVotes)[[which(str_ends(names(popVotes),nameTail))]]
  winnerVotes <- popVotes[names(popVotes) == winnerPopName]
  otherVotes <- popVotes[names(popVotes) != winnerPopName]
  winnerVoteTotal <- sum(winnerVotes)
  otherVoteTotals <- colSums(otherVotes)
  margin <- winnerVoteTotal - max(otherVoteTotals)
  return(margin)
}
