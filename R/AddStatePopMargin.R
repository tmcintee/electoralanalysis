AddStatePopMargin <- function(elecSheet)
{
  require(tidyverse)
  elecVoteCols <- grep(pattern = "^Elec",x = names(elecSheet))
  popVoteCols <- grep(pattern = "^Pop",x = names(elecSheet))
  totalElecCol <- grep(pattern = "^Total[._ ]Elec",x = names(elecSheet))
  totalPopCol <- grep(names(elecSheet),pattern = "^Total[._ ]Pop")
  elecVotes <- elecSheet[elecVoteCols]
  popVotes <- elecSheet[popVoteCols]
  winnerIndex <- which.max(colSums(elecVotes,na.rm = TRUE))
  winnerElecName <- names(elecVotes)[[winnerIndex]]
  nameTail <- str_trunc(winnerElecName,3,"left",ellipsis = "")
  winnerPopName <- names(popVotes)[[which(str_ends(names(popVotes),nameTail))]]
  otherVotes <- popVotes[names(popVotes) != winnerPopName]
  elecSheet$RawMargin <- 0
  for(i in 1:nrow(elecSheet))
  {
    if(elecSheet[[i,totalPopCol]] == 0)
    {
      elecSheet$RawMargin[[i]] <- 0
    }
    else
    {
      elecSheet$RawMargin[[i]] <- (elecSheet[[i,winnerPopName]]-max(otherVotes[i,]))
    }
  }
  elecSheet$Margin <- elecSheet$RawMargin / max(elecSheet[[totalPopCol]],1)
  return(elecSheet)
}
