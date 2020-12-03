CrucialStates <- function(elecSheet)
{
  require(tidyverse)
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
  otherVotes <- popVotes[names(popVotes) != winnerPopName]
  elecSheet$Margin <- 0
  for(i in 1:nrow(elecSheet))
  {
    if(elecSheet[[i,totalPopCol]] == 0)
    {
      elecSheet$Margin[[i]] <- 2*elecSheet[[i,winnerElecName]]/elecSheet[[i,totalElecCol]]-1
    }
    else
    {
      elecSheet$Margin[[i]] <- (elecSheet[[i,winnerPopName]]-max(otherVotes[i,]))/elecSheet[[i,totalPopCol]]
    }
  }
  elecSheet <- elecSheet %>% arrange(-Margin)
  quota <- ceiling(sum(elecSheet[totalElecCol])/2+0.5)
  elecSheet$CumulativeVote <- cumsum(elecSheet[[winnerElecName]])
  pivotIndex <- min(which(elecSheet$CumulativeVote >= quota))
  prePivotVote <- elecSheet$CumulativeVote[[pivotIndex-1]]
  lastIndex <- max(which(elecSheet[[winnerElecName]] > 0))
  targetIndices <- pivotIndex:lastIndex
  listIndices <- list()
  j <- 1
  for(i in 1:length(targetIndices))
  {
    if(prePivotVote + elecSheet[[winnerElecName]][[targetIndices[[i]]]] < quota)
    {
      prePivotVote <- prePivotVote + elecSheet[[winnerElecName]][[targetIndices[[i]]]]
    }
    else
    {
      listIndices[[j]] <- targetIndices[[i]]
      j <- j+1
    }
  }
  targetIndices <- unlist(listIndices)
  returnSheet <- elecSheet[targetIndices,]
  returnSheet$Share <- 1/nrow(returnSheet)
  return(elecSheet[targetIndices,])
}
