CriticalStates <- function(elecSheet,weightByMargin = FALSE)
{
  elecVoteCols <- grep(pattern = "^Elec",x = names(elecSheet))
  totalElecCol <- grep(pattern = "^Total[._ ]Elec",x = names(elecSheet))
  elecVotes <- elecSheet[elecVoteCols]
  winnerIndex <- which.max(colSums(elecVotes))
  winnerElecName <- names(elecVotes)[[winnerIndex]]
  winnerEV <- sum(elecSheet[[winnerElecName]])
  quota <- sum(elecSheet[totalElecCol])/2
  if(winnerEV > quota)
  {
    critIndex <- which(elecSheet[[winnerElecName]] >= winnerEV-quota)
  }
  else
  {
    critIndex <- which(elecSheet[[totalElecCol]]-elecSheet[[winnerElecName]] >=  quota-winnerEV)
  }
  critSheet <- elecSheet[critIndex,]
  if(nrow(critSheet) > 0 & weightByMargin == TRUE)
  {
    critSheet <- WeightByMargin(critSheet)
    critSheet$Share <- 1/nrow(critSheet)
  }
  return(critSheet)
}
CriticalStatesMargins <- function(elecSheet,weightByMargin = TRUE)
{
  elecVoteCols <- grep(pattern = "^Elec",x = names(elecSheet))
  totalElecCol <- grep(pattern = "^Total[._ ]Elec",x = names(elecSheet))
  elecVotes <- elecSheet[elecVoteCols]
  winnerIndex <- which.max(colSums(elecVotes))
  winnerElecName <- names(elecVotes)[[winnerIndex]]
  winnerEV <- sum(elecSheet[[winnerElecName]])
  quota <- sum(elecSheet[totalElecCol])/2
  if(winnerEV > quota)
  {
    critIndex <- which(elecSheet[[winnerElecName]] >= winnerEV-quota)
  }
  else
  {
    critIndex <- which(elecSheet[[totalElecCol]]-elecSheet[[winnerElecName]] >=  quota-winnerEV)
  }
  critSheet <- elecSheet[critIndex,]
  if(nrow(critSheet) > 0 & weightByMargin == TRUE)
  {
    critSheet <- WeightByMargin(critSheet)
    critSheet$Share <- 1/nrow(critSheet)
  }
  return(critSheet)
}
