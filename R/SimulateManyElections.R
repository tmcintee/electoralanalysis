SimulateManyElections <- function(elecList,repetitions)
{
  bigList <- list()
  for(i in 1:repetitions)
  {
    bigList[[i]] <- lapply(elecList,SimulateElection)
  }
  return(bigList)
}
