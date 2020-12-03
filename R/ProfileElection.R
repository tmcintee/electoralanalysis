ProfileElection <- function(elecSheet)
{

  pivot <- PivotalState(elecSheet)
  crits <- CriticalStates(elecSheet)
  crucial <- CrucialStates(elecSheet)
  pivotEntry <- as.character(pivot$State)
  critEntry <- ""
  if(nrow(crits)==0)
  {
    critEntry <- "None."
  }
  else
  {
    for(i in 1:nrow(crits))
    {
      critEntry <- paste0(critEntry,crits$State[[i]])
      if(i != nrow(crits))
      {
        critEntry <- paste0(critEntry,", ")
      }
    }
  }
  crucialEntry <- ""
  for(i in 1:nrow(crucial))
  {
    crucialEntry <- paste0(crucialEntry,crucialEntry$State[[i]])
    if(i != nrow(crits))
    {
      critEntry <- paste0(critEntry,", ")
    }
  }
  labels <- c("Pivotal state","Critical states","Crucial states")
  entries <- c(pivotEntry,critEntry,crucialEntry)
  cat("Pivotal State: ", pivotEntry, '\n')
  cat("Critical state(s): ", pivotEntry, '\n')
  cat("Crucial State(s): ", pivotEntry, '\n')

  returnFrame <- data.frame(x = labels, States = entries)
  return(returnFrame)
}
