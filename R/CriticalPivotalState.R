PivotalCriticalState <- function(elecsheet)
{
  crits <- CriticalStates(elecsheet)
  pivot <- PivotalState(elecsheet)
  critpivot <- crits[crits$State %in% pivot$State,]
  return(critpivot)
}
