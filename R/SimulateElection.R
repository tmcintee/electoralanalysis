SimulateElection <- function(elecSheet)
{
  totalElecCol <- grep(pattern = "^Total[._ ]Elec",x = names(elecSheet))
  totalPopCol <- grep(pattern = "^Total[._ ]Pop",x = names(elecSheet))
  stateCol <- grep(pattern = "^State",x = names(elecSheet))
  short <- elecSheet[c(stateCol,totalElecCol,totalPopCol)]
  #Add a single dummy voter to legislative contests.
  short[[3]][short[[3]] == 0] <- 1
  #Without replacement neatly avoids margin ties.
  short$Popular.Vote.A1 <- round(short[[3]]*sample(0.001*c(1:1000),size = nrow(short)))
  short$Popular.Vote.B2 <- short[[3]] - short$Popular.Vote.A1
  short$Elec.Vote.A1 <- short[[2]]*(short$Popular.Vote.A1 >= short$Popular.Vote.B2)
  short$Elec.Vote.B2 <- short[[2]] - short$Elec.Vote.A1
  return(short)
}
