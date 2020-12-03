WeightByMargin <- function(elecSheet)
{
  elecSheet <- AddStatePopMargin(elecSheet)
  tempSheet <- elecSheet
  tempSheet$Weight <- 1/sum(tempSheet$RawMargin != 0)
  tempSheet$InvMarg <- (1/elecSheet$RawMargin)
  typical <- mean(tempSheet$InvMarg[tempSheet$RawMargin != 0],na.rm = TRUE)
  tempSheet$InvMarg[tempSheet$RawMargin == 0] <- typical
  tempSheet$Weight <- tempSheet$Weight*tempSheet$InvMarg/typical
  elecSheet$Weight <- tempSheet$Weight
  return(elecSheet)
}
