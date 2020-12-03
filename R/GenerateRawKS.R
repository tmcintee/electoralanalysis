GenerateRawKS <- function(elecListList, functionChoice, baseECDF, weight = "none")
{
  outputVals <- list()
  for(i in 1:length(elecListList))
  {
    elecList <- elecListList[[i]]
    outFrame <- ProcessListOfElections(elecList,functionChoice)
    if(weight == "none")
    {
      ecdf1 <- ecdf(outFrame$Total.Elec.Vote)
    }
    else
    {
      ecdf1 <- spatstat::ewcdf(outFrame$Total.Elec.Vote,outFrame[[weight]])
    }
    # Handling hypothetical cases from 1 to 100 electoral votes. This code will break if applied to other examples.
    outputVals[[i]] <- KS_ecdf(ecdf1,baseECDF,c(1:100))
  }
  outputVals <- unlist(outputVals)
  return(outputVals)
}
