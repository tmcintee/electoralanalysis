AddHSVColumns <- function(elecSheet,
                          hues = c(240,0,80,160),
                          sats = c(100,100,100,100),
                          vals = c(0,0,0,0,0,0),
                          alphas = c(0,0,0,0,20,20),
                          winner = 1)
{
  nameInfo <- names(elecSheet)
  #Hardcoded order: D, R / W / F, then alphabetical.
  nameAdj <- str_replace(nameInfo,".D",".000D")
  nameAdj <- str_replace(nameAdj,".R",".001R")
  nameAdj <- str_replace(nameAdj,".W",".002W")
  nameAdj <- str_replace(nameAdj,".F",".003F")
  elecSheet <- elecSheet[,order(nameAdj)]
  elecVoteCols <- grep(pattern = "^Elec",x = names(elecSheet))
  clippedSheet <- elecSheet[elecVoteCols]
  elecSheet$hue <- 0
  elecSheet$sat <- 0
  elecSheet$val <- 0
  elecSheet$alpha <- 0
  for(i in 1:nrow(clippedSheet))
  {
    winnerIndex <- which.max(clippedSheet[i,])
  }
  return(elecSheet)
}
