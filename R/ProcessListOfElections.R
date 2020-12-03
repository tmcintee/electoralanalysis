ProcessListOfElections <- function(.l, .f)
{
  require(tidyverse)
  s_1 <- lapply(.l,.f)
  #Add name column to track origin:
  for(i in 1:length(s_1))
  {
    if(nrow(s_1[[i]]) > 0)
    {
      s_1[[i]]$Election <- names(.l)[[i]]
    }
  }
  #Turn into single data frame:
  s_1 <- bind_rows(s_1)
  return(s_1)
}
