WeightedAverageTwoHues <- function(hue1,hue2,weight1=1,weight2=1)
{
  #safety: If there are hue numbers outside of [0,360), fix them.
  hue1 <- hue1 %% 360
  hue2 <- hue2 %% 360
  #if the difference is < 180, the average of the two lies along the shorter segment between the two.
  if(abs(hue1-hue2) <= 180)
  {
    newHue <- (hue1*weight1 + hue2*weight2)/(weight1+weight2)
  }
  else
  {
    # Necessarily, one and only one hue is >180 for this to be the case. We will turn this hue into a negative number, so that adding them together puts us near to 0 (as we should be).
    if(hue1 > 180)
    {
      hue1 <- hue1 - 360
    }
    else if(hue2 > 180)
    {
      hue2 <- hue2 - 360
    }

    newHue <- (hue1*weight1 + hue2*weight2)/(weight1+weight2)
    #newHue could be negative at this point. If so, add 360 to make it positive.
    if(newHue < 0)
    {
      newHue <- newHue + 360
    }
  }
  return(newHue)
}
