KS_ecdf <- function(ecdf1,ecdf2,vals)
{
  k <- max(abs(ecdf1(vals) - ecdf2(vals)))
  return(k)
}
