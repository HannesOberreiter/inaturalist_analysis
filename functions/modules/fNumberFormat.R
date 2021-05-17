# Number Format, round to 2 after decimal
# x = number
fNumberFormat <- function(x)
{
  as.numeric( format( round( x, 1 ), nsmall = 2))
}