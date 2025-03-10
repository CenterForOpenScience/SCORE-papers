# Customized rounding function that pushes 5 up rather than down
round_off <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- trunc(abs(x) * 10 ^ (digits + 1)) / 10
  z <- posneg * (floor(z + 0.5) / 10 ^ digits)
  return(z)
}