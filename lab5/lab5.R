variabilaAleatoare <- function(valori, prob) 
{
  if(length(valori)!=length(prob)) 
  {
    stop("Numar diferit de valori si de probabilitati.")
  }
  if(any(prob<0))
  {
    stop("Toate probabilitatile trebuie sa fie pozitive.")
  }
  if(sum(prob)!=1)
  {
    stop("Suma probabilitatilor trebuie sa fie 1.")
  }
  x <- runif(1)
  probCumulate <- cumsum(prob)
  i <- match(TRUE, x <= probCumulate)
  return(valori[i])
}

valori <- c(3, 6, 10)
prob <- c(0.4, 0.1, 0.5)
test <- variabilaAleatoare(valori, prob)
test