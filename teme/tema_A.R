#A1.a
probabilitati<-function(lambda,p,n,m,k) {
  values <- k:m
  poisson_probs <- dpois(values, lambda)
  geom_probs <- dgeom(values, p)
  binom_probs <- dbinom(values, n, p)
  return(list(poisson = poisson_probs, geometric = geom_probs, binomial = binom_probs))
}

prob <- probabilitati(3.5,0.4,10,5,2)
print(prob)

#A1.b 
probabilitati_grafic<-function(lambda,p,n,m,k) {
  values<-k:m
  layout(matrix(1:3, ncol = 1))
  prob<-probabilitati(lambda,p,n,m,k)
  barplot(prob$poisson, values, main = 'Distributia Poisson', xlab = 'Valori', ylab = 'Probabilitati', col = 'red')
  barplot(prob$geometric, values, main = 'Distributia Geometrica', xlab = 'Valori', ylab = 'Probabilitati', col = 'red')
  barplot(prob$binomial, values, main = 'Distributia Binomiala', xlab = 'Valori', ylab = 'Probabilitati', col = 'red')
  layout(matrix(1))
}

probabilitati_grafic(3.5,0.4,10,5,2)

#A1.c
k0_min = function(lambda) {
  k0 = 0
  sum = 0
  upper_bound = 1 - 10 ^ (- 6)
  while (sum <= upper_bound) {
    sum = sum + dpois(k0, lambda)
    if (sum <= upper_bound)
      k0 = k0 + 1
  }
  return (k0)
}

k0_min(3.5)

#A2.a
calcul_medii <- function() {
  tablou <- read.csv("note_PS.csv",header=T)
  P <- tablou[['P']]
  S <- tablou[['S']]
  frecvente_P <- as.vector(table(P))
  frecvente_relative_P <- frecvente_P / sum(frecvente_P)
  frecvente_S <- as.vector(table(S))
  frecvente_relative_S <- frecvente_S / sum(frecvente_S)
  media_P <- mean(P)
  media_S <- mean(S)
  rezultate <- list(frecvente_absolute_P = frecvente_P, frecvente_relative_P = frecvente_relative_P,
                    frecvente_absolute_S = frecvente_S, frecvente_relative_S = frecvente_relative_S,
                    medie_P = media_P, medie_S = media_S)
  
  return(rezultate)
}
rezultate<-calcul_medii()
print(rezultate)

#A2.b
elimina_aberante <- function(nume_fisier, nume_esantion) {
  tablou <- read.csv(nume_fisier)
  esantion <- tablou[[nume_esantion]]
  media <- mean(esantion)
  sd_val <- sd(esantion)
  lower_bound <- media - 3 * sd_val
  upper_bound <- media + 3 * sd_val
  esantion_curat <- esantion[esantion > lower_bound & esantion < upper_bound]
  intervale <- cut(esantion_curat, breaks = seq(1, 10, by = 1))
  frecvente <- table(intervale)
  barplot(frecvente, xlab = "Intervale", ylab = "Frecvente", col = "blue")
  return(esantion_curat)
}

esantion_curat_P <- elimina_aberante("note_PS.csv", "P")
esantion_curat_S <- elimina_aberante("note_PS.csv", "S")
print(esantion_curat_P)
print(esantion_curat_S)
