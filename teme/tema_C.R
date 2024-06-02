#C1.a
permutare <- function(n) {
  U <- runif(n)
  perm <- order(U)
  return(perm)
}

n <- 10
perm <- permutare(n)
print(perm)

#C1.b
generare <- function(n, k) {
  bit_strings <- list()
  for (i in 1:n) {
    string <- sample(0:1, k, replace = TRUE)
    bit_strings[[i]] <- string
  }
  return(bit_strings)
}

compara <- function(s, t) {
  s <- unlist(s)
  s <- as.numeric(s)
  t <- unlist(t)
  t <- as.numeric(t)
  L <- min(length(s), length(t))
  for (i in 1:L) {
    if (s[i] < t[i]) return(TRUE)
    if (t[i] < s[i]) return(FALSE)
  }
  if (length(s) != length(t)) {
    if (length(s) > length(t)) {
      b <- s; ok <- 0
    } else {
      b <- t; ok <- 1
    }
    for (i in (L + 1):length(b)) {
      bit <- sample(0:1, 1)
      if (ok == 1) {
        if (bit < b[i]) return(TRUE)
        if (bit > b[i]) return(FALSE)
      } else {
        if (bit < b[i]) return(FALSE)
        if (bit > b[i]) return(TRUE)
      }
    }
  } else {
    while (TRUE) {
      bit1 <- sample(0:1, 1)
      bit2 <- sample(0:1, 1)
      if (bit1 < bit2) return(TRUE)
      if (bit2 < bit1) return(FALSE)
    }
  }
}

bit_strings<-generare(10,4)
s<-bit_strings[1]
t<-bit_strings[2]
s
t
compara(s,t)

#C1.c
randomized_quick_sort<-function(lista) {
  if (length(lista) <= 1) {
    return(lista)
  }
  index <- sample(1:length(lista), 1)
  x<-lista[[index]]
  st<-list(); dr<-list()
  for(i in 1:length(lista)) 
    if(i!=index){
      if(compara(lista[[i]],x)==TRUE)
        st <- c(st, list(lista[[i]]))
      else
        dr<-c(dr, list(lista[[i]]))
    }
  sorted_st<-randomized_quick_sort(st)
  sorted_dr<-randomized_quick_sort(dr)
  return (c(sorted_st, list(x), sorted_dr))
}

lista<-generare(4,4)
lista
randomized_quick_sort(lista)

#C1.d
randomized_quick_sort_2<-function(lista,indici) {
  if (length(lista) <= 1) {
    return(list(lista=lista,indici=indici))
  }
  index <- sample(1:length(lista), 1)
  x<-lista[[index]]
  st<-list(); dr<-list()
  st_indici <- c();  dr_indici <- c()
  for(i in 1:length(lista)) 
    if(i!=index){
      if(compara(lista[[i]],x)==TRUE) {
        st <- c(st, list(lista[[i]]))
        st_indici<-c(st_indici,indici[i])
      }
      else {
        dr<-c(dr, list(lista[[i]]))
        dr_indici<-c(dr_indici,indici[i])
      }
    }
  sorted_st<-randomized_quick_sort_2(st,st_indici)
  sorted_dr<-randomized_quick_sort_2(dr,dr_indici)
  lista <- c(sorted_st$lista, list(x), sorted_dr$lista)
  indici<-c(sorted_st$indici,indici[index],sorted_dr$indici)
  return (list(lista=lista,indici=indici))
}

generare_permutare <- function(n, k) {
  lista <- generare(n, k)
  indici <- 1:n
  sorted_result <- randomized_quick_sort_2(lista, indici)
  return(sorted_result$indici)
}

generare_permutare(5,4)

#C2.a
max_cut = function(graph) {
  V <- names(graph)
  n <- length(V) %/% 2
  A <- sample(V, n)
  B <- setdiff(V, A)
  cut_size <- 0
  cut_edges <- list()
  for (u in A) 
    for (v in graph[[u]]) 
      if (v %in% B) {
        cut_edges <- append(cut_edges, list(c(u, v)))
        cut_size <- cut_size + 1
      }
  return(list(cut_size = cut_size, cut_edges = cut_edges))
}

graph <- list(
  '0' = c('2'),
  '1' = c('2', '3'),
  '2' = c('0', '1', '3'),
  '3' = c('1', '2')
)
max_cut(graph)

#C2.b
max_cut_2 = function(graph, num_trials) {
  max_result<-NULL
  max_cut_size <- 0
  for (i in 1:num_trials) {
    result <- max_cut(graph)
    if (result$cut_size > max_cut_size) {
      max_result = result
      max_cut_size = result$cut_size
    }
  }
  return(max_result)
}

graph <- list(
  '0' = c('2'),
  '1' = c('2', '3'),
  '2' = c('0', '1', '3'),
  '3' = c('1', '2')
)
max_cut_2(graph,10)
