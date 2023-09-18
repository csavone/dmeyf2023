#CZ1
set.seed(729193)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres

ftirar <- function(prob, qty) {
  return(sum(runif(qty) < prob))
}


# defino los jugadores
mejor <- 0.7
peloton <- rep(0.6,99)
jugadoras <- c(peloton, mejor)
length(jugadoras)

# veo que tiene el vector
jugadoras

mejor_ganadora <- 0

for (i in 1:10000) { # diez mil experimentos
  
  vaciertos <- mapply(ftirar, jugadoras, 100) # 100 tiros libres cada jugador
  
  mejor_ronda <- which.max(vaciertos)
  if (mejor_ronda == length(jugadoras)) mejor_ganadora <- mejor_ganadora + 1
}

cat("cz1:", mejor_ganadora)

#CZ2
set.seed(729193)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres

ftirar <- function(prob, qty) {
  return(sum(runif(qty) < prob))
}


# defino los jugadores
mejor <- 0.7
peloton <- rep(0.6,199)
jugadoras <- c(peloton, mejor)
length(jugadoras)

# veo que tiene el vector
jugadoras

mejor_ganadora <- 0

for (i in 1:10000) { # diez mil experimentos
  
  vaciertos <- mapply(ftirar, jugadoras, 100) # 100 tiros libres cada jugador
  
  mejor_ronda <- which.max(vaciertos)
  if (mejor_ronda == length(jugadoras)) mejor_ganadora <- mejor_ganadora + 1
}


cat("cz2:", mejor_ganadora)


#CZ3
set.seed(729193)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres

ftirar <- function(prob, qty) {
  return(sum(runif(qty) < prob))
}


# defino los jugadores
mejor <- 0.7
peloton <- rep(0.60,1)
jugadoras <- c(peloton, mejor)
length(jugadoras)

# veo que tiene el vector
jugadoras

mejor_ganadora <- 0

for (i in 1:10000) { # diez mil experimentos
  
  vaciertos <- mapply(ftirar, jugadoras, 100) # 100 tiros libres cada jugador
  
  mejor_ronda <- which.max(vaciertos)
  if (mejor_ronda == length(jugadoras)) mejor_ganadora <- mejor_ganadora + 1
}


cat("cz3:", mejor_ganadora)

#cz4

set.seed(729193)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres

ftirar <- function(prob, qty) {
  return(sum(runif(qty) < prob))
}


# defino los jugadores
mejor <- 0.7
peloton <- rep(0.6,99)
jugadoras <- c(peloton, mejor)
length(jugadoras)

# veo que tiene el vector
jugadoras

mejor_ganadora <- 0

for (i in 1:10000) { # diez mil experimentos
  
  vaciertos <- mapply(ftirar, jugadoras, 10) # 10 tiros libres cada jugador
  
  mejor_ronda <- which.max(vaciertos)
  if (mejor_ronda == length(jugadoras)) mejor_ganadora <- mejor_ganadora + 1
}


cat("cz4:", mejor_ganadora)


#cz5
set.seed(729193)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres

ftirar <- function(prob, qty) {
  return(sum(runif(qty) < prob))
}


# defino las jugadoras
mejor <- 0.7
peloton <- rep(0.6,99)
jugadoras <- c(peloton, mejor)
length(jugadoras)

# veo que tiene el vector
jugadoras

mejor_ganadora  <- 0
for (i in 1:10000) {
  vaciertos <- mapply(ftirar, jugadoras, 100) # cada jugadora tira 100 tiros
  mejores_5 <- which(vaciertos >= sort(vaciertos, decreasing=T)[5], arr.ind=TRUE)[1:5]
  aciertos1 <- vaciertos[mejores_5]
  
  aciertos2 <- mapply(ftirar, jugadoras[mejores_5], 100)
  aciertos3 <- mapply(ftirar, jugadoras[mejores_5], 100)
  
  v_pos_mejores_por_ronda = mejores_5[c( which.max(aciertos1),which.max(aciertos2),which.max(aciertos3))]
  pos_mejor_todas= v_pos_mejores_por_ronda[which.max(c( max(aciertos1),max(aciertos2),max(aciertos3)))]
  
  if (pos_mejor_todas == length(jugadoras)) mejor_ganadora <- mejor_ganadora + 1
}
cat("cz5:", mejor_ganadora)

#CZ7
set.seed(729193)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres

ftirar <- function(prob, qty) {
  return(sum(runif(qty) < prob))
}


# defino las jugadoras
mejor <- 0.7
peloton <- rep(0.6,99)
jugadoras <- c(peloton, mejor)
length(jugadoras)

# veo que tiene el vector
jugadoras


mejor_ganadora  <- 0
for (i in 1:10000) {
  vaciertos <- mapply(ftirar, jugadoras, 100) # cada jugadora tira 100 tiros
  mejores_5 <- which(vaciertos >= sort(vaciertos, decreasing=T)[5], arr.ind=TRUE)[1:5]
  
  aciertos2 <- mapply(ftirar, jugadoras[mejores_5], 100)
  pos_mejor_segunda= mejores_5[which.max(aciertos2)]
  
  if (pos_mejor_segunda == length(jugadoras)) mejor_ganadora <- mejor_ganadora + 1
}
cat("cz7:", mejor_ganadora)


#CZ6, CZ9: LA PRUEBA ES CON UNA JUGADORA POR LO QUE ESTA NO HABRÀ "MEJOR". ORDENE POR CANTIDAD DE ENCESTES. CZ6: 80 DE 100. CZ9:701 DE 1000
#CZ8: ES SIMILAR AL CZ1, PERO LOGRA 85 ENCESTES EN 100 TIROS (Y 790 DE 1000). ORDENE C1 Y C8 POR CANTIDAD DE ENCESTES
