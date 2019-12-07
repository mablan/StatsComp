play_game_cuenta<-function(a, b) {
  state <- c(0,0, 1)
  cuenta<-0
  while (status(state[1], state[2]) == "sin terminar") {
    # show(state)
    state <- play_point(state, a, b)
    cuenta<-cuenta+1
  }
    return(cuenta)
}

####Hace la tabla

a=seq(0.1,0.9,0.1)
b=seq(0.1,0.9,0.1)
tabla=matrix(0,9,9)
n=2500
cuenta_puntos.V=Vectorize(cuenta_puntos)
set.seed(1)
tabla=outer(a,b,"cuenta_puntos.V",n)
colnames(tabla)= paste("b=",b)
rownames(tabla)=paste("a=",a)
tabla
