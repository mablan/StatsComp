play_point<-function(estado,a,b) {
  x=estado[1]
  y=estado[2]
  z=estado[3]
  
  #El jugador 1 tiene el servicio:
  if (z==1) {               
    u<-runif(1)
    #si gana o pierde  
    if (u<=a) x=x+1 else z=2
  } else {
    #El jugador 2 tiene el servicio
    u<-runif(1)
    #si pierde o gana  
    if (u<=b) z=1 else y=y+1
  }
  estado=c(x,y,z)
}
