cuenta_puntos<-function(a,b,n){
  sum(replicate(n,play_game_cuenta(a,b)))/n
}
