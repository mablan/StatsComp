prob_ganar<-function(a,b,n){
  sum(as.integer(replicate(n,play_game(a,b))))/n
}
