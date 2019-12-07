#Simulación de un juego 
set.seed(1)
n=2^k
ejex=log(n)/log(2)
ejey=numeric(12)

for (i in 1:12) {
  ejey[i]=prob_ganar(0.55,0.45,n[i])
  std.dev=ejey*(1-ejey)/n
}

phat=ejey
plot(ejex,phat,type="p",main="Probabilidad estimada para diferentes tamaños de muestra",xlab="log(n)/log(2)",ylab="p.hat")

##############################
#Probabilidad de ganar:
a=seq(0.1,0.9,0.1)
b=seq(0.1,0.9,0.1)
tabla=matrix(0,9,9)
n=2500
prob_ganar.V=Vectorize(prob_ganar)
set.seed(1)
tabla=outer(a,b,"prob_ganar.V",n)
colnames(tabla)= paste("b=",b)
rownames(tabla)=paste("a=",a)


####################################
