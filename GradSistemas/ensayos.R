library("lattice")

grad = read.csv2("DatosGraduadosSistemas.csv")

xyplot(prom_general~años_estudio,groups=sexo,auto.key=T,data=grad)
xyplot(prom_general~año_grado,groups=sexo,auto.key=T,data=grad)
xyplot(prom_general~años_nacimiento|sexo,data=grad)

xyplot(prom_general~prom_ponderado|sexo,data=grad)

xyplot(prom_general~años_estudio|sexo,data=grad,pch=20,
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.loess(x, y) }
)

#Esta es interesante:
xyplot(prom_general~año_nacimiento,data=grad,pch=20,
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.loess(x, y) }
)

xyplot(prom_general~año_grado,data=grad,pch=20,
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.loess(x, y) }
)


histogram(~prom_general|sexo,data=gradMerida)

densityplot(~ prom_general, data = gradMerida, groups = sexo,
           plot.points = FALSE, ref = TRUE,
           auto.key = list(columns = 2))

densityplot(~ prom_ponderado, data = gradMerida, groups = sexo,
            plot.points = FALSE, ref = TRUE,
            auto.key = list(columns = 2))

bwplot(prom_ponderado~factor(sexo),data=grad,varwidth=T)
bwplot(prom_general~factor(sexo),data=grad,varwidth=T)

histogram(~prom_general|estado_procedencia,data=grad)

bwplot(prom_general~factor(años_estudio),data=grad)
#Este definitivamente es interesante:
bwplot(prom_ponderado~factor(años_estudio),data=grad,varwidth=T)

bwplot(prom_ponderado~factor(año_grado),data=grad,varwidth=T)

ae.cut<-equal.count(grad$años_estudio,4)


#Este definitivamente es interesante:
bwplot(prom_ponderado~factor(ae.cut),data=grad,varwidth=T)
splom()
xyplot(y ~ x | f,data =
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.lmline(x, y, col = 2)
       })
