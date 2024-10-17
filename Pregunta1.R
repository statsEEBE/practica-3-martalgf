#PREGUNTA 1

#a) 
#Bernoulli
x <- c(0,1) # 0 = no y 1 = sí
f <- c(0.68, 0.32) # p = 0.32 y 1-p = 0.63
 
plot(x, f, type="h",ylim=c(0,1), col="red")

n <- 43 #Personas ecuestadas
sample(x, n, f, replace=TRUE) #Sacar un número aleatorios de una función de masa de probabilidad
Y <- function(i){sum(sample(x, n, f, replace=TRUE))} #Crear un función para poder reptetir infinitas veces la encuesta /  ensayo de Bernoulli
Y(i)

encuestas <- sapply(1:400000, Y)
encuestas
#Cada uno de los números es un experiemento aleatorio que esconde 43 ensayos de Bernoulli
table(encuestas)
fr <- table(encuestas)/400000
barplot(fr)
fr["13"]

#Binomial
dbinom(13, 43, 0.32)
#Probalidad de tener 13 muestras positivas (que digan que sí) en una encuesta de 43, cuando la probabilidad de que alguien diga se sí es del 0.32
y <- 0:43
plot(y, dbinom(y, 43, 0.32), type="h", col="red") #tener y respuestas postivas en un encuesta de 43, cuando la probabilidad de una repuesta postiva es de 0.32

#b)
# P(X=17); Z -> Binom(44, 0.32)
dbinom(17, 44, 0.32)
y <- 0:44
plot(y, dbinom(y, 44, 0.32), type="h", col="red")   

#dbinom() = función de massa de probablidad
#pbinom() = función de distribución

#c)
#Nos preguntan que probailidad hay de que menos de 17 personas te digan que sí -> P(X<17)=P(X<=16)
pbinom(16,44,0.32)
#mediana
qbinom(0.25, 44, 0.32) #quartil
#F-1(0.5)=Xm ; la funció inversa en 0.5

24*0.68 #media
24*0.68*0.32 #varianza
qbinom(0.25, 24, 0.68)

y <- 0:43
plot(y, pbinom(y, 24,0.68, type="h", col="red"))

#d) 
#E(X) = n*p
# v
