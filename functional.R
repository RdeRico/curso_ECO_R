#This script is for playing with loops and functions, using null models

#A simple null model to test correlations----

# Create some data

abundances <- c(1,3,4,7,8,13)
body_size <- c(9,6,3,3,1,1)

plot(abundances, body_size)

cor(abundances, body_size)
cor.test(abundances,body_size)


#let's brake the pattern
cor(abundances, sample(body_size, size = length(body_size), replace = F))

#let's start with the loops
out <- c()
for(i in 1:999){
  out [i] <- cor(abundances, sample(body_size, size = length(body_size), replace = F))
}
hist(out)

corr <- cor(abundances, body_size)
lines(c(corr,corr),c(0,170), col = 'red')

#P-value
length(which(out < corr)) / length(out)


#Is our community uneven?
#let's calculate Pielou's index
p<- (abundances/sum(abundances))
s<- -sum(p*log2(p))
J<-s/log2(length(abundances))

J<- function(x){
  p<- (x/sum(x))
  s<- -sum(p*log2(p))
  s/log2(length(x))
}
eve<- J(abundances)

rand<- sample(x = c(1:6), size= sum(abundances), replace = T)
null<-table(rand)
J(null)

out<-c()
for (i in 1:999) {
  rand<- sample(x = c(1:6), size= sum(abundances), replace = T)
  null<-table(rand)
  out[i] <- J(null)
}
hist(out)
lines(c(eve, eve), c(0,210), col='red')


#P-value

length(which(out < eve))/length(out)
