# Dog Food Experiment

Height=runif(100000,5,50)

Weight=2*Height+rnorm(100000,5,3)

# Let p_i be the probability that dog i is in our experiment

vector=2*Height+rnorm(100000,5,3)

p=0.1*sqrt(vector)/max(vector)

library(Rlab)

I=rbern(100000,p) # I indicates whether the dog is in our sample

data=data.frame(cbind(I,Height,Weight))

experiment=data[data$I==1,]

# We randomally select 300 of our dogs to be treated, and the rest our control.

T=sample(c(rep(1,300),rep(0,sum(I)-300)))

experiment["T"]=T

# For the population dogs, let q_i be the probability that the dog gets the new food

population=data[data$I==0,]

vector=3*Weight[which(I==0)]+rnorm(100000-sum(I),5,3)

q=0.5*vector/max(vector)

T=rbern(100000-sum(I),q) 

population["T"]=T

# Now we will compute the outcomes.

Outcome=experiment$Weight+rnorm(sum(I),0,2)+0.1*experiment$T*experiment$Weight+rnorm(sum(I),0,2)

experiment=data.frame(cbind(Outcome,experiment))



Outcome=population$Weight+rnorm(100000-sum(I),0,2)+0.1*population$T*population$Weight+rnorm(100000-sum(I),0,2)

population=data.frame(cbind(Outcome,population))

library(Matching)
gen=GenMatch(T=experiment$T,X=cbind(experiment$Weight,experiment$Height))

mat=Match(T=experiment$T, X=cbind(experiment$Weight,experiment$Height), Weight.matrix=gen)

t.experiment=experiment[mat$index.treated,]
c.experiment=experiment[mat$index.control,]



pop.treated=population[population$T==1,]

library(minxent)

pop.treated.means=apply(pop.treated[,3:4],2,mean)
eta=c(1,pop.treated.means)
constr=length(eta)
G=cbind(1,t.experiment[,3:4])
G=t(G)
lambda = rep(0, constr - 1)
q1 = rep(1 / length(lambda), ncol(G))
mt <- minxent.multiple(q=q1, G=G, eta=eta, lambda=lambda)
weights=c(mt$Estimates)

library(weights)

wtd.t.test(t.experiment$Height,pop.treated$Height,weight=weights*300,samedata=FALSE)

wtd.t.test(t.experiment$Weight,pop.treated$Weight, weight=weights*300,samedata=FALSE)

# Now for the placebo tests

wtd.t.test(t.experiment$Outcome,pop.treated$Outcome, weight=weights*300,samedata=FALSE)




# Now to calculate the estimated treatment effect.

wtd.t.test(t.experiment$Outcome,c.experiment$Outcome, weight=weights*300,samedata=TRUE)

# Compare this to a normal t-test

t.test(t.experiment$Outcome,c.experiment$Outcome,paired=TRUE)

# The real treatment effect for the population can be found by taking the original 10% increase away

mean(pop.treated$Outcome-pop.treated$Outcome/1.1)












# Vitamin C vs. Extra Rest Observational Study

# So we have 1000 people. The first 100 will take orange juice. 50-200 will get extra rest. The only covariate that 
# we have data on is age.

OrangeJuice=c(rep(1,100),rep(0,900))

ExtraRest=c(rep(0,50),rep(1,150),rep(0,800))

Age=runif(1000, 15, 80)

# People who drink orange juice will tend to be younger, and people who get extra rest will tend to be older

# Orange juice will decrease the days sick by 1, and extra rest will decrease the days sick by 2. Age will 
# increase days sick.

DaysSick=rpois(1000,lambda=4)-OrangeJuice-2*ExtraRest+signif(Age, digits=1)/10

data=data.frame(cbind(DaysSick,OrangeJuice,ExtraRest,Age))

OJ=data[data$OrangeJuice==1,]

Sleepers=data[data$ExtraRest==1,]

NonSleepers=data[data$ExtraRest==0,]


X=c(OJ$Age,Sleepers$Age)

T=c(rep(1, sum(data$OrangeJuice)),rep(0, sum(data$ExtraRest)))

mat1=Match(Tr=T,X=X,estimand="ATT")


X=c(OJ$Age,NonSleepers$Age)

T=c(rep(1, sum(data$OrangeJuice)),rep(0, 1000-sum(data$ExtraRest)))

mat2=Match(Tr=T,X=X,estimand="ATT")

t=Sleepers[mat1$index.control-sum(data$OrangeJuice),]

c=NonSleepers[mat2$index.control-sum(data$OrangeJuice),]

t.test(t$DaysSick,c$DaysSick,paired=TRUE)
