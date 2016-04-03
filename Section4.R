require(Rlab)

# Run the code from last section many times to estimate the bias of our matching estimator

estimates=rep(0,10000)

for(j in 1:10000){

Age=sample(25:90,100,replace=TRUE)

Gender=sample(c(0,1),100,replace=TRUE)

Parents.Eaters=sample(c(0,1),replace=TRUE)

Stress=runif(100,0,1)

Prob.Treat=1/300*Age+0.1*Gender+0.2*Parents.Eaters+0.3*Stress

# To get the outcomes, we will need to use rbern (which gives you Bernoulli random variables for a vector of probabilities) 

Treat=rbern(100,Prob.Treat)

Prob.Heart.Disease=0.3*Treat+1/400*Age+0.05*Gender+0.1*Stress

Heart.Disease=rbern(100,Prob.Heart.Disease)

# Now let's put together our data matrix. This is the data we would see in real life.

data=data.frame(cbind(Age,Gender,Parents.Eaters,Treat,Heart.Disease))


# Now we have our data. This is where the problem would start if we were doing a study about this. So first, we will 
# estimate the probability of treatment given our covariates (Age,Gender,Parents.Eaters)

pscore=glm(Treat ~ Age + Gender + Parents.Eaters, family= binomial(link=logit),data=data)$fitted.values

data=cbind(data,pscore)

t=data[data$Treat==1,]
c=data[data$Treat==0,]

# For each treatment unit, we will find the closest control unit to use as a match. We will do our matching with replacement

Controls=rep(0,length(t$pscore))

for(i in 1:length(t$pscore)){

Controls[i]=which.min(abs(c$pscore-t$pscore[i]))

}

estimates[j]=t.test(t$Heart.Disease,c$Heart.Disease[Controls],paired=TRUE)$estimate

}


mean(estimates)
hist(estimates)
abline(v=0.3)






# What happens when we increase the effect of stress on eating burgers?

mean.of.estimates=rep(0,11)

for(k in seq(0,0.5, by=0.05)){

estimates=rep(0,10000)

for(j in 1:10000){

Age=sample(25:90,100,replace=TRUE)

Gender=sample(c(0,1),100,replace=TRUE)

Parents.Eaters=sample(c(0,1),replace=TRUE)

Stress=runif(100,0,1)

Prob.Treat=1/300*Age+0.1*Gender+0.2*Parents.Eaters+k*Stress

Prob.Treat[which(Prob.Treat>1)]=1


# To get the outcomes, we will need to use rbern (which gives you Bernoulli random variables for a vector of probabilities) 

Treat=rbern(100,Prob.Treat)

Prob.Heart.Disease=0.3*Treat+1/400*Age+0.05*Gender+0.2*Stress

Prob.Heart.Disease[which(Prob.Heart.Disease>1)]=1

Heart.Disease=rbern(100,Prob.Heart.Disease)

# Now let's put together our data matrix. This is the data we would see in real life.

data=data.frame(cbind(Age,Gender,Parents.Eaters,Treat,Heart.Disease))


# Now we have our data. This is where the problem would start if we were doing a study about this. So first, 
# we will estimate the probability of treatment given our covariates (Age,Gender,Parents.Eaters)

pscore=glm(Treat ~ Age + Gender + Parents.Eaters, family= binomial(link=logit),data=data)$fitted.values

data=cbind(data,pscore)

t=data[data$Treat==1,]
c=data[data$Treat==0,]

# For each treatment unit, we will find the closest control unit to use as a match. We will do our matching with replacement

Controls=rep(0,length(t$pscore))

for(i in 1:length(t$pscore)){

Controls[i]=which.min(abs(c$pscore-t$pscore[i]))

}

estimates[j]=t.test(t$Heart.Disease,c$Heart.Disease[Controls],paired=TRUE)$estimate

}


mean.of.estimates[1+20*k]=mean(estimates)

}



pdf("FastFood.pdf")

plot(seq(0,0.5, by=0.05),mean.of.estimates,ylim=c(0.25,0.35),xlab="Importance of Stress",ylab="Expectation of Estimate",
main="Bias Increases as Stress Makes Eating Fast Food More Likely")
abline(h=0.3,col="blue")
text(0.4, 0.304,"Parameter",col="blue")

dev.off()


# What happens when the effect of stress on heart disease increases.

# What happens when we increase the effect of stress on eating burgers?

mean.of.estimates=rep(0,11)

for(k in seq(0,0.5, by=0.05)){

estimates=rep(0,10000)

for(j in 1:10000){

Age=sample(25:90,100,replace=TRUE)

Gender=sample(c(0,1),100,replace=TRUE)

Parents.Eaters=sample(c(0,1),replace=TRUE)

Stress=runif(100,0,1)

Prob.Treat=1/300*Age+0.1*Gender+0.2*Parents.Eaters+0.2*Stress

Prob.Treat[which(Prob.Treat>1)]=1


# To get the outcomes, we will need to use rbern (which gives you Bernoulli random variables for a vector of probabilities) 

Treat=rbern(100,Prob.Treat)

Prob.Heart.Disease=0.3*Treat+1/400*Age+0.05*Gender+k*Stress

Prob.Heart.Disease[which(Prob.Heart.Disease>1)]=1

Heart.Disease=rbern(100,Prob.Heart.Disease)

# Now let's put together our data matrix. This is the data we would see in real life.

data=data.frame(cbind(Age,Gender,Parents.Eaters,Treat,Heart.Disease))


# Now we have our data. This is where the problem would start if we were doing a study about this. So first, we will 
# estimate the probability of treatment given our covariates (Age,Gender,Parents.Eaters)

pscore=glm(Treat ~ Age + Gender + Parents.Eaters, family= binomial(link=logit),data=data)$fitted.values

data=cbind(data,pscore)

t=data[data$Treat==1,]
c=data[data$Treat==0,]

# For each treatment unit, we will find the closest control unit to use as a match. We will do our matching with replacement

Controls=rep(0,length(t$pscore))

for(i in 1:length(t$pscore)){

Controls[i]=which.min(abs(c$pscore-t$pscore[i]))

}

estimates[j]=t.test(t$Heart.Disease,c$Heart.Disease[Controls],paired=TRUE)$estimate

}


mean.of.estimates[1+20*k]=mean(estimates)

}

pdf("FastFood2.pdf")

plot(seq(0,0.5, by=0.05),mean.of.estimates,ylim=c(0.25,0.35),xlab="Importance of Stress",ylab="Expectation of Estimate",
main="Bias Increases as Stress Makes Heart Disease More Likely")
abline(h=0.3,col="blue")
text(0.4, 0.304,"Parameter",col="blue")

dev.off()







# If there is no treatment effect, can we manipulate are p-value by p-hacking

Age=sample(25:90,100,replace=TRUE)

Gender=sample(c(0,1),100,replace=TRUE)

Parents.Eaters=sample(c(0,1),replace=TRUE)

Stress=runif(100,0,1)

Prob.Treat=1/300*Age+0.1*Gender+0.2*Parents.Eaters+0.3*Stress

# To get the outcomes, we will need to use rbern (which gives you Bernoulli random variables for a vector of probabilities) 

Treat=rbern(100,Prob.Treat)

Prob.Heart.Disease=0.3*Treat+1/400*Age+0.05*Gender+0.1*Stress

Heart.Disease=rbern(100,Prob.Heart.Disease)

# Now let's put together our data matrix. This is the data we would see in real life.

data=data.frame(cbind(Age,Gender,Parents.Eaters,Treat,Heart.Disease))


# Now we have our data. This is where the problem would start if we were doing a study about this. So first, we 
# will estimate the probability of treatment given our covariates (Age,Gender,Parents.Eaters)






t=data[data$Treat==1,]
c=data[data$Treat==0,]


# Here is the p-value for no matching. 

No.Matching.p.value=t.test(t$Heart.Disease,c$Heart.Disease)$p.value

No.Matching.p.value



# Now let's see the p-values when we match on different things. First lets try just Age.

pscore=glm(Treat ~ Age, family= binomial(link=logit),data=data)$fitted.values

data.p=cbind(data,pscore)

t=data.p[data.p$Treat==1,]
c=data.p[data.p$Treat==0,]

Controls=rep(0,length(t$pscore))

for(i in 1:length(t$pscore)){

Controls[i]=which.min(abs(c$pscore-t$pscore[i]))

}

Age.p.value=t.test(t$Heart.Disease,c$Heart.Disease[Controls],paired=TRUE)$p.value

Age.p.value


# Now let's do just Gender

pscore=glm(Treat ~ Gender, family= binomial(link=logit),data=data)$fitted.values

data.p=cbind(data,pscore)

t=data.p[data.p$Treat==1,]
c=data.p[data.p$Treat==0,]

Controls=rep(0,length(t$pscore))

for(i in 1:length(t$pscore)){

Controls[i]=which.min(abs(c$pscore-t$pscore[i]))

}

Gender.p.value=t.test(t$Heart.Disease,c$Heart.Disease[Controls],paired=TRUE)$p.value

Gender.p.value




# Now let's do Parents.Eaters

pscore=glm(Treat ~ Parents.Eaters, family= binomial(link=logit),data=data)$fitted.values

data.p=cbind(data,pscore)

t=data.p[data.p$Treat==1,]
c=data.p[data.p$Treat==0,]

Controls=rep(0,length(t$pscore))

for(i in 1:length(t$pscore)){

Controls[i]=which.min(abs(c$pscore-t$pscore[i]))

}

Parents.Eaters.p.value=t.test(t$Heart.Disease,c$Heart.Disease[Controls],paired=TRUE)$p.value

Parents.Eaters.p.value




# Now let's do Age and Gender

pscore=glm(Treat ~ Age + Gender, family= binomial(link=logit),data=data)$fitted.values

data.p=cbind(data,pscore)

t=data.p[data.p$Treat==1,]
c=data.p[data.p$Treat==0,]

Controls=rep(0,length(t$pscore))

for(i in 1:length(t$pscore)){

Controls[i]=which.min(abs(c$pscore-t$pscore[i]))

}

Age.Gender.p.value=t.test(t$Heart.Disease,c$Heart.Disease[Controls],paired=TRUE)$p.value

Age.Gender.p.value




# Now let's do Age and Parents.Eaters

pscore=glm(Treat ~ Age + Parents.Eaters, family= binomial(link=logit),data=data)$fitted.values

data.p=cbind(data,pscore)

t=data.p[data.p$Treat==1,]
c=data.p[data.p$Treat==0,]

Controls=rep(0,length(t$pscore))

for(i in 1:length(t$pscore)){

Controls[i]=which.min(abs(c$pscore-t$pscore[i]))

}

Age.Parents.Eaters.p.value=t.test(t$Heart.Disease,c$Heart.Disease[Controls],paired=TRUE)$p.value

Age.Parents.Eaters.p.value





# Now let's do Gender and Parents.Eaters

pscore=glm(Treat ~ Gender + Parents.Eaters, family= binomial(link=logit),data=data)$fitted.values

data.p=cbind(data,pscore)

t=data.p[data.p$Treat==1,]
c=data.p[data.p$Treat==0,]

Controls=rep(0,length(t$pscore))

for(i in 1:length(t$pscore)){

Controls[i]=which.min(abs(c$pscore-t$pscore[i]))

}

Gender.Parents.Eaters.p.value=t.test(t$Heart.Disease,c$Heart.Disease[Controls],paired=TRUE)$p.value

Gender.Parents.Eaters.p.value






# Now let's do Age, Gender, and Parents.Eaters

pscore=glm(Treat ~ Age + Gender + Parents.Eaters, family= binomial(link=logit),data=data)$fitted.values

data.p=cbind(data,pscore)

t=data.p[data.p$Treat==1,]
c=data.p[data.p$Treat==0,]

Controls=rep(0,length(t$pscore))

for(i in 1:length(t$pscore)){

Controls[i]=which.min(abs(c$pscore-t$pscore[i]))

}

All.p.value=t.test(t$Heart.Disease,c$Heart.Disease[Controls],paired=TRUE)$p.value

All.p.value










# For a positive use of matching, let's look at the Florida example from Jas's website

library(Matching)
options(width=150)

#use this if we have already downloaded the data
#dta <- read.table(file="election04.raw", header=TRUE)

#get the data from the web
dta <- read.table(file="http://sekhon.berkeley.edu/causalinf/R/election04.raw",
                  header=TRUE)

#
# For codebook see http://sekhon.berkeley.edu/causalinf/R/codebook_election04.raw.txt
#

cbind(as.data.frame(dta$county), dta$etouch)

Y  <- dta$b04pc
Tr <- dta$etouch


glm2 <- glm(b04pc ~ etouch + b00pc + b00pc_sq + d96pc1 + v_change +
            income + hispanic +  b00pc_e + b00pcsq_e, data=dta)
summary(glm2)


rr <- Match(Y=Y, Tr=Tr, X=cbind(dta$b00pc, dta$b00pc_sq, dta$d96pc1, dta$v_change, dta$income, dta$hispanic, dta$b00pc_e,
dta$b00pcsq_e), estimand="ATT")
summary(rr)

# So what this shows is that regression finds a significant effect because it is extrapolating from uninformative points. 
# Matching shows that when you compare the counties with e-touch to similar counties without e-touch, there is no 
# discernable effect

