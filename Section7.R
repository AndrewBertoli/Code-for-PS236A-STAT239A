# Section 7: Matching II

library(Rlab)
library(ICSNP)

Person=c("Kobe Bryant",
"Michelle Obama",
"Einstein",
"Billy")

Age=c(35, 49, 56, 6)

Height=c(6*12+6, 5*12+11, 5*12+8, 42)

data=data.frame(Age, Height)

rownames(data)=Person

S=var(data)

S

K.M=as.numeric(data["Kobe",]-data["Michelle",])

MD.Kobe.Michelle=sqrt(t(K.M)%*%solve(S)%*%(K.M))


K.H=as.numeric(data["Kobe",]-data["Einstein",])

MD.Kobe.Einstein=sqrt(t(K.H)%*%solve(S)%*%(K.H))


K.B.=as.numeric(data["Kobe",]-data["Billy",])

MD.Kobe.Billy=sqrt(t(K.B.)%*%solve(S)%*%(K.B.))


M.H.=as.numeric(data["Michelle",]-data["Einstein",])

MD.Michelle.Einstein=sqrt(t(M.H.)%*%solve(S)%*%(M.H.))


M.B.=as.numeric(data["Michelle",]-data["Billy",])

MD.Michelle.Billy=sqrt(t(M.B.)%*%solve(S)%*%(M.B.))


H.B.=as.numeric(data["Einstein",]-data["Billy",])

MD.Einstein.Billy=sqrt(t(H.B.)%*%solve(S)%*%(H.B.))


MD.Kobe.Michelle

MD.Kobe.Einstein

MD.Kobe.Billy

MD.Michelle.Einstein

MD.Michelle.Billy

MD.Einstein.Billy




# Here is a faster function for getting the Mahalanobis's distances

fastPwMahal = function(data) {

    # Calculate inverse square root matrix
    invCov = solve(cov(data))
    svds = svd(invCov)
    invCovSqr = svds$u %*% diag(sqrt(svds$d)) %*% t(svds$u)

    Q = data %*% invCovSqr

    # Calculate distances
    # pair.diff() calculates the n(n-1)/2 element-by-element
    # pairwise differences between each row of the input matrix
    sqrDiffs = pair.diff(Q)^2
    distVec = rowSums(sqrDiffs)

    # Create dist object without creating a n x n matrix
    attr(distVec, "Size") = nrow(data)
    attr(distVec, "Diag") = F
    attr(distVec, "Upper") = F
    class(distVec) = "dist"
    return(distVec)
}

fastPwMahal(as.matrix(data))
















# Recall this Example from Propensity Score Matching

# Imagine that the probability that a person eats fast food is determined by their age, gender, whether their 
# parents eat fast food, and their stress level. We observe everything but their stress level.

Age=sample(25:90,100,replace=TRUE)

Gender=sample(c(0,1),100,replace=TRUE)

Parents.Eaters=sample(c(0,1),100,replace=TRUE)

Stress=runif(100,0,1)

Prob.Treat=1/300*Age+0.1*Gender+0.2*Parents.Eaters+0.3*Stress

# To get the outcomes, we will need to use rbern (which gives you Bernoulli random variables for a vector of probabilities) 

Treat=rbern(100,Prob.Treat)

Prob.Heart.Disease=0.3*Treat+1/400*Age+0.05*Gender+0.1*Stress

Heart.Disease=rbern(100,Prob.Heart.Disease)

# Now let's put together our data matrix. This is the data we would see in real life.

data=data.frame(cbind(Age,Gender,Parents.Eaters,Treat,Heart.Disease))

# Now we have our data. This is where the problem would start if we were doing a study about this. So first, 
# we will estimate the probability of treatment given our covariates (Age,Gender,Parents.Eaters).  If we have the 
# right model of propensity scores, everything is fine.

pscore=glm(Treat ~ Age + Gender + Parents.Eaters, family= binomial(link=logit),data=data)$fitted.values

data=cbind(data[,1:5],pscore)

t=data[data$Treat==1,]
c=data[data$Treat==0,]

# Now we can see the pscores for the treatment and control group.

t$pscore



# For each treatment unit, we will find the closest control unit to use as a match. We will do our matching with replacement

Controls=rep(0,length(t$pscore))

for(i in 1:length(t$pscore)){

Controls[i]=which.min(abs(c$pscore-t$pscore[i]))

}


# Now we will do a t.test using the treated units and the controls that we matched them to.

t.test(t$Heart.Disease,c$Heart.Disease[Controls],paired=TRUE)

# We can also test for balance on the covariates

t.test(t$Age,c$Age[Controls],paired=TRUE)

t.test(t$Gender,c$Gender[Controls],paired=TRUE)

t.test(t$Parents.Eaters,c$Parents.Eaters[Controls],paired=TRUE)





# Now what happens if we have the wrong model of the propensity score.

pscore=glm(Treat ~ Age + Gender + Parents.Eaters + Age*Gender + Age*Parents.Eaters + Gender*Parents.Eaters, 
family= binomial(link=logit),data=data)$fitted.values

data=cbind(data[,1:5],pscore)

t=data[data$Treat==1,]
c=data[data$Treat==0,]

# Now we can see the pscores for the treatment and control group.

t$pscore



# For each treatment unit, we will find the closest control unit to use as a match. We will do our matching with replacement

Controls=rep(0,length(t$pscore))

for(i in 1:length(t$pscore)){

Controls[i]=which.min(abs(c$pscore-t$pscore[i]))

}


# Now we will do a t.test using the treated units and the controls that we matched them to.

t.test(t$Heart.Disease,c$Heart.Disease[Controls],paired=TRUE)

# We can also test for balance on the covariates

t.test(t$Age,c$Age[Controls],paired=TRUE)

t.test(t$Gender,c$Gender[Controls],paired=TRUE)

t.test(t$Parents.Eaters,c$Parents.Eaters[Controls],paired=TRUE)



# What happens when there is noise in our controls

mean.of.estimates=rep(0,11)

for(k in seq(0,0.5, by=0.05)){

estimates=rep(0,10000)

for(j in 1:10000){

Age=sample(25:90,100,replace=TRUE)+rnorm(100,0,k*20)

Gender=sample(c(0,1),100,replace=TRUE)+rnorm(100,0,k*20)

Parents.Eaters=sample(c(0,1),100,replace=TRUE)+rnorm(100,0,k*20)

Stress=runif(100,0,1)

Prob.Treat=1/300*Age+0.1*Gender+0.2*Parents.Eaters

Prob.Treat[which(Prob.Treat>1)]=1

Prob.Treat[which(Prob.Treat<0)]=0

# To get the outcomes, we will need to use rbern (which gives you Bernoulli random variables for a vector of probabilities) 

Treat=rbern(100,Prob.Treat)

Prob.Heart.Disease=0.3*Treat+1/400*Age+0.05*Gender

Prob.Heart.Disease[which(Prob.Heart.Disease>1)]=1

Prob.Heart.Disease[which(Prob.Heart.Disease<0)]=0

Heart.Disease=rbern(100,Prob.Heart.Disease)

# Now let's put together our data matrix. This is the data we would see in real life.

data=data.frame(cbind(Age,Gender,Parents.Eaters,Treat,Heart.Disease))


# Now we have our data. This is where the problem would start if we were doing a study about this. 
# So first, we will estimate the probability of treatment given our covariates (Age,Gender,Parents.Eaters)

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

pdf("FastFoodAttenuationBias.pdf")

plot(seq(0,0.5, by=0.05),mean.of.estimates,ylim=c(0.25,0.35),xlab="Noise in Control",ylab="Expectation of Estimate",
main="Bias Increases as Controls Become Noiser")
abline(h=0.3,col="blue")
text(0.4, 0.304,"Parameter",col="blue")

dev.off()

