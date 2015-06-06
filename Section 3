# Permutation Inference: How rare was the data we got assuming that the treatment had no effect? We will first run the analysis with a treatment effect of 1.

t=rnorm(50,1,2)
c=rnorm(50,0,2)

real.t.stat=mean(t)-mean(c)

outcomes=c(t,c)

fake.t.stats=rep(0,1000)

for(i in 1:1000){
	treatmentassignment=sample(c(rep(0,50),rep(1,50)),100, replace=FALSE)
	fake.t=outcomes[treatmentassignment==1]
	fake.c=outcomes[treatmentassignment==0]
	fake.t.stats[i]=mean(fake.t)-mean(fake.c)
	}
	
pvalue=length(which(abs(fake.t.stats)>=abs(real.t.stat)))/length(fake.t.stats)

pvalue

pdf("PermHist.pdf")

hist(fake.t.stats,main="Distribution of Fake Test Statistics",col="lightblue",freq=FALSE)
abline(v=real.t.stat,col="darkgreen")
text(real.t.stat-0.07, 0.45,"Real Test Statistic",srt=90,col="darkgreen")

dev.off()



# Now run the permuation test with no treatment effect

t=rnorm(50,0,2) # No Treatment Effect this time
c=rnorm(50,0,2)

real.t.stat=mean(t)-mean(c)

outcomes=c(t,c)

fake.t.stats=rep(0,1000)

for(i in 1:1000){
	treatmentassignment=sample(c(rep(0,50),rep(1,50)),100, replace=FALSE)
	fake.t=outcomes[treatmentassignment==1]
	fake.c=outcomes[treatmentassignment==0]
	fake.t.stats[i]=mean(fake.t)-mean(fake.c)
	}
	
pvalue=length(which(abs(fake.t.stats)>=abs(real.t.stat)))/length(fake.t.stats)

pvalue

pdf("PermHistNoEffect.pdf")

hist(fake.t.stats,main="Distribution of Fake Test Statistics",col="lightblue",freq=FALSE)
abline(v=real.t.stat,col="darkgreen")
text(real.t.stat-0.07, 0.45,"Real Test Statistic",srt=90,col="darkgreen")

dev.off()


# Wilcoxon Rank Sum and Signed Rank Test

t=rnorm(50,1,2)
c=rnorm(50,0,2)

library(stats)

# If we are not dealing with matched pairs

wilcox.test(t,c)

# If we have matched pairs

wilcox.test(t, c, paired=TRUE)






# Propensity Score Matching

# Imagine that the probability that a person eats fast food is determined by their age, gender, whether their parents eat fast food, and their stress level. We observe everything but their stress level.

Age=sample(25:90,100,replace=TRUE)

Gender=sample(c(0,1),100,replace=TRUE)

Parents.Eaters=sample(c(0,1),replace=TRUE)

Stress=runif(100,0,1)

Prob.Treat=1/300*Age+0.1*Gender+0.2*Parents.Eaters+0.3*Stress

# To get the outcomes, we will need to use rbern (which gives you Bernoulli random variables for a vector of probabilities) 

install.packages("Rlab")
library(Rlab)

Treat=rbern(100,Prob.Treat)

Prob.Heart.Disease=0.3*Treat+1/400*Age+0.05*Gender+0.1*Stress

Heart.Disease=rbern(100,Prob.Heart.Disease)

# Now let's put together our data matrix. This is the data we would see in real life.

data=data.frame(cbind(Age,Gender,Parents.Eaters,Treat,Heart.Disease))


# Now we have our data. This is where the problem would start if we were doing a study about this. So first, we will estimate the probability of treatment given our covariates (Age,Gender,Parents.Eaters)

pscore=glm(Treat ~ Age + Gender + Parents.Eaters, family= binomial(link=logit),data=data)$fitted.values

data=cbind(data,pscore)

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


# In this example, our model was very close to the true model. In most real world cases, our model is not as good.



