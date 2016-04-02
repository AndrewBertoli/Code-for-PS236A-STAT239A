# Equivalence Tests

# So first make the treatment and control groups

treatment=data.frame(rnorm(200,25,1))
colnames(treatment)="Age"

control=data.frame(rnorm(200,25,1))
colnames(control)="Age"

# Now run the equivalence test for unpaired data.

tost(x=treatment$Age, y=control$Age, alpha=0.05, epsilon=0.2*sd(c(treatment$Age,control$Age)))

# If we were dealing with paired data, we would run this code

differences=treatment$Age-control$Age

tost(x=differences, alpha=0.05, epsilon=0.2*sd(c(treatment$Age,control$Age)))


# QQ plots

# Let's start by comparing two samples that came from the same uniform distribution

pdf("qqplotsamedistributionuniform.pdf")
t=runif(100,0,1) 
c=runif(100,0,1)
qqplot(t,c,xlim=c(-5,5),ylim=c(-5,5))
abline(0,1,col="red")
abline(v=0)
abline(h=0)
dev.off()



# Now let's compare two samples that came from the same normal distribution

pdf("qqplotsamedistributionnormal.pdf")
t=rnorm(100,0,1) 
c=rnorm(100,0,1)
qqplot(t,c,xlim=c(-5,5),ylim=c(-5,5))
abline(0,1,col="red")
abline(v=0)
abline(h=0)
dev.off()



# Now lets plot a graph where the treatment group has a higher mean

pdf("qqplotdifferentmeans.pdf")
t=rnorm(100,1,1) 
c=rnorm(100,0,1)
qqplot(t,c,xlim=c(-5,5),ylim=c(-5,5))
abline(0,1,col="red")
abline(v=0)
abline(h=0)
dev.off()

# Now lets plot a graph where the treatment group has a higher variance

pdf("qqplotdifferentvariances.pdf")
t=rnorm(100,0,2) 
c=rnorm(100,0,1)
qqplot(t,c,xlim=c(-5,5),ylim=c(-5,5))
abline(0,1,col="red")
abline(v=0)
abline(h=0)
dev.off()






# Boostrapping

# Example: Want to find the median of an Exponential(1) distribution. The true median is

qexp(p=0.5, rate=1)

# We will take a sample of 100. To estimate the sampling distribution, we will take 10,000 samples 
# with replacement from our original sample of 100. The size of each new sample will be 100.

original.sample=rexp(n=100, rate=1) 

boot.medians=rep(0,10000)

for(i in 1:10000){
	
new.sample=sample(original.sample, size=100, replace=TRUE)
boot.medians[i]=median(new.sample)

}

mean(boot.medians)

hist(boot.medians, main="Estimated Sampling Distribution from Bootstrapping")

# We can compare this sampling distribution to the real sampling distribution.  Since we know the data is Exp(1), 
# we can create a very accurate estimate the distribution of the median with Monte Carlo Simulation

mc.medians=rep(0,10000)

for(i in 1:10000){
	
new.sample=rexp(n=100, rate=1) 
mc.medians[i]=median(new.sample)

}

mean(mc.medians)

hist(mc.medians, main="Real Sampling Distribution of the Median")

pdf("ExpBoot.pdf")

op=par(mfrow=c(2,1))

hist(boot.medians, main="Estimated Sampling Distribution from Bootstrapping")

hist(mc.medians, main="Real Sampling Distribution of the Median")

dev.off()
