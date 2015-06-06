library(Rlab)

Initial.Stress.Level=runif(30000,0.5,1)

Initial.Walk.Hours=runif(30000,0,5)

# We will make stressful people more likely to be compliers

Complier=rbern(30000,0.1*Initial.Stress.Level) 


# Now for the givaway

Offered.Segway=sample(c(rep(0,20000),rep(1,10000)))

Segway=as.numeric(Offered.Segway==1 & Complier==1)

# Segay winners will walk between 10% and 50% less than they would have otherwise. Everyone else will maintain their 
# initial walk hours.

Walk.Hours=Initial.Walk.Hours-Segway*Initial.Walk.Hours*runif(30000, 0.1, 0.5)

# We could say that every hour of walking decreases your stress levels by 10%. You may want to try this example for yourself.

# Stress.Level=Initial.Stress.Level-0.1*Walk.Hours

#However, for now let's say that walking has no effect on stress levels

Stress.Level=Initial.Stress.Level

# Of course, we do not know who were the compliers for the people who weren't offered Segways

Complier[Offered.Segway==0]=NA 


# Our data will consist of everyone's stress levels and walk hours (after the giveaway), who was offered a Segway, and 
# those who got a segway.

data=data.frame(Stress.Level, Walk.Hours, Offered.Segway, Complier)


# So the ITT Estimator is

t.test(Stress.Level~Offered.Segway,data=data)

# The Wald Estimator is

with(data,

mean(Stress.Level[Offered.Segway==1])-mean(Stress.Level[Offered.Segway==0])
/
mean(Walk.Hours[Offered.Segway==1])-mean(Walk.Hours[Offered.Segway==0])

)




# For the Monte Carlo simulation

Estimates=rep(0,10000)

for(i in 1:10000){
	
Initial.Stress.Level=runif(30000,0.5,1)

Initial.Walk.Hours=runif(30000,0,5)

Complier=rbern(30000,0.1*Initial.Stress.Level) 

Offered.Segway=sample(c(rep(0,20000),rep(1,10000)))

Segway=as.numeric(Offered.Segway==1 & Complier==1)

Walk.Hours=Initial.Walk.Hours-Segway*Initial.Walk.Hours*runif(30000, 0.1, 0.5)

Stress.Level=Initial.Stress.Level

Complier[Offered.Segway==0]=NA 

data=data.frame(Stress.Level, Walk.Hours, Offered.Segway, Complier)



# The Wald Estimator is

Estimates[i]=with(data,

mean(Stress.Level[Offered.Segway==1])-mean(Stress.Level[Offered.Segway==0])
/
mean(Walk.Hours[Offered.Segway==1])-mean(Walk.Hours[Offered.Segway==0])

)
	
	
}

hist(Estimates, xlim=c(-3,3),main="Histogram of Wald Estimates",freq=FALSE)




