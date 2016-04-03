# Section 1 Code-PS236A/STAT239A-Andrew Bertoli

# Example 1: What is the chance of seeing 753-4219 in a well-shuffled deck of cards?

# Warning! This code takes a while to run, so I recommend looking at it but not actually running it

Success.Vector=rep(0,100000000) # This will be a vector of that codes successes as 1 and failures as 0

for(i in 1:100000000){

deck=sample(c(rep(2:14,4), 52, replace=FALSE) 

# We are labeling Jack=11, Queen=12's, King=13, and Ace=14. This line of code produces a well shuffled deck of cards (without suits).

seven=which(deck==7)
five=which(deck[seven+1]==5)
three=which(deck[five+1]==3)

four=which(deck[three+1]==4)
two=which(deck[four+1]==2)
one=which(deck[two+1]==1)
nine=which(deck[one+1]==9)

if(length(nine)==0){Success.Vector[i]=0}
if(length(nine)>0){Success.Vector[i]=1}

}

sum(Success.Vector)

# Answer is about 5 in 100,000,000





# Example 2: Monte Hall Problem

Success.For.Switching.Doors=rep(0,1000)

for(i in 1:1000){

Doors=1:3
Prize.Door=sample(Doors,1) # Door where the prize is hidden
First.Choice=sample(Doors,1) # Door you pick first
if(First.Choice != Prize.Door){Open.Door=Doors[-c(Prize.Door,First.Choice)]}
if(First.Choice == Prize.Door){Open.Door=sample(Doors[-First.Choice],1)}
Doors.Remaining=Doors[-Open.Door]
isTRUE(Prize.Door==First.Choice)
isTRUE(Prize.Door!=First.Choice)

if(Prize.Door==First.Choice)Success.For.Switching.Doors[i]=0
if(Prize.Door!=First.Choice)Success.For.Switching.Doors[i]=1

}

sum(Success.For.Switching.Doors)/length(Success.For.Switching.Doors)




# Example 3: What is the integral of log(x^2/(x+1)^3)*sin(x^5)/arctan(1/x^2) evaluated from 0 to 1?

curve(abs(log(x^2/(x+1)^3)*sin(x^5)/atan(1/x^2)),from=1,to=2) # View the graph


# We will use hit-or-miss Monte Carlo simulation

Xs=runif(10000,1,2)
Ys=runif(10000,0,10)

Area.Rectangle=1*10

Function.Values=abs(log(Xs^2/(Xs+1)^3)*sin(Xs^5)/atan(1/Xs^2))

hit.rate=length(which(Ys<Function.Values))/length(Function.Values)

integral=hit.rate*Area.Rectangle

integral


# Example 4: We want to take 10,000 draws of a random variable with the density function f(x)=cos(x) for 0 < x < pi/2

curve(cos(x),from=0,to=pi/2) # View the graph

Xs=runif(50,0,pi/2)
Ys=runif(50,0,1)

Function.Values=cos(Xs)

hits=which(Ys<Function.Values)

draws=Xs[hits]

draws


# Example 5: How rare was the data we got assuming that the treatment had no effect?

t=rnorm(50,1,3)
c=rnorm(50,0,3)

real.t.stat=mean(t)-mean(c)

fake.t.stats=rep(0,1000)

for(i in 1:1000){
	treatmentassignment=sample(c(rep(0,50),rep(1,50)),100, replace=FALSE)
	outcomes=c(t,c)
	fake.t=outcomes[treatmentassignment==1]
	fake.c=outcomes[treatmentassignment==0]
	fake.t.stats[i]=mean(fake.t)-mean(fake.c)
	}
	
pvalue=length(which(abs(fake.t.stats)>=real.t.stat))/length(fake.t.stats)

pvalue

# Compare this p-value to pvalue of a normal t-test.

t.test(t,c)	
