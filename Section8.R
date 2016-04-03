# KS Tests

rvars=rexp(n=100, rate=1)
pdf("ExponentialECDF.pdf")
plot(ecdf(rvars), verticals=TRUE, do.points=FALSE, main="Empirical Distribution for Exponential")
dev.off()



rvars=rnorm(n=100, 3, 1)

pdf("NormalECDF.No.Title.pdf")
plot(ecdf(rvars), verticals=TRUE, do.points=FALSE, main="")
dev.off()


pdf("NormalECDF.pdf")
plot(ecdf(rvars), verticals=TRUE, do.points=FALSE, main="Empirical Distribution for Normal")
dev.off()



rvars=rpois(n=100, lambda=1)

pdf("PoissonECDFNoTitle.pdf")
plot(ecdf(rvars), verticals=TRUE, do.points=FALSE, main="")
dev.off()


pdf("PoissonECDF.pdf")
plot(ecdf(rvars), verticals=TRUE, do.points=FALSE, main="Empirical Distribution for Poisson")
dev.off()



rvars=rbinom(n=100, size=10, prob=0.5)

pdf("BinomialECDFNoTitle.pdf")
plot(ecdf(rvars), verticals=TRUE, do.points=FALSE, main="")
dev.off()


pdf("BinomialECDF.pdf")
plot(ecdf(rvars), verticals=TRUE, do.points=FALSE, main="Empirical Distribution for Binomial")
dev.off()




# One Sample KS Test example

sample=rnorm(n=100, mean=3, sd=1)

normalized.sample=(sample-mean(sample))/sd(sample)

ks.test(x=normalized.sample, y=pnorm)


# Two Sample KS Test example

sample1=rnorm(n=100, mean=3, sd=1)

sample2=rnorm(n=100, mean=3, sd=2)

ks.test(sample1, sample2)




# Here is an example of using GenMatch for the Florida voting machines example

data <- read.table(file="http://sekhon.berkeley.edu/causalinf/R/election04.raw", header=TRUE)


library(GenMatch)
gen=GenMatch(Tr=data$etouch,X=with(data,cbind(income, b2000, hispanic, size)))

Match(Y=data$b2004, Tr=data$etouch, X=with(data,cbind(income, b2000, hispanic, size)), Weight.matrix=gen)

mat=Match(Y=data$b2004, Tr=data$etouch, X=with(data,cbind(income, b2000, hispanic, size)), Weight.matrix=gen)

MatchBalance(etouch ~ income+b2000+hispanic+size, data=data, match.out=mat)
