# The following function will compute the p-values for different Gamma levels. The argument "differences" takes a vector 
# of the differences for the matched pairs in your data. "Gamma" is the Gamma level. In general, setting Gamma=k will 
# return the same p-value as Gamma=1/k. When "type" is set at "mean", the function will do a permutation test with the mean 
# as the test statistic. When "type" is set at "rank", the function will do the Wilcoxon Signed-Rank Test, which is less 
# senstive to outliers. The p-values are computed using Monte Carlo simulation, so the function will take a few seconds to 
# run, and you will get slightly different answers every time. Remember to interpret the test results correctly. In the 
# example where we tested if the going to the beach had a benefial effect on health, if the results held for Gamma=4, 
# the correct interpretation would be, "The results would hold if within each pair the healthier person was four times 
# as likely to go to the beach as the less healthy person."

sens=function(differences,Gamma,type){

differences=differences[differences!=0]	

if(type=="mean"){
real.t.stat=mean(differences)
fake.t.stats=rep(0,100000)
for(i in 1:100000){
treatment.assignment.t.more.agg=sample(c(1,-1),size=sum(differences>0),replace=TRUE,prob=c(Gamma/(Gamma+1),1/(Gamma+1)))	
treatment.assignment.c.more.agg=sample(c(1,-1),size=sum(differences<0),replace=TRUE,prob=c(1/(Gamma+1),Gamma/(Gamma+1)))	
first.group=differences[differences>0]*treatment.assignment.t.more.agg
second.group=differences[differences<0]*treatment.assignment.c.more.agg
new.differences=c(first.group,second.group)
fake.t.stats[i]=mean(new.differences)
}
p.val=length(which(abs(fake.t.stats)>=abs(real.t.stat)))/length(fake.t.stats)
return(p.val)}

if(type=="rank"){
ranks=rank(abs(differences))
real.t.stat=sum(ranks[differences>0])-sum(ranks)/2	
fake.t.stats=rep(0,100000)
for(i in 1:100000){
treatment.assignment.t.more.agg=sample(c(1,-1),size=sum(differences>0),replace=TRUE,prob=c(Gamma/(Gamma+1),1/(Gamma+1)))	
treatment.assignment.c.more.agg=sample(c(1,-1),size=sum(differences<0),replace=TRUE,prob=c(1/(Gamma+1),Gamma/(Gamma+1)))	
first.group=differences[differences>0]*treatment.assignment.t.more.agg
second.group=differences[differences<0]*treatment.assignment.c.more.agg
new.differences=c(first.group,second.group)
new.ranks=rank(abs(new.differences))
fake.t.stats[i]=sum(new.ranks[new.differences>0])-sum(new.ranks)/2
}
p.val=length(which(abs(fake.t.stats)>=abs(real.t.stat)))/length(fake.t.stats)
return(p.val)}

}
