# Maximize minimum p-value (break ties by looking at the second p-value)

loss.function=function(x) {
	p.vals = x	
	return(sort(p.vals))
}


# Maximize minimum p-value

loss.function=function(x) {
	p.vals = x	
	return(min(p.vals))
}


# Maximize the average of the p-values

loss.function=function(x) {
	p.vals = x	
	return(mean(p.vals))
}




# Maximize the minimum p-value, giving more weight to the first covariate (we will treat the p-values for 
# the first covariate as half their actual value)

loss.function=function(x) {
	p.vals = x
	p.vals[1]=p.vals[1]*0.5	
	p.vals[length(p.vals)/2+1]=p.vals[length(p.vals)/2+1]*0.5	
	return(sort(p.vals))
}



# Restrict the search to just weighting schemes that give us better balance on every covariate than we had in 
# the initial dataset.



# First, call your vector of treatment assignments 'treat'.  Then find the initial p-values

initialize = function(X) {
	initial = NULL
	for(i in 1:ncol(X)) {
		initial = c(initial, t.test(X[,i][treat == 1], X[,i][treat == 0])$p.value)
	}
	# ks.test will conduct a ks.test for difference in distributions
	for(i in 1:ncol(X)) {
		initial = c(initial, ks.test(X[,i][treat == 1], X[,i][treat == 0])$p.value)
	}
	return(initial)
}

initial = initialize(BalanceMat)

# So the loss function is

loss.function = function(x) {
	p.vals = x
	if(sum(x < initial) > 0) {
		p.vals = 0.1*p.vals	
	}
	return(sort(p.vals))
}
