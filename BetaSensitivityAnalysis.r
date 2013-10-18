# This script is a prior sensitivity analysis for a Bernoulli|Beta example.
# We take random variables {X_i}\in {0,1}, with 1 indicating 'success' and 
# 0 indicating 'failure.'
#
# Prior assumptions:  We know that the probability p of success must be in [0,1],
# and for external reasons we assume it is likely that p<1/2.  We use a 
# beta distribution.  Recall that such distributions are parametrized by a,b>0. 
# We choose b>a to give the prior distribution the desired skew-left property.
#
# Assume during a certain trial, we have y=3 successes out of n=8 attempts.
#   
#
#
# Use the following set of (a,b) for the sensitivity analysis:
#
# a=1, b=1    (Uniform prior)
# a=.5,b=1
# a=1, b=2
# a=1, b=4
# a=10, b=20
# a=10, b=5   # Skew-right example just for fun


# The following script plots the prior distributions as dotted lines:

x<-seq(0,1,length=100)

a=c(1,.5,1,1,10,10)
b=c(1,1,2,4,20,5)

colors<-c("black","red","green","orange","blue","purple")
labels<-c("(a,b)=(1,1)","(a,b)=(.5,1)","(a,b)=(1,2)","(a,b)=(1,4)",
	"(a,b)=(10,20)","(a,b)=(10,5)")

plot(x,dbeta(x,shape1=a[1],shape2=b[1]),type="l",lty=3,xlab="p-value",ylab="Density",
	xlim=c(0,1),ylim=c(0,6), main="Sensitivity Analysis of Beta Distribution")

mtext("(Dotted lines are priors, continuous lines are corresponding posteriors with y=3, n=8)",side=3)

for (i in 2:6){
	lines(x,dbeta(x,shape1=a[i],shape2=b[i]),lty=3,col=colors[i])
}

legend("topright",title="Prior Parameters",labels,lty=c(3,3,3,3,3,3),col=colors)

# We now compute the posterior distributions.  For the Bernoulli|Beta example,
# one has closed forms for the resulting parameters (a2,b2):
#
#		a2 = a+y =a+3
#		b2 = b+n-y = b+5
#

a2<-a+3
b2<-b+5

# Now plot the posterior distributions as solid lines:

for (i in 1:6){
	lines(x,dbeta(x,shape1=a2[i],shape2=b2[i]),lty=1,col=colors[i])
}

# The Maximum liklihood estimator is p=3/8=.375, so we add that in to the plot:

abline(v=.375,lty=2,col="magenta")
text(locator(1),"<--- MLE =3/8=.375",pos=4,col="magenta",cex=.75)







