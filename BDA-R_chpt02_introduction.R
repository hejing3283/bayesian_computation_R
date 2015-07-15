install.packages("LearnBayes")

require(LearnBayes)
###----------- Propotion of heavy sleepers 
## Prior g(p)
## likelihood L(p) -- p^s (1-p)^f 
## Posterior g(p|data) -- g(p) * L(p)

## Prior 1: discrete
##---------------------------------
p = c(.05, .15, .25, .35, .45, .55, .65, .75, .85, .95 )
prior = c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0 )
prior =  prior / sum(prior)
plot(p, prior, type = 'h', ylab = 'prior probability')

data = c(11, 16) ## 11 sucess, 16 fail
#pdisc Posterior distribution for a proportion with discrete priors
post = pdisc( p, prior, data)

round( cbind( p, prior, post), 2)
require(lattice)
PRIOR = data.frame("prior", p, prior)
POST = data.frame("posterior", p, post)
names(PRIOR) = c( "Type", "P", "Probability")
names(POST )= c( "Type", "P", "Probability")
data = rbind( PRIOR, POST)
xyplot ( Probability ~ P | Type, data = data, layout = c(1,2), type = 'h', lwd = 3, col = 'black')


#### Prior 2: Beta Prior 
##---------------------------------
# given of quantile of beta function, find the parameters 
quantile2 = list(p = 0.9, x = 0.5)
quantile1 = list( p = 0.5, x = 0.3)
temp = beta.select(quantile1, quantile2)
a = temp[1]; 
b = temp[2]
rm(temp)
s = 11; f= 16
curve( dbeta( x, a+ s, b + f), from = 0, to = 1, xlab = 'p', ylab = 'densty', lty = 1, lwd = 4)
curve( dbeta( x, s+ 1, f+ 1), add = T, lty = 2, lwd = 4)
curve( dbeta( x, a, b), add = T, lty = 3, lwd = 4)
legend( .7, 4, c("Prior","Likelihood", "Posterior"), lty = c(3,2,1), lwd = c(3,3,3))

## summarize posterior using R function
## beta cdf , pbeta:  P( p >= -.5|data)
## inversee cdf qbeta
1 - pbeta( 0.5, a + s, b+f)
## confidence interval
qbeta(c(0.05, 0.95), a+s, b+f)

## summerize posterir using simulation
ps = rbeta( 1000, a+s, b+f)
hist(ps, xlab = 'p', main = "")
sum(ps >=0.5) / 1000
quantile( ps, c(0.05, 0.95))

#### Prior 3: histogram Prior 
##---------------------------------

## chosse a grid of p 
midpt = seq(0.05, 095, by = 0.1)
prior = c( 1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0 )
prior = prior / sum(prior)

curve( histprior( x, midpt, prior), from = 0, to = 1, ylab = 'Priory density ', ylim = c(0, .3))

curve( histprior( x, midpt, prior) * dbeta( x, s+1, f+1), from = 0, to = 1, ylab = 'posterior probability')

## 
p = seq( 0, 1, length = 500) ## equalily seprated p,
post = histprior(p, midpt, prior) * dbeta( p, s+1, f+1) ## posterior
post = post / sum(post)  ## normalization 
ps = sample( p, replace = T, prob = post)  ## simulated draws
hist( ps, xlab = 'p', main = "")

## Predcition 
## ----------------------------
## pdiscp: predictive probabilities when p is discrete distribution
p = seq( 0.05, 0.95, by  = .1 )
prior = c( 1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0) 
prior = prior / sum(prior)
m = 20; ys = 0:20
pred = pdiscp( p, prior, m ,ys )
round (cbind( 0:20, pred), 3)

### pbetap : predictive probablities using the beta density
ab = c(3.26, 7.19)
m = 20, ys = 0:20
pred = pbetap( ab, m, ys)

### compute predictive density using simulation
p = rbeta( 1000, 3.26, 7.19) ## simulate prior probabiliyt
y  = rbinom ( 1000, 20, p)  ## simulate new y 
freq = table( y)
ys = as.integer ( names( freq ))
predprob = freq / sum(freq)
plot( ys, predprob, type = 'h', xlab = 'y', ylab = 'predictie probability')
### output confidence interval: discint 
dist = cbind( ys, predprob)
covprob = .9
discint(dist, covprob)

#sample 1 2  exercises problem
sample(1:6,2)
# 2, 4

### sumarize R function


### the END



