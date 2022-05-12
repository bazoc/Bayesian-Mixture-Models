df = read.csv('C:/Users/Barry/Downloads/nestsize.csv', header = F)
x = df[,1]
set.seed(81196)  # So that results are reproducible


## Initialize the parameters
n = length(x)                   #Get the data
w = 1/2                         #Assign equal weight to each component to start with
cc[x==0] = sample(1:2,sum(x==0),replace=T,prob=c(.5,.5))
lambda = mean(x)                #Initialise rate at mean of data 
KK = 2                          #2 models in the mix

# Priors
aa  = rep(1,KK)  # Uniform prior on w
lambda0 = 1      # Exponential prior with rate = 1 on lambda
dd  = 2
qq  = 1

# Number of samples
rrr   = 6000
burn  = 1000


# Storing the samples
cc.out    = array(0, dim=c(rrr, n))
w.out     = rep(0, rrr)
lambda.out    = rep(0,rrr)

# MCMC iterations
for(s in 1:rrr){

  # Sample the indicators
  v = array(0,dim = c(n,KK))
  cc = rep(0,n)
  v[,1] = (x == 0)*w                                 #Compute the weights of point mass at zero
  v[,2] = (1-w)*dpois(x,lambda,log=F)               #Compute the weights for poisson
  v = v/apply(v,1,sum)
  for(i in 1:n) {
    cc[i] = sample(1:KK,size =1, replace=TRUE, prob=v[i,])
  }
  
  # Sample the weights
  w = rbeta(1, aa[1] + sum(cc==1), aa[2] + sum(cc==2))
  
  # Sample the rate
  n2 = sum(cc==2)
  xsum2 = sum(x[cc==2])
  alpha = xsum2+1
  beta = n2+1
  lambda = rgamma(1, alpha, beta)

  # Store samples
  cc.out[s,]   = cc
  w.out[s]     = w
  lambda.out[s] = lambda
}


mean(lambda.out)
mean(w.out)
