# Adapt the code to generate a population of 10,000,000 from a chi-square  
# distribution with 2 degrees of freedom.  This is a heavily right 
# skewed distribution.  (Hint: rchisq()).  You will have to read up on this 
# function and probably do some trial and error.  Being able to learn new 
# functions and methods is a key skill.

n = 1.0e7 # number of data points
df = 2 # number of independent variables
ncp = 0 # non-centrality parameter
# rchisq(n, df, ncp) generates random numbers from a chi-square distribution
population = rchisq(n=n,df=df, ncp=ncp) 

# Provide a histogram of this population… display the right skewness.
hist(population)

# Record the mean and standard deviation of this population.  
mu = mean(population) # about 2.0
sigma = sd(population) # about 2.0

# According to the central limit theorem, what should be the approximate 
# distribution of sample means of size 50 from this right skewed population?  
# What should be the mean and standard error of the mean (standard deviation 
# of the distribution of sample means)?  
# Answer: the distribution should be very slim, because the number of samples 
# is so great.

n = 50 # sample size per sample for distribution
simulations = 1000 #number of samples and thus number of xbars we will generate.  
#mu = 0; # mean parameter for use with normal distribuions
#sigma = 1; # standard deviation parameter for use with normal distribuions

xbar_holder = numeric(simulations) # This will hold all the sample means for the first distribution.

for (i in 1:simulations)
{ 
  sample = rnorm(n,mean = mu, sd = sigma)
  xbar = mean(sample)
  xbar_holder[i] = xbar
}

par(mfrow = c(2,1))
# The approximate distribution of the sample mean: n = 50
hist(xbar_holder, col = "blue", main = paste("Distribution of the sample mean: 
                                             n = ", n), xlab = "Dist Sample 
     Means", xlim = c(-10,10))


# the standard error of the mean is sigma/sqrt(length(n))
se = sigma / sqrt(n)
se # 0.06322449

summary(xbar_holder) #5 number summary and the mean
sd(xbar_holder) # standard deviation of distribution

# Now let’s check this: Adapt the CLT code to draw 10,000 means each of size 50 
# from this population and provide the sampling distribution of this sample 
# mean.  Provide a histogram of these 10,000 sample means.






