# Duke University Synthetic Data Project
# Differential private algorithm using beta, binomial, and Laplace probabilities

# Authors:  Felipe Barrientos, Tom Balmat
# Version:  5/14/2017

# Copyright 2017 Duke University, Durham, North Carolina

# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
# associated documentation files (the "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the
# following conditions:

# The above copyright notice and this permission notice shall be included in all copies or substantial
# portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
# LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
# WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

# import Laplace distribution functions
sys.source("LaplaceDistribution.r", env=environment())

#####################################################################################
#### DP algorithm
#####################################################################################

DP.threshold <- function (S, M, epsilon, alpha, nIter=250000) {

  ### add random Laplace noise to S
  ### note that this converts S to a real number
  ### since the Laplace distribution (double exponential) is symmetric about 0,
  ### nS maybe greater than or less than S
  ### also, nS can be negative or greater than M
  nS <- S + rlaplace(1, 0, 1/epsilon)

  ### Range of S
  RangeS <- 0:M

  ### Initial value of p
  ### shape parameters of (1, 1) correspond to the uniform(0, 1) distribution
  ### use supplied initial shape parameters
  ### p = rbeta(1, alpha[1], alpha[2])
  ### note that E[rbeta(n, shape1, shape2)] = shape1/(shape1+shape2)
  ### use of rbeta(1, S, M-S) would give a theoretical mean p of S/M,
  ### but S=0 requires rbeta() to return 0, in order to satisfy a mean value of 0.00
  ### once a supplied or randomly generated value of S becomes 0, all subsequent
  ### p and S values are 0
  ### similarly, S=50 requires rbeta() mean of 1.00, once supplied or generated S=50,
  ### all subsequent S values are 50
  ### adding initial beta parameters prevents expected value numerators of zero and
  ### gives mean of (S+alpha1)/(S+alpha1+alpha2) for S=M and should be unequal to 1.00
  p <- rbeta(1, S+alpha[1], M-S+alpha[2])

  ### generate Laplace density (using random nS) for each centrality value in 0:M
  dslap <- dlaplace(RangeS, nS, 1/epsilon)

  ### Gibbs interaction
  f.iter <- function(j) {
    # update S
    # product of binomial mass (using curent p) and Laplace density for each 0:M
    prob <- dbinom(RangeS, M, prob=p)*dslap
    # randomly select one S in 0:M
    # selection weights are probability products for each 0:M
    S <- sample(RangeS, 1, prob=prob)
    # update p
    # note that E(p) = (shape par 1)/(shape par 1 + shape par 2)
    # = (alpha[1]+S)/(alpha[1]+S + alpha[2]+M-S)
    # = (alpha[1]+S)/(alpha[1]+alpha[2]+M)
    # variance = ab/((a+b)^2*(a+b+1)), where a=shape par 1, b=shape par 2
    p <<- rbeta(1, S+alpha[1], M-S+alpha[2])
  }
  
  ### iterate MCMC and return all but first 1,000 generated values
  if(nIter<10000)
    nIter <- 10000
  sapply(1:nIter, f.iter)[1001:nIter]

}


