# Synthetic Data Project
# Laplace probability functions

# Author:
# Version:  5/14/2017

# Copyright 2017

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

# density
dlaplace <- function(x, m, lambda, log=F)
  # evaluate Laplace density (double-sided exponential)
  # f(x, m, lambda) =
  # x<m:   lambda/2 * exp[-lambda*(m-x)]
  # x>=m:  lambda/2 * exp[-lambda*(x-m)]
  # note that, by custom, 1/lambda is used in the pdf definition
  if(!log) {
    exp(-abs(x-m)/lambda)/2/lambda
  } else {
    -log(2*lambda)-abs(x-m)/lambda
  }

# cumulative density
plaplace <- function(x, m, lambda) {
  # evaluate Laplace cumulative density (double-sided exponential)
  # F(x, m, lambda) =
  # x<m:   [1-Fexp(m-x, lambda)]/2 = (1-1+exp[-lambda*(m-x)])/2 = exp[-lambda*(m-x)]/2
  # x>=m:  1/2+Fexp(m-x, lambda)/2 = 1/2+(1-exp[-lambda*(x-m)])/2
  #        = 1/2*(1+1-exp[-lambda*(x-m)]) = (2-exp[-lambda*(x-m)])/2
  # note that, by custom, 1/lambda is used in the pdf definition
  i <- ifelse(x<m, 1, -1)
  ((1-i) + i*exp(i/lambda*(x-m)))/2
}

# inverse cdf, Finv(p, m, lambda)
qlaplace <- function(p, m, lambda) {
  # evaluate Laplace quantile (double-sided exponential)
  # Finverse(p, m, lambda) =
  # x<m:   ln(2p)/lambda+m
  # x>=m:  -ln(2-2p)/lambda+m
  # note that, by custom, 1/lambda is used in the pdf definition
  i <- ifelse(p<.5, 1, -1)
  i*log(1-i+i*2*p)*lambda+m
}

# random Laplace quantiles
rlaplace <- function(n, m, lambda)
  qlaplace(runif(n), m, lambda)
