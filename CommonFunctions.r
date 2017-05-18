# Synthetic Data Project
# Common Functions

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

frequencyMode <- function(x, nbins=25, nTopMean=1) {

  # compute mode of vector x as weighted mean of top nTopMean frequencies using nbins cut intervals
  # tabulate frequencies
  # take weighted mean of all bin midpoints in top nTopMean frequencies
  xtab <- table(cut(x, nbins))
  # identify top nTopMean frequencies
  k <- which(xtab %in% sort(unique(xtab), decreasing=T)[1:nTopMean])
  sum(mapply(k, FUN=function(i) {
                      # multiply interval centerpoints by frequencies
                      # format is "(b0,b1]" from cut()
                      interval <- names(xtab)[i]
                      # locate comma
                      cpos <- regexpr(",", interval)[1]
                      # exclude leading ( and trailing ]
                      b0 <- as.numeric(substr(interval, 2, cpos-1))
                      b1 <- as.numeric(substr(interval, cpos+1, nchar(interval)-1))
                      # multiply frequency by midpoint of lower and upper bounds
                      as.numeric(xtab[i])*(b0+b1)/2
                    })) / sum(xtab[k])

}
