#### FINAL EXAM - PART 1

library(tidyverse)
library(pwr)

## POWER FOR MULTIPLE REGRESSION USING INCRIMENTAL PREDICTORS 

# determine df
# interested in the incremental prediction of one variable, so u = 1

#determine f2 using sr2 and R2
# sr2 = .10, R2 = .20


my.f2 <- .10/(1 - .20)
print(my.f2)
#f2=0.125

#calculate power
pwr.f2.test(u=1, f2=0.125, power=.85)

#calculate N
# in this case, u now again = number of predictors
N = 1 + 72 + 1
print(N)
# N = 74
