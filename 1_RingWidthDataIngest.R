# Code outline for reading in the data & metadata associated iwth the tree ring widths

# 1. open the different series; I downloaded a bunch of different formats into the sharedGoogle Drive folder; some have metadata attached; some don't
#     -- TRY using dplR, but that may be a bit of a pain
# 
# 2. Delete 0s in the first rings --> Miranda discovered after the fact that some of the inner rings couldn't be measured and we need to get rid of the 0s
# 3. Figure out how to crossdate in R
#4. Test