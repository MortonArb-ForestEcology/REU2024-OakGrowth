# # # # # # # 
# 3. Graph Establishment Conditions ----
#  - Plan A: Originally wanted to compare trees with known establishment vs. unknown, but we don't have good enough data
#  - **Plan B** [what we're going with]: Describing establishment Conditions
#  - 3.1. Describing the variability in year of establishment --> are they from the same year or are there groups before/after certain events like the Arb's founding or when the region was settled?
#.     -- CR Note: This is the piece that will really be of most interest to folks at the Arboretum
#  - 3.2. Describe the variability in growth rates during establishment --> is it a pretty tight/normal distribution or super variable?
#  -- [[Added from CR: can look at the *trend* in initial growth and see if it's declining (sign of geometric decline; typical of open grown) or increasing or stead]]
# # # # # # # 
dfTree <- read.csv(file.path(path.out, "TreeData_toAnalyze.csv"))
summary(dfTree)
# # # # # # # 

