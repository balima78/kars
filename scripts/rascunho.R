## go for a for loop on the donors' file

source("scripts/read_data.R")
source("scripts/compat_fxs.R")
source("scripts/PT_fxs.R")

# add a columns to candidates' file to update respective donors
candidates$donor<-0

# create a list with the same length of the number of donors
res <- vector("list", length = dim(donors)[1])

# now the for loop
for (i in 1:dim(donors)[1]){
  candid<-candidates %>% filter(donor == 0)
  
  res[[i]]<-pt_points(iso = TRUE, # isogroup compatibility
                      dABO = donors$bg[i], # donor's blood group
                      dA = c(donors$A1[i],donors$A2[i]), 
                      dB = c(donors$B1[i],donors$B2[i]), 
                      dDR = c(donors$DR1[i],donors$DR2[i]),
                      dage = donors$age[i], # donor's age
                      cdata = candid, # data file with candidates
                      pra80 = 8, # points for a PRA equal or higher than 80%
                      pra50 = 4, # points for a PRA equal or higher than 50%
                      month = 0.1, # points for each month on dialysis
                      points = 4, # points for age difference in PT punctuation table
                      itemA = 12, # points for A) on PT points table
                      itemB = 8, # points for B) on PT points table
                      itemC = 4, # points for C) on PT points table
                      itemD = 2, # points for D) on PT points table
                      itemE = 1, # points for E) on PT points table
                      df.abs = abs) %>% 
    mutate(donor = donors$ID[i])
  
  candidates<-candidates %>% 
    mutate(donor = case_when(ID %in% res[[i]]$ID ~ donors$ID[i],
                             TRUE ~ donor))
  
} 

# list with the results
res
# candidates' file with respective donors
candidates %>% filter(donor > 0)

## bind the results in the list
do.call(rbind, res)

# alternatively
library(data.table)
rbindlist(res)


as.data.frame(
pt_points()
)
