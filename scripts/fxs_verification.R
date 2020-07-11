## functions verification
library(tidyverse)

## example for pt_mmHLA
pt_mmHLA(dA = c("01","02"), dB = c("03","04"), dDR = c("10","11"),
         cA = c("01","01"), cB = c("03","04"), cDR = c("10","11"))

# Example for applying compABO function
dt<-data.frame(dABO = sample(c("A","B","AB","O"),10, replace = T), 
               cABO = sample(c("A","B","AB","O"),10, replace = T))

dt %>% rowwise() %>% mutate(comp = compABO(iso = F, dABO = dABO, cABO = cABO)) 

# example for pt_age function
dt<-dt %>% mutate(dage = sample(c(65,50,35),10, replace = T),
                  cage = sample(c(60,40),10, replace = T))

dt %>% rowwise() %>% mutate(ptsAge = pt_age(dage = dage, cage = cage))

pt_PRA(100)

pt_dial(10,0.2)


# example for xmatch
xmatch(dA = c("01","02"), # donor's HLA-A typing
       dB = c("03","05"), # donor's HLA-B typing
       dDR = c("04","06"), # donor's HLA-DR typing
       acs = c("A01","A02","B05","B07","DR04","DR07","DR11"))

abs %>% group_by(ID) %>% .$abs

hla<-c("A1","A3","B5","B7","DR3","DR10")
tibble(data.frame(abs,res = is.element(abs$abs, hla))) %>% 
  group_by(ID) %>% 
  mutate(xmatch = if_else(sum(res)>0, TRUE, FALSE)) %>% 
  ungroup() %>% 
  distinct(ID,xmatch)
  




xmatch()

  