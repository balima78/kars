## functions verification
library(tidyverse)

## example for pt_mmHLA
pt_mmHLA(dA = c("01","02"), dB = c("03","04"), dDR = c("10","11"),
         cA = c("01","01"), cB = c("03","04"), cDR = c("10","11"))

# Example for applying compABO function
dt<-data.frame(dABO = sample(c("A","B","AB","O"),10, replace = T), 
               cABO = sample(c("A","B","AB","O"),10, replace = T))

dt %>% 
  rowwise() %>% 
  mutate(comp = compABO(iso = T, dABO = dABO, cABO = cABO)) 

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

## example dor et_sp

candidatos<-ex.candidates %>% rowwise() %>% 
  mutate(sp=et_sp(dage = 65, cage = age)) %>% ungroup()

## example for et_mmHLA
et_mmHLA(dA = c("01","02"),
         dB = c("03","05"), 
         dDR = c("04","06"), 
         cA = c("02","01"), 
         cB = c("07","05"), 
         cDR = c("04","06"))

mmHLA(dA = c("01","02"),
      dB = c("03","05"), 
      dDR = c("04","06"), 
      cA = c("02","01"), 
      cB = c("07","05"), 
      cDR = c("04","06"))


et_dial(dial = 301)


## verifying function et_points 
candidatos<-read.csv2("files/candid.csv")

candidatos<-candidatos %>% mutate_at(vars(bg, A1, A2, B1, B2, DR1, DR2), as.character)

as.data.frame(
et_points(cdata = ex.candidates, df.abs = ex.abs, dage = 70) )



teste<-xmatch.v2(dA = c("1","2"), 
                    dB = c("7","8"), 
                    dDR = c("1","3"), 
                    df.abs = ex.abs)

table(teste$abs, teste$xm)

candidatos %>% left_join(teste)

pt_points(cdata = ex.candidates, df.abs = ex.abs)



candidatos %>% rowwise() %>% mutate(compBlood=compABO(iso = T, dABO = "A", cABO = bg))

candidatos %>% rowwise() %>% 
  mutate(pointsHLA = pt_mmHLA(dA = c("2","24"), # donor's HLA-A typing
                              dB = c("15","44"), # donor's HLA-B typing
                              dDR = c("1","4"), # donor's HLA-DR typing
                              cA = c(A1,A2),
                              cB = c(B1,B2),
                              cDR = c(DR1,DR2)))

candidatos %>% rowwise() %>% 
  mutate(pointsAge = pt_age(cage = age)) %>% ungroup() %>% 
  filter(pointsAge == 4)

