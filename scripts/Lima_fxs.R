## Lima's algorithm functions ##
 

## identify senior candidates eligible for Lima's algorithm
lima_sp<-function(dage = 65, cage = 65){
  res<-NULL
  res<-ifelse(dage >= 65 & cage >= 65,1,
              ifelse(dage < 65 & cage >= 65,2,0))
  res
}

## resume function for Lima's algorithm orders
lima_order<-function(iso = TRUE, # isogroup compatibility
                     dABO = "A", # donor's blood group
                     dA = c("1","2"), dB = c("15","44"), dDR = c("1","4"), # donor's HLA typing'
                     dage = 65, # donor's age
                     cdata = candidates, # data file with candidates
                     df.abs = abs, # data frame with candidates' HLA antibodies
                     n = 2 # slice first n rows
                     ){
  # compute virtual crossmatch
  data<-cdata %>%
    left_join(xmatch.v2(df.abs = df.abs,
                        dA = dA, # donor's HLA-A typing
                        dB = dB, # donor's HLA-B typing
                        dDR = dDR))
  data %>% rowwise() %>%  
    mutate(donor_age = dage,
           SP = lima_sp(dage = dage, cage = age),
           compBlood=compABO(iso = iso, dABO = dABO, cABO = bg),
           mmA = mmHLA(dA = dA, dB = dB, dDR = dDR,
                       cA = c(A1,A2), cB = c(B1,B2), cDR = c(DR1,DR2))["mmA"],
           mmB = mmHLA(dA = dA, dB = dB, dDR = dDR,
                       cA = c(A1,A2), cB = c(B1,B2), cDR = c(DR1,DR2))["mmB"],
           mmDR = mmHLA(dA = dA, dB = dB, dDR = dDR,
                        cA = c(A1,A2), cB = c(B1,B2), cDR = c(DR1,DR2))["mmDR"],
           mmHLA = mmA + mmB + mmDR) %>%  ungroup() %>%
    filter(compBlood == TRUE & (xm == FALSE | is.na(xm)) & SP != 2) %>% 
    mutate(HI = hi(cPRA = cPRA, cutoff = 85)) %>%  
    arrange(desc(SP), color, mmHLA, desc(dialysis)) %>%
    slice(1:n) %>%
    select(ID, bg,
           A1, A2, B1, B2, DR1, DR2,
           mmA, mmB, mmDR, mmHLA, 
           age, donor_age, dialysis, cPRA, HI,
           color, SP)
}
           
  
  
  
  
  
