## ET algorithm functions ##

# identify Senior Program candidates for a senior donor
et_sp<-function(dage = 65, cage = 65){
  res<-NULL
  res<-ifelse(dage>=65 & cage>=65, 1, 0)
}

# ET points for mmHLA  
et_mmHLA<-function(dA = c("01","02"), # donor's HLA-A typing
                   dB = c("03","05"), # donor's HLA-B typing
                   dDR = c("04","06"), # donor's HLA-DR typing
                   cA = c("01","02"), # candidate's HLA-A typing
                   cB = c("03","05"), # candidate's HLA-B typing
                   cDR = c("04","06"), # candidate's HLA-DR typing
                   mm0 = 400, # points for 0 HLA mm on ETKAS points table
                   mm1 = 333.33, # points for 1 HLA mm on ET points table
                   mm2 = 266.67, # points for 2 HLA mm on ET points table
                   mm3 = 200, # points for 3 HLA mm on ET points table
                   mm4 = 133.33, # points for 4 HLA mm on ET points table
                   mm5 = 66.67, # points for 5 HLA mm on ET points table
                   mm6 = 0){ # points for 6 HLA mm on ET points table
  
  # verify function parameters
  if(!is.numeric(mm0) | mm0 < 0 | mm0 > 501){
    stop("points for 0 mmHLA (full match) is not valid!\n")}
  if(!is.numeric(mm1) | mm1 < 0 | mm1 > 501){
    stop("points for 1 mmHLA is not valid!\n")}
  if(!is.numeric(mm2) | mm2 < 0 | mm2 > 501){
    stop("points for 2 mmHLA is not valid!\n")}
  if(!is.numeric(mm3) | mm3 < 0 | mm3 > 501){
    stop("points for 3 mmHLA is not valid!\n")}
  if(!is.numeric(mm4) | mm4 < 0 | mm4 > 501){
    stop("points for 4 mmHLA is not valid!\n")}
  if(!is.numeric(mm5) | mm5 < 0 | mm5 > 501){
    stop("points for 5 mmHLA is not valid!\n")}
  if(!is.numeric(mm6) | mm6 < 0 | mm6 > 501){
    stop("points for 6 mmHLA is not valid!\n")}
  
  # apply mmHLA function
  mm<-mmHLA(dA = dA, dB = dB, dDR = dDR,
            cA = cA, cB = cB, cDR = cDR)
  
  pts<-if_else(sum(mm[4]) == 0, mm0,
               if_else(sum(mm[4]) == 1, mm1, 
                       if_else(sum(mm[4]) == 2, mm2, 
                               if_else(sum(mm[4]) == 3, mm3, 
                                       if_else(sum(mm[4]) == 4, mm4,
                                               if_else(sum(mm[4]) == 5, mm5,mm6))))))
  return(pts)
}


# ET points for time on dialysis (in months)
et_dial<-function(dial = 0, # candidate's time on dialysis
                  month = 2.78){ # points for each month on dialysis
  
  # verify function parameters
  if(!is.numeric(dial) | dial < 0 | dial > 499){
    stop("value for time on dialysis is not valid!\n")}
  if(!is.numeric(month) | month < 0 | month > 10){
    stop("attributed points for each month on dialysis is not valid!\n")}
  
  pts<-dial * month
  
  return(pts)
  
}


## resume function for ET algorithm punctuation
et_points<-function(iso = TRUE, # isogroup compatibility
                    dABO = "A", # donor's blood group
                    dA = c("1","2"), dB = c("15","44"), dDR = c("1","4"), # donor's HLA typing'
                    dage = 65, # donor's age
                    cdata = candidates, # data file with candidates
                    month = 2.78, # points for each month on dialysis
                    mm0 = 400, # points for 0 HLA mm on ETKAS points table
                    mm1 = 333.33, # points for 1 HLA mm on ET points table
                    mm2 = 266.67, # points for 2 HLA mm on ET points table
                    mm3 = 200, # points for 3 HLA mm on ET points table
                    mm4 = 133.33, # points for 4 HLA mm on ET points table
                    mm5 = 66.67, # points for 5 HLA mm on ET points table
                    mm6 = 0, # points for 6 HLA mm on ET points table
                    df.abs = abs, # data frame with candidates' HLA antibodies
                    n = 2 # slice first n rows
                    ){
  data<-cdata %>%
    left_join(xmatch.v2(df.abs = df.abs,
                        dA = dA, # donor's HLA-A typing
                        dB = dB, # donor's HLA-B typing
                        dDR = dDR))
  
  data %>% rowwise() %>%  
    mutate(donor_age = dage,
           SP = et_sp(dage = dage, cage = age),
           AM = ifelse(SP == 0 & cPRA >= 85, 1, 0),
           compBlood=ifelse(AM == 1, compABO(iso = FALSE, dABO = dABO, cABO = bg),
                            compABO(iso = iso, dABO = dABO, cABO = bg)),
           pointsHLA = et_mmHLA(dA = dA, dB = dB, dDR = dDR,
                                cA = c(A1,A2), cB = c(B1,B2), cDR = c(DR1,DR2),
                                mm0 = mm0, 
                                mm1 = mm1, 
                                mm2 = mm2, 
                                mm3 = mm3, 
                                mm4 = mm4, 
                                mm5 = mm5, 
                                mm6 = mm6),
           mmA = mmHLA(dA = dA, dB = dB, dDR = dDR,
                       cA = c(A1,A2), cB = c(B1,B2), cDR = c(DR1,DR2))["mmA"],
           mmB = mmHLA(dA = dA, dB = dB, dDR = dDR,
                       cA = c(A1,A2), cB = c(B1,B2), cDR = c(DR1,DR2))["mmB"],
           mmDR = mmHLA(dA = dA, dB = dB, dDR = dDR,
                        cA = c(A1,A2), cB = c(B1,B2), cDR = c(DR1,DR2))["mmDR"],
           mmHLA = mmA + mmB + mmDR,
           mm000 = ifelse(mmA + mmB + mmDR == 0, 1, 0),
           pointsDial = et_dial(month = month, dial = dialysis),
           pointsETx = round(pointsHLA + pointsDial + MMP)) %>%  ungroup() %>%
    filter(compBlood == TRUE & (xm == FALSE | is.na(xm))) %>% 
    # mutate(pointsET = case_when(SP == 1 ~ dialysis,
    #                             TRUE ~ pointsETx)) %>%
    mutate(pointsET = ifelse(SP == 1, dialysis, pointsETx),
           HI = hi(cPRA = cPRA, cutoff = 85)) %>%  
    arrange(desc(SP),desc(AM), desc(mm000), desc(pointsET)) %>%
    slice(1:n) %>%
    select(ID, bg,
           A1, A2, B1, B2, DR1, DR2,
           mmA, mmB, mmDR, mmHLA, 
           age, donor_age, dialysis, cPRA, HI,
           pointsET, SP, AM)
  

}

