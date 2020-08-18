## PT algorithm functions ##

# PT points for age
pt_age<-function(dage = 60, # donor's age
                 cage = 40, # candidate's age
                 points = 4){ # points from PT punctuation table
  
  # verify function parameters
  if(!is.numeric(dage) | dage < 18 | dage > 99){
    stop("donor's age is not valid!\n")}
  if(!is.numeric(cage) | cage < 18 | cage > 99){
    stop("candidate's age is not valid!\n")}
  if(!is.numeric(points) | points < 1 | points > 20){
    stop("age points are not valid!\n")}
  
  pts<-ifelse((dage > 60 & cage < 55) | (dage < 40 & cage > 55), 0, points)
  
  return(pts)
}

# PT points for mmHLA
pt_mmHLA<-function(dA = c("01","02"), # donor's HLA-A typing
                   dB = c("03","05"), # donor's HLA-B typing
                   dDR = c("04","06"), # donor's HLA-DR typing
                   cA = c("01","02"), # candidate's HLA-A typing
                   cB = c("03","05"), # candidate's HLA-B typing
                   cDR = c("04","06"), # candidate's HLA-DR typing
                   itemA = 12, # points for A) on PT points table
                   itemB = 8, # points for B) on PT points table
                   itemC = 4, # points for C) on PT points table
                   itemD = 2, # points for D) on PT points table
                   itemE = 1){ # points for E) on PT points table
  
  # verify function parameters
  if(!is.numeric(itemA) | itemA < 0 | itemA > 99){
    stop("points for 0 mmHLA (full match) is not valid!\n")}
  if(!is.numeric(itemB) | itemB < 0 | itemB > 99){
    stop("points for 0 mmB and mmDR is not valid!\n")}
  if(!is.numeric(itemC) | itemC < 0 | itemC > 99){
    stop("points for 1 mmB or mmDR is not valid!\n")}
  if(!is.numeric(itemD) | itemD < 0 | itemD > 99){
    stop("points for 1 mmB and 1 mmDR is not valid!\n")}
  if(!is.numeric(itemE) | itemE < 0 | itemE > 99){
    stop("points for more than 2 mmB and mmDR is not valid!\n")}
  
  # apply mmHLA function
  mm<-mmHLA(dA = dA, dB = dB, dDR = dDR,
            cA = cA, cB = cB, cDR = cDR)
  
  pts<-if_else(mm["mmHLA"] == 0, itemA,
               if_else(mm["mmB"]+mm["mmDR"] == 0, itemB, 
                       if_else(mm["mmB"]+mm["mmDR"] == 1, itemC, 
                               if_else(mm["mmB"] == 1 & mm["mmDR"] == 1, itemD, itemE))))
  return(pts)
  
}

# PT points for PRA
pt_PRA<-function(PRA = 0, # candidate's PRA value
                 pra80 = 8, # points for a PRA equal or higher than 80%
                 pra50 = 4){ # points for a PRA equal or higher than 50%
  
  # verify function parameters
  if(!is.numeric(PRA) | PRA < 0 | PRA > 100){
    stop("PRA value is not valid!\n")}
  if(!is.numeric(pra80) | pra80 < 0 | pra80 > 100){
    stop("attributed points for a PRA >= 80% is not valid!\n")}
  if(!is.numeric(pra50) | pra50 < 0 | pra50 > 100){
    stop("attributed points for a PRA >= 50% is not valid!\n")}
  
  pts<-if_else(PRA >= 80, pra80,
               if_else(PRA >= 50, pra50, 0))
  
  return(pts)
  
}

# PT points for time on dialysis (in months)
pt_dial<-function(dial = 0, # candidate's time on dialysis
                  month = 0.1){ # points for each month on dialysis
  
  # verify function parameters
  if(!is.numeric(dial) | dial < 0 | dial > 499){
    stop("value for time on dialysis is not valid!\n")}
  if(!is.numeric(month) | month < 0 | month > 10){
    stop("attributed points for each month on dialysis is not valid!\n")}
  
  pts<-dial * month
  
  return(pts)
  
}


## resume function for PT algorithm punctuation

pt_points<-function(iso = TRUE, # isogroup compatibility
                    dABO = "A", # donor's blood group
                    dA = c("1","2"), dB = c("15","44"), dDR = c("1","4"), # donor's HLA typing'
                    dage = 65, # donor's age
                    cdata = candidates, # data file with candidates
                    pra80 = 8, # points for a PRA equal or higher than 80%
                    pra50 = 4, # points for a PRA equal or higher than 50%
                    month = 0.1, # points for each month on dialysis
                    points = 4, # points for age difference in PT punctuation table
                    itemA = 12, # points for A) on PT points table
                    itemB = 8, # points for B) on PT points table
                    itemC = 4, # points for C) on PT points table
                    itemD = 2, # points for D) on PT points table
                    itemE = 1, # points for E) on PT points table
                    df.abs = abs, # data frame with candidates' HLA antibodies
                    n = 2 # slice first n rows
){
  
  cdata %>% 
    left_join(xmatch.v2(dA = dA, dB = dB, dDR = dDR,
                     df.abs = df.abs)) %>%
    rowwise() %>% 
    mutate(donor_age = dage,
           compBlood=compABO(iso = iso, dABO = dABO, cABO = bg),
           pointsHLA = pt_mmHLA(dA = dA, dB = dB, dDR = dDR,
                                cA = c(A1,A2), cB = c(B1,B2), cDR = c(DR1,DR2),
                                itemA = itemA,
                                itemB = itemB,
                                itemC = itemC,
                                itemD = itemD,
                                itemE = itemE
           ),
           mmA = mmHLA(dA = dA, dB = dB, dDR = dDR,
                         cA = c(A1,A2), cB = c(B1,B2), cDR = c(DR1,DR2))["mmA"],
           mmB = mmHLA(dA = dA, dB = dB, dDR = dDR,
                         cA = c(A1,A2), cB = c(B1,B2), cDR = c(DR1,DR2))["mmB"],
           mmDR = mmHLA(dA = dA, dB = dB, dDR = dDR,
                         cA = c(A1,A2), cB = c(B1,B2), cDR = c(DR1,DR2))["mmDR"],
           mmHLA = mmA + mmB + mmDR,
           pointsPRA = pt_PRA(pra80 = pra80, pra50 = pra50, cPRA),
           pointsDial = pt_dial(month = month, dialysis),
           pointsAge = pt_age(dage = dage, cage = age, points = points),
           pointsPT = pointsHLA + pointsPRA + pointsDial + pointsAge,
           HI = hi(cPRA = cPRA, cutoff = 85)) %>% ungroup() %>% 
    filter(compBlood == TRUE & (xm == FALSE | is.na(xm))) %>% 
    arrange(desc(HI), desc(pointsPT)) %>% # despacho 11420/2008 
    slice(1:n) %>% 
    select(ID, bg, 
           A1, A2, B1, B2, DR1, DR2, 
           mmA, mmB, mmDR, mmHLA, 
           age, donor_age, dialysis, cPRA, HI,
           pointsPT)
  
}


