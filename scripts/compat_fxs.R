## compatibility functions

# ABO compatibility

compABO<-function(iso = T, # isogroup TRUE/FALSE
                 dABO = "A", # donor group options ("A", "B", "AB", "O"),
                 cABO = "A") {   # candidate group options ("A", "B", "AB", "O"))

  res = NULL
  # verify function parameters
  if(!is.logical(iso)){stop("'iso' is not a logical value (T/F)!\n")
    } else if (!dABO %in% c("A", "B", "AB", "O")){stop("donor's group is not a valid option!\n")
    } else if (!cABO %in% c("A", "B", "AB", "O")){stop("candidate's group is not a valid option!\n")
        }
  
  if(iso == FALSE){
    res<-ifelse(dABO == "O", TRUE,
                ifelse(dABO == "A" & (cABO == "A" | cABO == "AB"), TRUE,
                       ifelse(dABO == "B" & (cABO == "B" | cABO == "AB"), TRUE,
                              ifelse(dABO == "AB" & cABO == "AB", TRUE, FALSE)
                              )
                       )
                )
    } else if (iso == TRUE){
      res<-ifelse(dABO == cABO, TRUE, FALSE)
      }
  
  return(res)
  }
         

# missmatchs HLA

mmHLA<-function(dA = c("01","02"), # donor's HLA-A typing
                dB = c("03","05"), # donor's HLA-B typing
                dDR = c("04","06"), # donor's HLA-DR typing
                cA = c("01","02"), # candidate's HLA-A typing
                cB = c("03","05"), # candidate's HLA-B typing
                cDR = c("04","06")){ # candidate's HLA-DR typing
  
  mmA = NULL
  mmB = NULL
  mmDR = NULL
  
  # verify function parameters
  if(!is.character(dA)){stop("donor's HLA-A typing is not valid!\n")}
  if(!is.character(dB)){stop("donor's HLA-B typing is not valid!\n")}
  if(!is.character(dDR)){stop("donor's HLA-DR typing is not valid!\n")}
  if(!is.character(cA)){stop("candidate's HLA-A typing is not valid!\n")}
  if(!is.character(cB)){stop("candidate's HLA-B typing is not valid!\n")}
  if(!is.character(cDR)){stop("candidate's HLA-DR typing is not valid!\n")}
    
  # compute missmatches
  mmA<-if_else((dA[1] %in% cA & dA[2] %in% cA) | (dA[1] %in% cA & (is.na(dA[2]) | dA[2] == "")), 0,
              if_else(dA[1] %in% cA | dA[2] %in% cA, 1,
                      if_else(!dA[1] %in% cA & (is.na(dA[2]) | dA[2] == ""), 1,
                              if_else(dA[1] == dA[2], 1,2))))
  
  mmB<-if_else((dB[1] %in% cB & dB[2] %in% cB) | (dB[1] %in% cB & (is.na(dB[2]) | dB[2] == "")), 0,
              if_else(dB[1] %in% cB | dB[2] %in% cB, 1,
                      if_else(!dB[1] %in% cB & (is.na(dB[2]) | dB[2] == ""), 1,
                              if_else(dB[1] == dB[2], 1,2))))
  
  mmDR<-if_else((dDR[1] %in% cDR & dDR[2] %in% cDR) | (dDR[1] %in% cDR & (is.na(dDR[2]) | dDR[2] == "")), 0,
              if_else(dDR[1] %in% cDR | dDR[2] %in% cDR, 1,
                      if_else(!dDR[1] %in% cDR & (is.na(dDR[2]) | dDR[2] == ""), 1,
                              if_else(dDR[1] == dDR[2],1,2))))
  
  # resume results
  mmHLA = mmA + mmB + mmDR
  mm = c(mmA,mmB,mmDR,mmHLA)
  names(mm) <- c("mmA","mmB","mmDR","mmHLA")
 
  return(mm) 
}


# xmatch
## this function give an error in a higher R version
xmatch<-function(dA = c("01","02"), # donor's HLA-A typing
                 dB = c("03","05"), # donor's HLA-B typing
                 dDR = c("04","06"), # donor's HLA-DR typing
                 df.abs = abs # data frame with candidates' HLA antibodies
){  
  
  # compile donors typing in a single vector
  hla<-c(paste0("A",dA[1]), paste0("A",dA[2]),
         paste0("B",dB[1]), paste0("B",dB[2]),
         paste0("DR",dDR[1]), paste0("DR",dDR[2]))
  
  # verify function parameters
  if(!is.character(dA)){stop("donor's HLA-A typing is not valid!\n")}
  if(!is.character(dB)){stop("donor's HLA-B typing is not valid!\n")}
  if(!is.character(dDR)){stop("donor's HLA-DR typing is not valid!\n")}
  
  # compute xmatch for each one of the HLA antibodies
  x<-tibble(data.frame(abs,
                       res = is.element(df.abs$abs, hla)
  )) %>% 
    group_by(ID) %>% 
    mutate(xm = if_else(sum(res)>0, TRUE, FALSE)) %>% 
    ungroup() %>% 
    distinct(ID,xm)
  
  return(x)
}

## v2 for the xmtach function 
xmatch.v2<-function(dA = c("01","02"), # donor's HLA-A typing
                    dB = c("03","05"), # donor's HLA-B typing
                    dDR = c("04","06"), # donor's HLA-DR typing
                    df.abs = ex.abs # data frame with candidates' HLA antibodies
){  
  
  # compile donors typing in a single vector
  hla<-c(paste0("A",dA[1]), paste0("A",dA[2]),
         paste0("B",dB[1]), paste0("B",dB[2]),
         paste0("DR",dDR[1]), paste0("DR",dDR[2]))
  
  # verify function parameters
  if(!is.character(dA)){stop("donor's HLA-A typing is not valid!\n")}
  if(!is.character(dB)){stop("donor's HLA-B typing is not valid!\n")}
  if(!is.character(dDR)){stop("donor's HLA-DR typing is not valid!\n")}
  
  # compute xmatch for each one of the HLA antibodies
  x<-df.abs %>% 
    mutate(xm=is.element(.$abs, hla))  %>% 
    filter(xm == TRUE) %>%  
    distinct(ID,xm)
  
  return(x)
}

## identify Hiper Imunized candidates according to given cutoff
hi<-function(cPRA = cPRA, cutoff = 85){
  res<-NULL
  res<-if_else(cPRA > cutoff, TRUE, FALSE)
}

## 
# function txscore(ageR = , race = , insurance= , causeESRD = 1, timeD = 1, diabetesR = F, coronary = F, albumin = , hemoglobin =, ageD = , diabetesD= F, ECD = F, mmHLA = )
txscore <- function(ageR = 20
                    , race = "White"
                    #, insurance = 0
                    , causeESRD = "Other"
                    , timeD = 12 #
                    , diabetesR = F
                    , coronary = F
                    , albumin = 1.5
                    , hemoglobin = 10
                    , ageD = 30
                    , diabetesD= "Absence"
                    , ECD = F
                    #, mmHLA = "0"
                    , mmHLA_A = 0
                    , mmHLA_B = 0
                    , mmHLA_DR = 0
){
  
  mmHLA_ <- as.numeric(mmHLA_A) + as.numeric(mmHLA_B) + as.numeric(mmHLA_DR)
  mmHLA <- ifelse(mmHLA_ == 0 , '0',
                  ifelse(mmHLA_ < 4, '1-3', '4-6'))
  
  ageR <- ifelse(ageR < 35 , 0.0993, 
                 ifelse(ageR <50 , -0.0784,
                        ifelse(ageR < 65, 0, 0.1881)))
  race <- ifelse(race == "White", 0, 
                 ifelse(race == "Black", 0.1609,
                        ifelse(race == "Hispanic", -0.2554, -0.4475)))
  causeESRD <- ifelse(causeESRD == "Diabetes", 0, 
                      ifelse(causeESRD == "Hypertension", 0.1541,
                             ifelse(causeESRD == "Glomerulonephritis", 0.1447,
                                    ifelse(causeESRD == "Cystic Disease", -0.1870, 0.3209))))
  timeD <- ifelse(timeD < 12, 0, 
                  ifelse(timeD < 36, -0.2618,
                         ifelse(timeD < 61, -0.3747, -0.1432)))
  diabetesR <- ifelse(diabetesR == T, 0.3021, 0)
  coronary <- ifelse(coronary == T, 0.2617, 0)
  albumin <- (albumin - 4)*(-0.2644)
  hemoglobin <- (hemoglobin - 12.3)*(-0.0451)
  ageD <- (ageD - 39)*0.0059
  diabetesD <- ifelse(diabetesD == "Absence", 0,  
                      ifelse(diabetesD == "Presence", 0.4596, -0.3308))
  ECD <- ifelse(ECD == T, 0.2082, 0)
  mmHLA <- ifelse(mmHLA == "0" , 0,
                  ifelse(mmHLA == "1-3", 0.3241, 0.3115))
  
  LP <- ageR + race + causeESRD + timeD + diabetesR + coronary + albumin + hemoglobin + ageD + diabetesD + ECD + mmHLA
  
  gamma <- 0.916
  
  PS = gamma * LP
  
  prob5y <- round((1-0.752292^exp(PS))*100,2)
  
  list(LP = LP
       , gamma = gamma
       , PS = PS
       , prob5y = prob5y)
  
}

