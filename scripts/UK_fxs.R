## UK algorithm functions ##
library(tidyverse)

# donor-recipient Risk Index Combination
ric<-function(DRI = 'D1', # Donor RisK Index group
              cdata =ex.candidates.uk, # data file with candidates
              D1R1 = 1000,
              D1R2 = 700,
              D1R3 = 350,
              D1R4 = 0,
              D2R1 = 700,
              D2R2 = 1000,
              D2R3 = 500,
              D2R4 = 350,
              D3R1 = 350,
              D3R2 = 500, 
              D3R3 = 1000,
              D3R4 = 700,
              D4R1 = 0,
              D4R2 = 350,
              D4R3 = 700,
              D4R4 = 1000
              ) { 
  
  # verify function parameters
  if(!DRI %in% c('D1','D2','D3','D4')){stop("DRI is not a valid option! Select of 'D1','D2','D3','D4' \n")
    } else if (D1R1 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
    } else if (D1R2 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
    } else if (D1R3 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
    } else if (D1R4 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
    } else if (D2R1 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
    } else if (D2R2 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
    } else if (D2R3 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
    } else if (D2R4 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
    } else if (D3R1 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
    } else if (D3R2 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
    } else if (D3R3 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
    } else if (D3R4 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
    } else if (D4R1 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
    } else if (D4R2 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
    } else if (D4R3 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
    } else if (D4R4 < 0 | D1R1 > 1000){stop("D1R1 is not between 0 and 1000!\n")
  }
  
  if (DRI == 'D1') {
    cdata <- cdata %>% mutate(ric = case_when(RRI == 'R1' ~ D1R1,
                                              RRI == 'R2' ~ D1R2,
                                              RRI == 'R3' ~ D1R3,
                                              RRI == 'R4' ~ D1R4))
    } else if (DRI == 'D2') {
    cdata <- cdata %>% mutate(ric = case_when(RRI == 'R1' ~ D2R1,
                                              RRI == 'R2' ~ D2R2,
                                              RRI == 'R3' ~ D2R3,
                                              RRI == 'R4' ~ D2R4))
    } else if (DRI == 'D3') {
      cdata <- cdata %>% mutate(ric = case_when(RRI == 'R1' ~ D3R1,
                                                RRI == 'R2' ~ D3R2,
                                                RRI == 'R3' ~ D3R3,
                                                RRI == 'R4' ~ D3R4))
    } else {
      cdata <- cdata %>% mutate(ric = case_when(RRI == 'R1' ~ D4R1,
                                                RRI == 'R2' ~ D4R2,
                                                RRI == 'R3' ~ D4R3,
                                                RRI == 'R4' ~ D4R4))
    }
  cdata
}

# # HLA match and age combined and Total HLA mismatch
# hla.age<-function(dA = c("1","2"), # donor's HLA-A typing
#                      dB = c("3","5"), # donor's HLA-B typing
#                      dDR = c("4","7"), # donor's HLA-DR typing
#                      dage = 65, # donor's age
#                      cdata = ex.candidates.uk){ # candidates data set 
#   
#   # verify function parameters
#   if(!is.character(dA)){stop("donor's HLA-A typing is not valid!\n")}
#   if(!is.character(dB)){stop("donor's HLA-B typing is not valid!\n")}
#   if(!is.character(dDR)){stop("donor's HLA-DR typing is not valid!\n")}
#   
#   cdata %>% rowwise() %>% 
#     mutate(mmA = mmHLA(dA = dA, dB = dB, dDR = dDR,
#                        cA = c(A1,A2), cB = c(B1,B2), cDR = c(DR1,DR2))["mmA"],
#            mmB = mmHLA(dA = dA, dB = dB, dDR = dDR,
#                        cA = c(A1,A2), cB = c(B1,B2), cDR = c(DR1,DR2))["mmB"],
#            mmDR = mmHLA(dA = dA, dB = dB, dDR = dDR,
#                         cA = c(A1,A2), cB = c(B1,B2), cDR = c(DR1,DR2))["mmDR"],
#            level = case_when(mmA + mmB + mmDR == 0 ~ 1,
#                               (mmDR == 0 & mmB <=1) | (mmDR == 1 & mmB == 0) ~ 2,
#                               (mmDR == 0 & mmB == 2) |(mmDR == 1 & mmB == 1) ~ 3,
#                               TRUE ~ 4),
#            pts.hla.age = case_when(level == 1 ~ 1200*cos(age/18)+2300,
#                                    level == 2 ~ 750*cos(age/18)+1500,
#                                    TRUE ~ 400*sin(age/50)
#                                    ),
#            total.HLA = case_when(mmA + mmB + mmDR == 0 ~ 0,
#                                  mmA + mmB + mmDR == 1 ~ -100,
#                                  mmA + mmB + mmDR < 4 ~ -150,
#                                  TRUE ~ -250)
#            ) %>% ungroup()
#   
# }

# Donor recipient age difference
age.diff<-function(dage = 60,
                   cage = 50){
  # verify ages
  if(!is.numeric(dage) | dage < 18 | dage > 99) {stop("donor's age is not valid!\n")}
  if(!is.numeric(cage) | cage < 18 | cage > 99) {stop("candidate's age is not valid!\n")}
  
  res<-NULL
  
  res<- (-1/2)*((dage-cage)^2)

}

# points blood group B match
b.blood<-function(dABO = "B",
                  cABO = "O",
                  tier = "B",
                  pts = -1000){
  res=NULL
  
  res<-ifelse(cABO == 'B' & dABO == 'O' & tier == 'B', pts, 0)
  
  return(res)
}



# ABO compatibility UK
compABO.uk<-function(dABO = "A", # donor group options ("A", "B", "AB", "O"),
                     cABO = "A", # candidate group options ("A", "B", "AB", "O"))
                     tier = "B") {   # candidates' TIER (optins A and B)
  
  res = NULL
  # verify function parameters
  if (!dABO %in% c("A", "B", "AB", "O")) {stop("donor's group is not a valid option!\n")} 
  if (!cABO %in% c("A", "B", "AB", "O")) {stop("candidate's group is not a valid option!\n")} 
  if (! tier %in% c('A','B')) {stop("candidate's Tier is not a valid option!\n")}
  
  if(tier == 'B'){
  res<-ifelse(dABO == "O" & (cABO == "O" | cABO == "B"), TRUE,
                 ifelse(dABO == "A" & (cABO == "A" | cABO == "AB"), TRUE,
                       ifelse(dABO == "B" & cABO == "B", TRUE,
                              ifelse(dABO == "AB" & cABO == "AB", TRUE, FALSE)
                       )
                )
    )
  } else {res<-ifelse(dABO == "O", TRUE,
                      ifelse(dABO == "A" & (cABO == "A" | cABO == "AB"), TRUE,
                             ifelse(dABO == "B" & cABO == "B", TRUE,
                                    ifelse(dABO == "AB" & cABO == "AB", TRUE, FALSE)
                             )
                      )
  )
  }
  
  return(res)
}

## resume function for UK algorithm punctuation
uk_points<-function(DRI = 'D1', # Donor RisK Index group
                    dA = c("1","2"), dB = c("15","44"), dDR = c("1","4"), # donor's HLA typing'
                    dABO = "O", # donors' blood group
                    dage = 65, # donors' age
                    cdata =ex.candidates.uk, # data file with candidates
                    D1R1 = 1000,
                    D1R2 = 700,
                    D1R3 = 350,
                    D1R4 = 0,
                    D2R1 = 700,
                    D2R2 = 1000,
                    D2R3 = 500,
                    D2R4 = 350,
                    D3R1 = 350,
                    D3R2 = 500, 
                    D3R3 = 1000,
                    D3R4 = 700,
                    D4R1 = 0,
                    D4R2 = 350,
                    D4R3 = 700,
                    D4R4 = 1000,
                    ptsDial = 1,
                    a1 = 2300, # value on HLA match and age combined formula
                    a2 = 1500, # value on HLA match and age combined formula
                    b1 = 1200, # value on HLA match and age combined formula
                    b2 = 750, # value on HLA match and age combined formula
                    b3 = 400, # value on HLA match and age combined formula
                    m = 40, # matchability formula
                    nn = 4.5, # matchability formula
                    o = 4.7, # matchability formula
                    mm1 = -100, # pontos a substrair para 1 mm
                    mm23 = -150, # pontos a substrair para 2-3 mm 
                    mm46 = -250, # pontos a substrair para 4-6 mm
                    pts = -1000, # pontos a substrair para
                    df.abs = ex.abs, # data frame with candidates' HLA antibodies
                    n = 2 # slice first n rows
){
  cdata<-cdata %>% 
    left_join(xmatch.v2(dA = dA, dB = dB, dDR = dDR,
                        df.abs = df.abs))
  cdata<-ric(DRI = DRI, D1R1 = D1R1, D1R2 = D1R2, D1R3 = D1R3, D1R4 = D1R4,
             D2R1 = D2R1, D2R2 = D2R2, D2R3 = D2R3, D2R4 = D2R4,
             D3R1 = D3R1, D3R2 = D3R2, D3R3 = D3R3, D3R4 = D3R4,
             D4R1 = D4R1, D4R2 = D4R2, D4R3 = D4R3, D4R4 = D4R4,
             cdata = cdata)
  
  cdata<-cdata %>% rowwise() %>% 
    mutate(donor_age = dage,
           compBlood = compABO.uk(dABO = dABO, cABO = bg, tier = Tier), 
           mmA = mmHLA(dA = dA, dB = dB, dDR = dDR,
                       cA = c(A1,A2), cB = c(B1,B2), cDR = c(DR1,DR2))["mmA"],
           mmB = mmHLA(dA = dA, dB = dB, dDR = dDR,
                       cA = c(A1,A2), cB = c(B1,B2), cDR = c(DR1,DR2))["mmB"],
           mmDR = mmHLA(dA = dA, dB = dB, dDR = dDR,
                        cA = c(A1,A2), cB = c(B1,B2), cDR = c(DR1,DR2))["mmDR"],
           mmHLA = mmA + mmB + mmDR,
           level = case_when(mmA + mmB + mmDR == 0 ~ 1,
                             (mmDR == 0 & mmB <=1) | (mmDR == 1 & mmB == 0) ~ 2,
                             (mmDR == 0 & mmB == 2) |(mmDR == 1 & mmB == 1) ~ 3,
                             TRUE ~ 4),
           pts.hla.age = case_when(level == 1 ~ b1*cos(age/18)+a1,
                                   level == 2 ~ b2*cos(age/18)+a2,
                                   TRUE ~ b3*sin(age/50)
                                   ),
           total.HLA = case_when(mmA + mmB + mmDR == 0 ~ 0,
                                 mmA + mmB + mmDR == 1 ~ mm1,
                                 mmA + mmB + mmDR < 4 ~ mm23,
                                 TRUE ~ mm46),
           matchability = round(m * (1+(MS/nn)^o),1),  # compute matchability points from Match Score
           pts.age = age.diff(dage = dage, cage = age),
           pts.abo = b.blood(dABO = dABO, cABO = bg, tier = Tier, pts = pts),
           pointsUK = round(ifelse(Tier == "A", 
                             9999, 
                             ric + pts.hla.age + matchability + pts.age + total.HLA + pts.abo),1)
           ) %>% 
    ungroup() %>% 
    filter(compBlood == TRUE & (xm == FALSE | is.na(xm))) %>%
    arrange(Tier, desc(pointsUK), desc(matchability), desc(dialysis)) %>%  
    slice(1:n) %>% 
    select(ID, bg, 
           A1, A2, B1, B2, DR1, DR2, 
           matchability, 
           mmA, mmB, mmDR, mmHLA, 
           age, donor_age, dialysis, cPRA, Tier,
           pointsUK)
  
  cdata
  
}


uk_points()
#exemplo<-uk_points()

# matchability score:
# counting number of matched donors per candidate,
# blood group identical & HLA compatible
# HLA mismatch level 1 or 2

donormatch<-function(data = ex.donors, 
                     cA = c("1","2"), cB = c("14","15"), cDR = c("4","6"),
                     cABO = "A",
                     abs = c("DR1", "DR7", "A1", "A3")){
  data<-data %>% rowwise() %>% mutate(abocomp = compABO(iso = T, 
                                                        dABO = bg, 
                                                        cABO = cABO),
                                      mmA = mmHLA(dA = c(A1,A2), dB = c(B1,B2), dDR = c(DR1,DR2),
                                                  cA = cA, cB = cB, cDR = cDR)["mmA"],
                                      mmB = mmHLA(dA = c(A1,A2), dB = c(B1,B2), dDR = c(DR1,DR2),
                                                  cA = cA, cB = cB, cDR = cDR)["mmB"],
                                      mmDR = mmHLA(dA = c(A1,A2), dB = c(B1,B2), dDR = c(DR1,DR2),
                                                   cA = cA, cB = cB, cDR = cDR)["mmDR"],
                                      level12 = ifelse((mmA == 0 & mmB == 0 & mmDR == 0) |
                                                         (mmA == 1 & mmB == 0 & mmDR == 0) |
                                                         (mmA == 0 & mmB == 1 & mmDR == 0) |
                                                         (mmA == 1 & mmB == 1 & mmDR == 0) |
                                                         (mmA == 2 & mmB == 0 & mmDR == 0) |
                                                         (mmA == 2 & mmB == 1 & mmDR == 0) |
                                                         (mmA == 0 & mmB == 0 & mmDR == 1) |
                                                         (mmA == 1 & mmB == 0 & mmDR == 1) |
                                                         (mmA == 2 & mmB == 0 & mmDR == 1), TRUE, FALSE),
                                      vmatch = sum(is.element(abs, 
                                                              c(paste0("A",A1), paste0("A",A2),
                                                                paste0("B",B1), paste0("B",B2),
                                                                paste0("DR",DR1), paste0("DR",DR2)))),
                                      matched=ifelse(abocomp == T & level12 == T & vmatch == 0, TRUE, FALSE)
  )
  
  dim(data)[1] - data %>% count(matched) %>% filter(matched == FALSE) %>% .$n
  #data %>% ungroup()
}

################ para apagar ###################
# donormatch()
# 
# # teste para um candidato
# res<-NULL
# 
# dados<-as.data.frame(ex.candidates.pt)
# 
# for (i in 1:20){
#   res[i]<-donormatch(data = ex.donors,
#                      cA = c(dados[i,"A1"],dados[i,"A2"]),
#                      cB = c(dados[i,"B1"],dados[i,"B2"]),
#                      cDR = c(dados[i,"DR1"],dados[i,"DR2"]),
#                      cABO = dados[i,"bg"],
#                      abs = NULL)
#   res
# }
# 
# res
# 
# 
# # criar tabela abs com um candidato por linha
# tab.abs<-ex.abs %>% group_by(ID) %>% nest() %>% ungroup()
# # juntar abs à tabela de candidatos
# ex.candidates.pt<-ex.candidates.pt %>% left_join(tab.abs)
# 
# # testar num loop depois de juntar os abs
# res<-NULL
# 
# dados<-as.data.frame(ex.candidates.pt)
# 
# for (i in 1:dim(ex.candidates.pt)[1]){
#   res[i]<-donormatch(data = ex.donors,
#                      cA = c(dados[i,"A1"],dados[i,"A2"]),
#                      cB = c(dados[i,"B1"],dados[i,"B2"]),
#                      cDR = c(dados[i,"DR1"],dados[i,"DR2"]),
#                      cABO = dados[i,"bg"],
#                      abs = as.data.frame(dados[i,"data"][[1]]) %>% .$abs)
#   res
# }



