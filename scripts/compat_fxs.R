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
  mmA<-if_else(dA[1] %in% cA & dA[2] %in% cA, 0,
              if_else(dA[1] %in% cA | dA[2] %in% cA, 1, 
                     if_else(dA[1] == dA[2],1,2)))
  
  mmB<-if_else((dB[1] %in% cB & dB[2] %in% cB) | (dB[1] %in% cB & is.na(dB[2])), 0,
              if_else(dB[1] %in% cB | dB[2] %in% cB, 1, 
                     if_else(dB[1] == dB[2],1,2)))
  
  mmDR<-if_else((dDR[1] %in% cDR & dDR[2] %in% cDR) | (dDR[1] %in% cDR & is.na(dDR[2])), 0,
              if_else(dDR[1] %in% cDR | dDR[2] %in% cDR, 1, 
                     if_else(dDR[1] == dDR[2],1,2)))
  
  # resume results
  mmHLA = mmA + mmB + mmDR
  mm = c(mmA,mmB,mmDR,mmHLA)
  names(mm) <- c("mmA","mmB","mmDR","mmHLA")
 
  return(mm) 
}


# xmatch

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
