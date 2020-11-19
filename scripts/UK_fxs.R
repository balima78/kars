## UK algorithm functions ##

# matchability score:
# counting number of matched donors per candidate,
# blood group identical & HLA compatible
# HLA mismatch level 1 or 2

donormatch<-function(data = ex.donors, 
                     cA = c("1","2"), cB = c("3","5"), cDR = c("4","6"),
                     cABO = "A",
                     abs = c("DR1", "DR7", "A1", "A2")){
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
  # data %>% ungroup()
}


donormatch()


res<-NULL

dados<-as.data.frame(ex.candidates.pt)

for (i in 1:20){
  res[i]<-donormatch(data = ex.donors,
                     cA = c(dados[i,"A1"],dados[i,"A2"]),
                     cB = c(dados[i,"B1"],dados[i,"B2"]),
                     cDR = c(dados[i,"DR1"],dados[i,"DR2"]),
                     cABO = dados[i,"bg"])
  res
}

res
