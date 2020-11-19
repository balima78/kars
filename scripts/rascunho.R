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

##### compute MMP from ETKAS
# read candidates files without MMP's
ex.candid <- read_csv2("files/candid.csv")

## HLA frequencies from portuguese CEDACE donors (Lima, 2013)
# A - HLA-A alleles; B - HLA-B alleles; DR - HLA-DR alleles;
# n - allele countings; type integer
# freq - allele relative frequencies; type numeric (between 0 and 100)
hlaA <- read.csv2("files/hlaA.csv") %>% 
  mutate_at(vars(A),as.character)
hlaB <- read.csv2("files/hlaB.csv") %>% 
  mutate_at(vars(B),as.character)
hlaDR <- read.csv2("files/hlaDR.csv") %>% 
  mutate_at(vars(DR),as.character)

## ABO blood group frequencies from portuguese blood donors (Duran et al, 2007)
# abo - blood groups; type character (A, AB, B, O) 
# freq - ABO relative frequencies; type numeric (between 0 and 1)
abo <- read.csv2("files/abo.csv") %>% 
  mutate_at(vars(abo),as.character)

## compute MMP from ETKAS
# compute the sum of squared frequencies for each loci
SallA <-sum(hlaA$freq^2)
SallB <-sum(hlaB$freq^2)
SallDR <-sum(hlaDR$freq^2)

# join to candidates data file the respective alle frequency
ex.candid<-ex.candid %>% left_join(hlaA %>% select(A,freq), by = c("A1" = "A")) 
ex.candid<- ex.candid %>% rename(a1=freq)

ex.candid<-ex.candid %>% left_join(hlaA %>% select(A,freq), by = c("A2" = "A")) 
ex.candid<- ex.candid %>% rename(a2=freq)

ex.candid<-ex.candid %>% left_join(hlaB %>% select(B,freq), by = c("B1" = "B")) 
ex.candid<- ex.candid %>% rename(b1=freq)

ex.candid<-ex.candid %>% left_join(hlaB %>% select(B,freq), by = c("B2" = "B")) 
ex.candid<- ex.candid %>% rename(b2=freq)

ex.candid<-ex.candid %>% left_join(hlaDR %>% select(DR,freq), by = c("DR1" = "DR")) 
ex.candid<- ex.candid %>% rename(dr1=freq)

ex.candid<-ex.candid %>% left_join(hlaDR %>% select(DR,freq), by = c("DR2" = "DR")) 
ex.candid<- ex.candid %>% rename(dr2=freq)

ex.candid<-ex.candid %>% left_join(abo, by = c("bg" = "abo")) 
ex.candid<- ex.candid %>% rename(abo=freq)

# compute MMP2 and add it to the data file
ex.candid$MMP2 <- with(ex.candid,
                           (((2*(a1+a2)*(1 - a1 - a2)) - a1^2 - a2^2 + SallA) /
                              ((a1+a2)^2))
                           + (((2*(b1+b2)*(1 - b1 - b2)) - b1^2 - b2^2 + SallB) /
                                ((b1+b2)^2))
                           + (((2*(dr1+dr2)*(1 - dr1 - dr2) ) - dr1^2 - dr2^2 + SallDR) /
                                ((dr1+dr2)^2))
)

# compute MMP0 and add it to the data file
ex.candid$MMP0 <- with(ex.candid,
                           (a1+a2)^2 * (b1+b2)^2 * (dr1+dr2)^2)

# compute MMP1 and add it to the data file
ex.candid$MMP1 <- with(ex.candid, 
                           MMP0 * MMP2)

# compute MMP and add it to the data file
ex.candid$MMP<-with(ex.candid,
                        100 * (1-(abo * (1-cPRA/100) * (MMP0 + MMP1)))^1000
)


write.csv2(ex.candid,"candidates.csv")

library(gtsummary)
library(gt)
teste<-pt_points(cdata= candidatos, df.abs = ex.abs, n=20) %>% 
  select(bg, age, dialysis)  
  tbl_summary(teste) %>% as_gt() %>% class()

  
  gt(pt_points(cdata= candidatos, df.abs = ex.abs, n=20)) %>% class()
  
  pt_points(cdata= candidatos, df.abs = ex.abs, n=20)
  
  
  hi<-function(cPRA = cPRA, cutoff = 85){
      res<-NULL
      res<-if_else(cPRA > cutoff, TRUE, FALSE)
  }
  

  candidatos %>% mutate(HI = hi(cPRA = cPRA, cutoff = 85)) %>% arrange(desc(HI))
  
  
  candET<-candidatos %>% left_join(hlaAet %>% select(A,freq), by = c("A1" = "A")) %>% rename(a1=freq) %>% 
    left_join(hlaAet %>% select(A,freq), by = c("A2" = "A")) %>% rename(a2=freq) %>% 
    left_join(hlaBet %>% select(B,freq), by = c("B1" = "B")) %>% rename(b1=freq) %>%
    left_join(hlaBet %>% select(B,freq), by = c("B2" = "B")) %>% rename(b2=freq) %>%
    left_join(hlaDRet %>% select(DR,freq), by = c("DR1" = "DR")) %>% rename(dr1=freq) %>%
    left_join(hlaDRet %>% select(DR,freq), by = c("DR2" = "DR")) %>% rename(dr2=freq) %>% 
    left_join(abo, by = c("bg" = "abo"))%>% rename(abo=freq)
  
  candET$MMP2 <- with(candET,
                    (((2*(a1+a2)*(1 - a1 - a2)) - a1^2 - a2^2 + SallA) /
                       ((a1+a2)^2))
                    + (((2*(b1+b2)*(1 - b1 - b2)) - b1^2 - b2^2 + SallB) /
                         ((b1+b2)^2))
                    + (((2*(dr1+dr2)*(1 - dr1 - dr2) ) - dr1^2 - dr2^2 + SallDR) /
                         ((dr1+dr2)^2))
  )
  
  # compute MMP0 and add it to the data file
  candET$MMP0 <- with(candET,
                    (a1+a2)^2 * (b1+b2)^2 * (dr1+dr2)^2)
  
  # compute MMP1 and add it to the data file
  candET$MMP1 <- with(candET,
                    MMP0 * MMP2)
  
  # compute MMP and add it to the data file
  candET$MMP<-with(candET,
                 100 * (1-(abo * (1-cPRA/100) * (MMP0 + MMP1)))^1000
  )
  
  write_csv2(candET, "files/candidates.et.csv")
  
  
  summary(ex.candidates.pt$dialysis)
  
  
  lima_order(iso = TRUE, # isogroup compatibility
             dABO = "A", # donor's blood group
             dA = c("1","2"), dB = c("15","44"), dDR = c("1","4"), # donor's HLA typing'
             dage = 66, # donor's age
             cdata = ex.candidates, # data file with candidates
             df.abs = ex.abs, # data frame with candidates' HLA antibodies
             n = 2)

identical(
  ex.candidates %>% rowwise() %>% mutate(x =lima_sp(dage = 50, cage = age))  %>% select(ID,age,x) %>% ungroup(),
  ex.candidates %>% mutate(x =lima_sp(dage = 50, cage = age))  %>% select(ID,age,x))

  lima_sp(dage = 50, cage = 50)
  

age<-seq(1:80)                           
l1<-1200*cos(age/18) + 2300
l2<-750*cos(age/18) + 1500
l3<-400*sin(age/50)

df<-data.frame(age,l1,l2,l3)
ggplot(df,aes(age,l1, color = "red")) + geom_line() +
  geom_line(aes(y=l2, color = "green")) + 
  geom_line(aes(y=l3, color = "blue")) +
  scale_color_discrete(name = "L series", labels = c("l1","l2","l3"))


library(DT)
dt<-data.frame(R1=c(1000,700,350,0),
               R2=c(700,1000,500,350),
               R3=c(350,500,1000,700),
               R4=c(0,350,700,1000))
rownames(dt)<-c("D1","D2","D3","D4")
datatable(dt*2)
