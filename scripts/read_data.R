## data description

library(tidyverse)

## candidates demographic
# file example with HLA allele PT frequencies for MMP computation
ex.candidates.pt <- read_csv2("files/candidates.csv")
ex.candidates.pt<-ex.candidates.pt %>% 
  mutate_at(vars(ID, A1,A2,B1,B2,DR1,DR2),as.character)
# file example with HLA allele ETKAS frequencies for MMP computation
ex.candidates.et <- read_csv2("files/candidateset.csv")
ex.candidates.et<-ex.candidates.et %>% 
  mutate_at(vars(ID, A1,A2,B1,B2,DR1,DR2),as.character)
## columns description
# ID - candidates identification; type integer
# bg - candidates' blood group; type character (A, AB, B, O)
# A1, A2, B1, B2, DR1, DR1 - HLA typing; type character (same resolution as defined for donors and antibodies)
# age - candidates' age; type integer
# dialysis - number of months on dialysis; type integer
# cPRA - calculated PRA percentage; type numeric (between 0 and 100)
# a1 - frequency of 1st HLA-A antigen
# a2 - frequency of 2nd HLA-A antigen
# b1 - frequency of 1st HLA-B antigen
# b2 - frequency of 2nd HLA-B antigen
# dr1 - frequency of 1st HLA-DR antigen
# dr2 - frequency of 2nd HLA-DR antigen
# abo - frequency of ABO blood group
# MMP's - ETKAS' Mismatch Probability (MMP)
# 
# file example for UK candidates
ex.candidates.uk <- read_csv2("files/candidates.csv")
ex.candidates.uk<-ex.candidates.uk %>% 
  mutate_at(vars(ID, A1,A2,B1,B2,DR1,DR2),as.character) %>% select(!(a1:MMP))

# create random column with matchability score 
set.seed(1)
ex.candidates.uk$MS<-sample(1:10, 
                            size = 500, 
                            replace = T, 
                            prob = c(0.12,0.12,0.11,0.11,0.11,0.1,0.1,0.1,0.1,0.03))
# compute column Tier with options A (MS = 10 OR cPRA = 100% OR dialysis >= 84) or B (others)
set.seed(1)
ex.candidates.uk<-ex.candidates.uk %>% 
  mutate(Tier = ifelse(MS == 10 | cPRA == 100 | dialysis >= 84, "A","B"),
         diabetic = sample(0:1,500,replace = T,prob = c(0.9,0.1)), # imput 10% of diabetic patients
         atregist = sample(0:1,500,replace = T,prob = c(0.95,0.05)), # imput 5% of patients preemptive registation
         rri = exp((0.016 * (age -75)) +
                     (0.361 * atregist) +
                     (0.033 * ((dialysis*30 -950) / 365.25)) +
                     (0.252 * diabetic)
                     ), # compute value for Recipient Risk Index
         RRI = case_when(rri <= 0.74 ~ 'R1',
                         rri <= 0.94 ~ 'R2',
                         rri <= 1.2 ~ 'R3',
                         TRUE ~ 'R4') # compute Recipient Risk Index from 'rri'
         )
## candidates HLA antibodies
# file example
ex.abs <- read_csv2("files/abs.csv")
ex.abs<-ex.abs %>% 
  mutate_at(vars(ID),as.character)
## columns description
# ID - candidates identification; type integer
# abs - candidates' HLA antibodies; type character (same resolution as defined for donors and candidates typing)

## donors demographic
# file example
ex.donors <- read_csv2("files/donors.csv")
ex.donors<-ex.donors %>% 
  mutate_at(vars(ID, A1,A2,B1,B2,DR1,DR2),as.character)
## columns description
# ID - donors' identification; type integer
# bg - donors' blood group; type character (A, AB, B, O)
# A1, A2, B1, B2, DR1, DR1 - HLA typing; type character (same resolution as defined for candidates and antibodies)
# age - donors' age; type integer

# UK donors example file
library(msm)
set.seed(1)
ex.donors.uk <-ex.donors %>% mutate(sex = sample(0:1, 70, replace = T, prob = c(0.6, 0.4)), # imput donors' sex (0- male; 1 - female)
                                    height = ifelse(sex == 1,
                                                    round(rtnorm(n = 70, mean = 165, sd = 10, lower=145, upper=180)),
                                                    round(rtnorm(n = 70, mean = 175, sd = 10, lower=150, upper=199))), # impute donor's height by sex
                                    ht = ifelse(sex == 1,
                                                sample(0:1, 70, replace = T, prob = c(0.85, 0.15)),
                                                sample(0:1, 70, replace = T, prob = c(0.80, 0.20))), # imput history of hipertension by sex
                                    cmv = sample(0:1, 70, replace = T, prob = c(0.2, 0.8)), # imput 1 when CMV+
                                    gfr = round(rtnorm(n = 70, mean = 100, sd = 10, lower=80, upper=120)), # imput values for eGFR
                                    hospital = round(rtnorm(n = 70, mean = 30, sd = 10, lower=0, upper=90)), # days in hospital
                                    dri = exp((0.023 * (age-50)) +
                                                (-0.152 * (height - 170) / 10) +
                                                (0.149 * ht) +
                                                (-0.184 * sex) +
                                                (0.190 * cmv) +
                                                (-0.023 * (gfr-90)/10) +
                                                (0.015 * hospital)), # compute value for Donor Risk Index
                                    DRI = case_when(dri <=0.79 ~ 'D1',
                                                    dri <= 1.12 ~ 'D2',
                                                    dri <= 1.5 ~ 'D3',
                                                    TRUE ~ 'D4')) # compute 4 levels Donor Risk Index from dri values

## HLA frequencies from portuguese CEDACE donors (Lima, 2013)
hlaApt <- read.csv2("files/hlaA.csv") %>% 
  mutate_at(vars(A),as.character)
hlaBpt <- read.csv2("files/hlaB.csv") %>% 
  mutate_at(vars(B),as.character)
hlaDRpt <- read.csv2("files/hlaDR.csv") %>% 
  mutate_at(vars(DR),as.character)

## HLA frequencies from EuroTransplant (ETKAS and SP chapter 4 kidney)
hlaAet <- read.csv2("files/hlaAet.csv") %>% 
  mutate_at(vars(A),as.character)
hlaBet <- read.csv2("files/hlaBet.csv") %>% 
  mutate_at(vars(B),as.character)
hlaDRet <- read.csv2("files/hlaDRet.csv") %>% 
  mutate_at(vars(DR),as.character)
## columns description
# A - HLA-A alleles; B - HLA-B alleles; DR - HLA-DR alleles;
# n - allele countings; type integer
# freq - allele relative frequencies; type numeric (between 0 and 100)

## ABO blood group frequencies from portuguese blood donors (Duran et al, 2007)
abo <- read.csv2("files/abo.csv") %>% 
  mutate_at(vars(abo),as.character)
## columns description
# abo - blood groups; type character (A, AB, B, O) 
# freq - ABO relative frequencies; type numeric (between 0 and 1)

## compute MMP from ETKAS
# compute the sum of squared frequencies for each loci with PT frequencies
SallApt <-sum(hlaApt$freq^2)
SallBpt <-sum(hlaBpt$freq^2)
SallDRpt <-sum(hlaDRpt$freq^2)
# compute the sum of squared frequencies for each loci with ET frequencies
SallAet <-sum((hlaAet %>% drop_na() %>% .$freq)^2)
SallBet <-sum((hlaBet %>% drop_na() %>% .$freq)^2)
SallDRet <-sum((hlaDRet %>% drop_na() %>% .$freq)^2)
