## data description

library(tidyverse)

## candidates demographic
# file example with HLA allele PT frequencies for MMP computation
ex.candidates.pt <- read_csv2("files/candidates.csv")
ex.candidates.pt<-ex.candidates.pt %>% 
  mutate_at(vars(A1,A2,B1,B2,DR1,DR2),as.character)
# file example with HLA allele ETKAS frequencies for MMP computation
ex.candidates.et <- read_csv2("files/candidateset.csv")
ex.candidates.et<-ex.candidates.et %>% 
  mutate_at(vars(A1,A2,B1,B2,DR1,DR2),as.character)
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

## candidates HLA antibodies
# file example
ex.abs <- read_csv2("files/abs.csv")
## columns description
# ID - candidates identification; type integer
# abs - candidates' HLA antibodies; type character (same resolution as defined for donors and candidates typing)

## donors demographic
# file example
ex.donors <- read_csv2("files/donors.csv")
ex.donors<-ex.donors %>% 
  mutate_at(vars(A1,A2,B1,B2,DR1,DR2),as.character)
## columns description
# ID - donors' identification; type integer
# bg - donors' blood group; type character (A, AB, B, O)
# A1, A2, B1, B2, DR1, DR1 - HLA typing; type character (same resolution as defined for candidates and antibodies)
# age - donors' age; type integer

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
