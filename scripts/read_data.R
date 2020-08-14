## data description

library(tidyverse)

## candidates demographic
# file example
ex.candidates <- read_csv2("files/candidates.csv")
ex.candidates<-ex.candidates %>% 
  mutate_at(vars(A1,A2,B1,B2,DR1,DR2),as.character)
## columns description
# ID - candidates identification; type integer
# bg - candidates' blood group; type character (A, AB, B, O)
# A1, A2, B1, B2, DR1, DR1 - HLA typing; type character (same resolution as defined for donors and antibodies)
# age - candidates' age; type integer
# dialysis - number of months on dialysis; type integer
# cPRA - calculated PRA percentage; type numeric (between 0 and 100)

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
hlaA <- read.csv2("files/hlaA.csv") %>% 
  mutate_at(vars(A),as.character)
hlaB <- read.csv2("files/hlaB.csv") %>% 
  mutate_at(vars(B),as.character)
hlaDR <- read.csv2("files/hlaDR.csv") %>% 
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
# compute the sum of squared frequencies for each loci
SallA <-sum(hlaA$freq^2)
SallB <-sum(hlaB$freq^2)
SallDR <-sum(hlaDR$freq^2)
