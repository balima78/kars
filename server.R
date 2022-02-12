# server functions' file

source("scripts/read_data.R")
source("scripts/compat_fxs.R")
source("scripts/PT_fxs.R")
source("scripts/ET_fxs.R")
source("scripts/Lima_fxs.R")
source("scripts/UK_fxs.R")

library(DT)
library(tidyverse)
library(openxlsx)
library(gtsummary)

function(input, output, session) {
  
  output$ex.cands<- renderDataTable({
    
    datatable(ex.candidates.pt %>%
                select(ID, bg,A1,A2,B1,B2,DR1,DR2,age,dialysis,cPRA),
              rownames = FALSE)
  })
  output$ex.abs<- renderDataTable({
    datatable(ex.abs,
              rownames = FALSE)
  })
  output$ex.donors<- renderDataTable({
    datatable(ex.donors,
              rownames = FALSE)
  })
  
  # reactive to the input UK files
  ukf<-reactive(input$ukfiles)
  
  # read candidates' file
  datasetCands <- reactive({
    
    file_cands <- input$file_cand
    
    if (is.null(file_cands))
      return(NULL)
    
    if (input$fileSepDF == 1) {
      data<-read.csv(file_cands$datapath)
    } else if (input$fileSepDF == 2) {
      read.delim(file_cands$datapath)
    } else if (input$fileSepDF == 3) {
      data<-read.csv2(file_cands$datapath)
    } else {data<-read.table(file_cands$datapath)}
    
     
    validate(
      if(ukf() == 1){need(identical(colnames(data),c("ID","bg","A1","A2","B1","B2","DR1","DR2","age","dialysis","cPRA", "Tier", "MS", "RRI")), 
                             "Candidates column names are not the necessary for UK algorithm!")} else {need(identical(colnames(data),c("ID","bg","A1","A2","B1","B2","DR1","DR2","age","dialysis","cPRA")), 
           "Candidates column names are not identical to example data!")}
      )
    
    data %>% 
      mutate_at(vars(ID, A1,A2,B1,B2,DR1,DR2),as.character) 
    
    
  })
  
  # read donors' file 
  datasetDonors <- reactive({
    
    file_donors <- input$file_donor
    
    if (is.null(file_donors))
      return(NULL)
    
    if (input$fileSepDF == 1) {
      data<-read.csv(file_donors$datapath)
    } else if (input$fileSepDF == 2) {
      read.delim(file_donors$datapath)
    } else if (input$fileSepDF == 3) {
      data<-read.csv2(file_donors$datapath)
    } else {data<-read.table(file_donors$datapath)}
    
    
    validate(
      if(ukf() == 1){
        need(identical(colnames(data),c(colnames(ex.donors),"DRI")), 
             "Donors column names are not the necessary for UK algorithm!")
      } else {
      need(identical(colnames(data),colnames(ex.donors)), 
           "Donors column names are not identical to example data!")}
    )
    
    data %>% 
      mutate_at(vars(ID,A1,A2,B1,B2,DR1,DR2),as.character) %>% 
      mutate_at(vars(age), as.numeric)
    
  })
  
  # read candidates' antibodies' file
  datasetAbs <- reactive({
    
    file_abss <- input$file_abs
    
    if (is.null(file_abss))
      return(NULL)
    
    if (input$fileSepDF == 1) {
      data<-read.csv(file_abss$datapath)
    } else if (input$fileSepDF == 2) {
      read.delim(file_abss$datapath)
    } else if (input$fileSepDF == 3) {
      data<-read.csv2(file_abss$datapath)
    } else {data<-read.table(file_abss$datapath)}
    
    
    validate(
      need(identical(colnames(data),colnames(ex.abs)), 
           "HLA antibodies column names are not identical to example data!")
    )
    
    data %>% 
      mutate_at(vars(ID), as.character)
    
  })
  
  output$sel.cands<- renderDataTable({
    datasetCands() %>% datatable(rownames = FALSE)
  })
  
  output$sel.abs<- renderDataTable({
    datasetAbs() %>% datatable(rownames = FALSE)
  })
  
  output$sel.donors<- renderDataTable({
    datasetDonors() %>% datatable(rownames = FALSE)
  })
  

  ############################
  ### PT algorithm
  ############################
 
  ## for 10 best candidates selected according to unique donor 
  
  output$res1 <- renderDataTable({
    
    if (input$dataInput == 1) {candidates<-ex.candidates.pt %>% 
      select(ID, bg,A1,A2,B1,B2,DR1,DR2,age,dialysis,cPRA)} else {candidates<-datasetCands() %>% 
        select(ID, bg,A1,A2,B1,B2,DR1,DR2,age,dialysis,cPRA)}
    if (input$dataInput == 1) {abs.d<-ex.abs} else {abs.d<-datasetAbs()}
    
    validate(
      need(candidates != "", "Please select a candidates data set!")
    )
    
    validate(
      need(abs.d != "", "Please select candidates' HLA antibodies data set!")
    )
    
      dt<-pt_points(iso = input$iso, # isogroup compatibility
                  dABO = input$dabo, # donor's blood group
                  dA = c(input$a1,input$a2),
                  dB = c(input$b1,input$b2),
                  dDR = c(input$dr1,input$dr2),
                  dage = input$dage, # donor's age
                  cdata = candidates, # data file with candidates
                  pra80 = as.numeric(input$pra8), # points for a PRA equal or higher than 80%
                  pra50 = as.numeric(input$pra5), # points for a PRA equal or higher than 50%
                  month = input$dialysis, # points for each month on dialysis
                  points = input$age_dif, # points for age difference in PT punctuation table
                  itemA = as.numeric(input$a), # points for A) on PT points table
                  itemB = as.numeric(input$b), # points for B) on PT points table
                  itemC = as.numeric(input$c), # points for C) on PT points table
                  itemD = as.numeric(input$d), # points for D) on PT points table
                  itemE = as.numeric(input$e), # points for E) on PT points table
                  df.abs = abs.d, # candidates' HLA antibodies
                  n = 10)
      
      dt <- dt %>%
        #rowwise() %>% 
        mutate(txScore = txscore(ageR = age
                                 , timeD = dialysis
                                 , ageD = donor_age
                                 , mmHLA_A = mmA
                                 , mmHLA_B = mmB
                                 , mmHLA_DR = mmDR)$prob5y
               ) #%>% ungroup()

    datatable(dt, options = list(pageLength = 5, dom = 'tip'))
  })

  
  #### to reset PT sidebarpanel
  observeEvent(input$reset_inputPT, {
    shinyjs::reset("side-panelPT")
  })
  
  N <- 2
  
  compute_resm <- reactiveVal()
 
  observeEvent(input$Go, {
    
    compute_resm(NULL)
    
    withProgress(message = 'Calculation in progress, be patient!', {
      for(i in 1:N){
        # Long Running Task
        Sys.sleep(1)
        # Update progress
        incProgress(1/N)
        }
      
      iso = input$iso
      pra80 = as.numeric(input$pra8) # points for a PRA equal or higher than 80%
      pra50 = as.numeric(input$pra5) # points for a PRA equal or higher than 50%
      month = input$dialysis # points for each month on dialysis
      points = input$age_dif # points for age difference in PT punctuation table
      itemA = as.numeric(input$a) # points for A) on PT points table
      itemB = as.numeric(input$b) # points for B) on PT points table
      itemC = as.numeric(input$c) # points for C) on PT points table
      itemD = as.numeric(input$d) # points for D) on PT points table
      itemE = as.numeric(input$e) # points for E) on PT points table
   
    if (input$dataInput == 1) {candidates<-ex.candidates.pt} else {candidates<-datasetCands() %>% 
      select(ID, bg,A1,A2,B1,B2,DR1,DR2,age,dialysis,cPRA)}
    if (input$dataInput == 1) {abs.d<-ex.abs} else {abs.d<-datasetAbs()}
    if (input$dataInput == 1) {donors<-ex.donors} else {donors<-datasetDonors()}
    
    validate(
      need(candidates != "", "Please select a candidates data set!")
    )
    
    validate(
      need(abs.d != "", "Please select candidates' HLA antibodies data set!")
    )
    
    validate(
      need(donors != "", "Please select donors' data set!")
    )

    # add a column to candidates' file to update respective donors
    candidatesN<-candidates %>% mutate(donor = 'X') # donor column changed to character
    
    # create a list with the same length of the number of donors
    res <- vector("list", length = dim(donors)[1])
    
    # now the for loop
    for (i in 1:dim(donors)[1]){
      candid<-candidatesN %>% filter(donor == 'X')
      
      res[[i]]<-pt_points(iso = iso, # ABO compatibility as selected 
                          dABO = donors$bg[i], # donor's blood group
                          dA = c(donors$A1[i],donors$A2[i]), 
                          dB = c(donors$B1[i],donors$B2[i]), 
                          dDR = c(donors$DR1[i],donors$DR2[i]),
                          dage = donors$age[i], # donor's age
                          cdata = candid, # data file with candidates
                          pra80 = pra80, # points for a PRA equal or higher than 80%
                          pra50 = pra50, # points for a PRA equal or higher than 50%
                          month = month, # points for each month on dialysis
                          points = points, # points for age difference in PT punctuation table
                          itemA = itemA, # points for A) on PT points table
                          itemB = itemB, # points for B) on PT points table
                          itemC = itemC, # points for C) on PT points table
                          itemD = itemD, # points for D) on PT points table
                          itemE = itemE, # points for E) on PT points table
                          df.abs = abs.d) %>% 
        mutate(donor = donors$ID[i])
      
      candidatesN<-candidatesN %>% 
        mutate(donor = case_when(ID %in% res[[i]]$ID ~ donors$ID[i],
                                 TRUE ~ donor))
      
    }
    
    ## bind the results in the list and compute txScore
    dt <- do.call(rbind, res) %>% 
      mutate(txScore = txscore(ageR = age
                               , timeD = dialysis
                               , ageD = donor_age
                               , mmHLA_A = mmA
                               , mmHLA_B = mmB
                               , mmHLA_DR = mmDR)$prob5y
      )
    # add to reactiveval
    compute_resm(dt)
    })
    
    
  })

  output$resm <- renderDataTable({
    compute_resm()
  })

  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("PT_results", ".csv", sep = "")
    },
    content = function(file) {
      write.csv2(compute_resm(), file, row.names = FALSE, fileEncoding="latin1")
    }
  )
  
  ## Resume dataset results from PT algorithm
  output$resumePT <-
    render_gt({
      
      validate(
        need(compute_resm() != "", "Results will be presented after the run!")
      )
      
      tabsum<-compute_resm() %>% 
        select(bg, age, dialysis, cPRA, HI, mmHLA, txScore) %>% 
        rename(`Blood group` = bg,
               `receptores' age (years)` = age,
               `time on dialysis (months)` = dialysis,
               `Hiper Immunized` = HI,
               `HLA miss matchs` = mmHLA,
               TxScore = txScore)
      
      tbl_summary(tabsum) %>% as_gt()
    })

  
  ############################
  ### ET algorithm
  ############################
  
  ## compute MMP for uploaded candidates dataset
  # take uploaded dataset and compute MMP
  datasetCandsET<-reactive({
    
    # select HLA frequencies from PT or ET
    if (input$hlafreqs == 1) {hlaA<-hlaApt} else {hlaA<-hlaAet}
    if (input$hlafreqs == 1) {hlaB<-hlaBpt} else {hlaB<-hlaBet}
    if (input$hlafreqs == 1) {hlaDR<-hlaDRpt} else {hlaDR<-hlaDRet}
    
    #if (input$hlafreqs == 1) {abo<-abopt} else {abo<-aboet}
    
    if (input$hlafreqs == 1) {SallA<-SallApt} else {SallA<-SallAet}
    if (input$hlafreqs == 1) {SallB<-SallBpt} else {SallB<-SallBet}
    if (input$hlafreqs == 1) {SallDR<-SallDRpt} else {SallDR<-SallDRet}
    
    # join allele frequencies to candidates dataset
    data<-datasetCands() %>% left_join(hlaA %>% select(A,freq), by = c("A1" = "A"))
    data<- data %>% rename(a1=freq)
    data<-data %>% left_join(hlaA %>% select(A,freq), by = c("A2" = "A"))
    data<- data %>% rename(a2=freq)
    data<-data %>% left_join(hlaB %>% select(B,freq), by = c("B1" = "B"))
    data<- data %>% rename(b1=freq)
    data<-data %>% left_join(hlaB %>% select(B,freq), by = c("B2" = "B"))
    data<- data %>% rename(b2=freq)
    data<-data %>% left_join(hlaDR %>% select(DR,freq), by = c("DR1" = "DR"))
    data<- data %>% rename(dr1=freq)
    data<-data %>% left_join(hlaDR %>% select(DR,freq), by = c("DR2" = "DR"))
    data<- data %>% rename(dr2=freq)
    data<-data %>% left_join(abo, by = c("bg" = "abo"))
    data<- data %>% rename(abo=freq)

  # compute MMP2 and add it to the data file
  data$MMP2 <- with(data,
                         (((2*(a1+a2)*(1 - a1 - a2)) - a1^2 - a2^2 + SallA) /
                            ((a1+a2)^2))
                         + (((2*(b1+b2)*(1 - b1 - b2)) - b1^2 - b2^2 + SallB) /
                              ((b1+b2)^2))
                         + (((2*(dr1+dr2)*(1 - dr1 - dr2) ) - dr1^2 - dr2^2 + SallDR) /
                              ((dr1+dr2)^2))
  )

  # compute MMP0 and add it to the data file
  data$MMP0 <- with(data,
                         (a1+a2)^2 * (b1+b2)^2 * (dr1+dr2)^2)

  # compute MMP1 and add it to the data file
  data$MMP1 <- with(data,
                         MMP0 * MMP2)

  # compute MMP and add it to the data file
  data$MMP<-with(data,
                      100 * (1-(abo * (1-cPRA/100) * (MMP0 + MMP1)))^1000
  )
  
  data
  })

 
  ## for 10 first candidates selected according to unique donor 
  output$res1ET <- renderDataTable({
    
    if (input$hlafreqs == 1) {ex.candidates<-ex.candidates.pt} else {ex.candidates<-ex.candidates.et}

    if (input$dataInput == 1) {candidates<-ex.candidates} else {candidates<-datasetCandsET()}
    if (input$dataInput == 1) {abs.d<-ex.abs} else {abs.d<-datasetAbs()}

    validate(
      need(candidates != "", "Please select a candidates data set!")
    )

    validate(
      need(abs.d != "", "Please select candidates' HLA antibodies data set!")
    )

    dt<-et_points(iso = input$isoET, # isogroup compatibility
                  dABO = input$daboET, # donor's blood group
                  dA = c(input$a1ET,input$a2ET),
                  dB = c(input$b1ET,input$b2ET),
                  dDR = c(input$dr1ET,input$dr2ET),
                  dage = input$dageET, # donor's age
                  cdata = candidates, # data file with candidates
                  month = as.numeric(input$tdET), # points for each month on dialysis
                  mm0 = as.numeric(input$mm0), # points for 0 HLA mm on ETKAS points table
                  mm1 = as.numeric(input$mm1), # points for 1 HLA mm on ET points table
                  mm2 = as.numeric(input$mm2), # points for 2 HLA mm on ET points table
                  mm3 = as.numeric(input$mm3), # points for 3 HLA mm on ET points table
                  mm4 = as.numeric(input$mm4), # points for 4 HLA mm on ET points table
                  mm5 = as.numeric(input$mm5), # points for 5 HLA mm on ET points table
                  mm6 = as.numeric(input$mm6), # points for 6 HLA mm on ET points table
                  df.abs = abs.d, # candidates' HLA antibodies
                  n = 10)

    dt <- dt %>%
      mutate(txScore = txscore(ageR = age
                               , timeD = dialysis
                               , ageD = donor_age
                               , mmHLA_A = mmA
                               , mmHLA_B = mmB
                               , mmHLA_DR = mmDR)$prob5y
      )
    
    datatable(dt, options = list(pageLength = 5, dom = 'tip'))
  }) 
  
  observeEvent(input$reset_inputET, {
    shinyjs::reset("side-panelET")
  })
 
  ## compute nultiple results for ET algorithm
  compute_resmET <- reactiveVal()
  
  observeEvent(input$GoET, {
    
    compute_resmET(NULL)

    withProgress(message = 'Calculation in progress, be patient!', {
      for(i in 1:N){
        # Long Running Task
        Sys.sleep(1)
        # Update progress
        incProgress(1/N)
      }
      
      iso = input$isoET
      month = as.numeric(input$tdET) # points for each month on dialysis
      mm0 = as.numeric(input$mm0) # points for 0 HLA mm on ETKAS points table
      mm1 = as.numeric(input$mm1) # points for 1 HLA mm on ET points table
      mm2 = as.numeric(input$mm2) # points for 2 HLA mm on ET points table
      mm3 = as.numeric(input$mm3) # points for 3 HLA mm on ET points table
      mm4 = as.numeric(input$mm4) # points for 4 HLA mm on ET points table
      mm5 = as.numeric(input$mm5) # points for 5 HLA mm on ET points table
      mm6 = as.numeric(input$mm6) # points for 6 HLA mm on ET points table
     
      if (input$hlafreqs == 1) {ex.candidates<-ex.candidates.pt} else {ex.candidates<-ex.candidates.et}
      if (input$dataInput == 1) {candidates<-ex.candidates} else {candidates<-datasetCandsET()}
      
      if (input$dataInput == 1) {abs.d<-ex.abs} else {abs.d<-datasetAbs()}
      if (input$dataInput == 1) {donors<-ex.donors} else {donors<-datasetDonors()}
      
      validate(
        need(candidates != "", "Please select a candidates data set!")
      )
      
      validate(
        need(abs.d != "", "Please select candidates' HLA antibodies data set!")
      )
      
      validate(
        need(donors != "", "Please select donors' data set!")
      )
      
      # add a column to candidates' file to update respective donors
      candidatesN<-candidates %>% mutate(donor = 'X')
      
      # create a list with the same length of the number of donors
      res <- vector("list", length = dim(donors)[1])
      
      # now the for loop
      for (i in 1:dim(donors)[1]){
        candid<-candidatesN %>% filter(donor == 'X')
        
        res[[i]]<-et_points(iso = iso, # isogroup compatibility
                            dABO = donors$bg[i], # donor's blood group
                            dA = c(donors$A1[i],donors$A2[i]), 
                            dB = c(donors$B1[i],donors$B2[i]), 
                            dDR = c(donors$DR1[i],donors$DR2[i]),
                            dage = donors$age[i], # donor's age
                            cdata = candid, # data file with candidates
                            month = month, # points for each month on dialysis
                            mm0 = mm0, # points for 0 HLA mm on ETKAS points table
                            mm1 = mm1, # points for 1 HLA mm on ET points table
                            mm2 = mm2, # points for 2 HLA mm on ET points table
                            mm3 = mm3, # points for 3 HLA mm on ET points table
                            mm4 = mm4, # points for 4 HLA mm on ET points table
                            mm5 = mm5, # points for 5 HLA mm on ET points table
                            mm6 = mm6, # points for 6 HLA mm on ET points table
                            df.abs = abs.d) %>% 
          mutate(donor = donors$ID[i])
        
        candidatesN<-candidatesN %>% 
          mutate(donor = case_when(ID %in% res[[i]]$ID ~ donors$ID[i],
                                   TRUE ~ donor))
        
      }
      
      ## bind the results in the list and compute txScore
      dt <- do.call(rbind, res) %>% 
        mutate(txScore = txscore(ageR = age
                                 , timeD = dialysis
                                 , ageD = donor_age
                                 , mmHLA_A = mmA
                                 , mmHLA_B = mmB
                                 , mmHLA_DR = mmDR)$prob5y
        )
      # add to reactiveval
      compute_resmET(dt)
      
      })
    
  })
  
  output$resmET <- renderDataTable({
    compute_resmET()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadDataET <- downloadHandler(
    filename = function() {
      paste("ET_results", ".csv", sep = "")
    },
    content = function(file) {
      write.csv2(compute_resmET(), file, row.names = FALSE, fileEncoding="latin1")
    }
  )
  
  
  ## Resume dataset results from ET algorithm
  output$resumeET <-
    render_gt({
      
      validate(
        need(compute_resmET() != "", "Results will be presented after the run!")
      )
      
      tabsum<-compute_resmET() %>% 
        select(bg, age, dialysis, cPRA, HI, mmHLA, txScore) %>% 
        rename(`Blood group` = bg,
               `receptores' age (years)` = age,
               `time on dialysis (months)` = dialysis,
               `Hiper Immunized` = HI,
               `HLA miss matchs` = mmHLA,
               TxScore = txScore)
      
      tbl_summary(tabsum) %>% as_gt()
    })
  
  
  ############################
  ### Lima algorithm
  ############################
  
  #### to reset LIMA sidebarpanel
  observeEvent(input$reset_inputLIMA, {
    shinyjs::reset("side-panelLima")
  })
  
  
  output$res1LIMA <- renderDataTable({
    
    if (input$dataInput == 1) {candidates<-ex.candidates.pt %>% 
      select(ID, bg,A1,A2,B1,B2,DR1,DR2,age,dialysis,cPRA)} else {candidates<-datasetCands() %>% 
        select(ID, bg,A1,A2,B1,B2,DR1,DR2,age,dialysis,cPRA)}
    if (input$dataInput == 1) {abs.d<-ex.abs} else {abs.d<-datasetAbs()}
    
    validate(
      need(candidates != "", "Please select a candidates data set!")
    )
    
    validate(
      need(abs.d != "", "Please select candidates' HLA antibodies data set!")
    )
    
    candidates<-candidates %>% mutate(color = case_when(cPRA >= 85 | dialysis >= input$td3q ~ "orange",
                                                        cPRA >= 50 | dialysis >= input$td2q ~ "yellow",
                                                        TRUE ~ "green"),
                                      color = fct_relevel(color,"orange","yellow","green")
                                      )
    
    dt<-lima_order(iso = input$isoLIMA, # isogroup compatibility
                   dABO = input$daboLIMA, # donor's blood group
                   dA = c(input$a1LIMA,input$a2LIMA),
                   dB = c(input$b1LIMA,input$b2LIMA),
                   dDR = c(input$dr1LIMA,input$dr2LIMA),
                   dage = input$dageLIMA, # donor's age
                   cdata = candidates, # data file with candidates
                   df.abs = abs.d, # candidates' HLA antibodies
                   n = 10)
    
    dt <- dt %>%
      mutate(txScore = txscore(ageR = age
                               , timeD = dialysis
                               , ageD = donor_age
                               , mmHLA_A = mmA
                               , mmHLA_B = mmB
                               , mmHLA_DR = mmDR)$prob5y
      )
    
    datatable(dt, options = list(pageLength = 5, dom = 'tip'))
    
    })
  
  ## compute multiple results for LIMA algorithm
  compute_resmLIMA <- reactiveVal()
  
  observeEvent(input$GoLIMA, {
    
    compute_resmLIMA(NULL)
    
    withProgress(message = 'Calculation in progress, be patient!', {
      for(i in 1:N){
        # Long Running Task
        Sys.sleep(1)
        # Update progress
        incProgress(1/N)
      }
      
      iso = input$isoLIMA
      
      if (input$dataInput == 1) {candidates<-ex.candidates.pt %>% 
        select(ID, bg,A1,A2,B1,B2,DR1,DR2,age,dialysis,cPRA)} else {candidates<-datasetCands() %>% 
          select(ID, bg,A1,A2,B1,B2,DR1,DR2,age,dialysis,cPRA)}
      if (input$dataInput == 1) {abs.d<-ex.abs} else {abs.d<-datasetAbs()}
      if (input$dataInput == 1) {donors<-ex.donors} else {donors<-datasetDonors()}
      
      validate(
        need(candidates != "", "Please select a candidates data set!")
      )
      
      validate(
        need(abs.d != "", "Please select candidates' HLA antibodies data set!")
      )
      
      validate(
        need(donors != "", "Please select donors' data set!")
      )
      
      # add colors column
      candidates<-candidates %>% mutate(color = case_when(cPRA >= 85 | dialysis >= input$td3q ~ "orange",
                                                          cPRA >= 50 | dialysis >= input$td2q ~ "yellow",
                                                          TRUE ~ "green"),
                                        color = fct_relevel(color,"orange","yellow","green")
                                        )
      # add a column to candidates' file to update respective donors
      candidatesN<-candidates %>% mutate(donor = 'X')
      
      # create a list with the same length of the number of donors
      res <- vector("list", length = dim(donors)[1])
      
      # now the for loop
      for (i in 1:dim(donors)[1]){
        candid<-candidatesN %>% filter(donor == 'X')
        
        res[[i]]<-lima_order(iso = iso, # isogroup compatibility
                             dABO = donors$bg[i], # donor's blood group
                             dA = c(donors$A1[i],donors$A2[i]), 
                             dB = c(donors$B1[i],donors$B2[i]), 
                             dDR = c(donors$DR1[i],donors$DR2[i]), # donor's HLA typing'
                             dage = donors$age[i], # donor's age
                             cdata = candid, # data file with candidates
                             df.abs = abs.d, # data frame with candidates' HLA antibodies
                             n = 2 # slice first n rows
                             ) %>% 
          mutate(donor = donors$ID[i])
        
        candidatesN<-candidatesN %>%
          mutate(donor = case_when(ID %in% res[[i]]$ID ~ donors$ID[i],
                                   TRUE ~ donor))
        
      }
      
      ## bind the results in the list and compute txScore
      dt <- do.call(rbind, res) %>% 
        mutate(txScore = txscore(ageR = age
                                 , timeD = dialysis
                                 , ageD = donor_age
                                 , mmHLA_A = mmA
                                 , mmHLA_B = mmB
                                 , mmHLA_DR = mmDR)$prob5y
        )
      # add to reactiveval
      compute_resmLIMA(dt)

    })
    })
  
  output$resmLIMA <- renderDataTable({
    compute_resmLIMA()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadDataLIMA <- downloadHandler(
    filename = function() {
      paste("LIMA_results", ".csv", sep = "")
    },
    content = function(file) {
      write.csv2(compute_resmLIMA(), file, row.names = FALSE, fileEncoding="latin1")
    }
  )
  
  ## Resume dataset results from LIMA algorithm
  output$resumeLIMA <-
    render_gt({
      
      validate(
        need(compute_resmLIMA() != "", "Results will be presented after the run!")
      )
      
      tabsum<-compute_resmLIMA() %>% 
        select(bg, age, dialysis, cPRA, HI, mmHLA, txScore) %>% 
        rename(`Blood group` = bg,
               `receptores' age (years)` = age,
               `time on dialysis (months)` = dialysis,
               `Hiper Immunized` = HI,
               `HLA miss matchs` = mmHLA,
               TxScore = txScore)
      
      tbl_summary(tabsum) %>% as_gt()
    })
  
  ############################
  ### UK Transplant
  ############################

  # Donor-recipient risk index combinations
  # multiplyer<-reactive({input$multipleUK})
  
  output$tableDRriskUK <- renderDT({
    dt<-data.frame(R1=c(1000,700,350,0),
                   R2=c(700,1000,500,350),
                   R3=c(350,500,1000,700),
                   R4=c(0,350,700,1000))
    rownames(dt)<-c("D1","D2","D3","D4")
    
    datatable(dt * 1, # multiplyer()
              selection = 'single', escape=FALSE,
              options = list(searching = FALSE, dom = 't'))
  })
  
  # Matchability - illustrative plot
  pointsM<-reactive({
    input$mUK * (1 + ((1:9) / input$nUK)^input$oUK)
  })
  output$matchability<-renderPlot({
    ggplot(data.frame(Points = pointsM(), MatchScore = 1:9)) +
      geom_line(aes(MatchScore,Points)) + 
      ggtitle("Points scored illustration")
  })
  
  #### to reset UK sidebarpanel
  observeEvent(input$reset_inputUK, {
    shinyjs::reset("side-panelUK")
  })
  
  
  ############################ UK algorithm ###################
  
  ## compute DRI for one donor
  driv<-reactive({
    exp(0.023 * (input$dageUK-50) +
          -0.152 * ((input$dheightUK - 170) / 10) +
          0.149 * ifelse(input$dhtUK == 'Yes', 1, 0) +
          -0.184 * ifelse(input$dsexUK == 'Female', 1, 0) +
          0.19 * ifelse(input$dcmvUK == 'Yes', 1, 0) +
          -0.023 * (input$dgfrUK-90)/10 +
          0.015 * input$dhospUK
    )
 
  })
  
  output$dri<-renderText({
   paste("DRI is:", round(driv(),2), "; the donor belong to", 
         ifelse(driv() <= 0.79, "D1",
                ifelse(driv() <= 1.12,"D2",
                       ifelse(driv() <= 1.5,"D3","D4"))))
    })
  
  
  output$res1UK <- renderDataTable({
    
    if (input$dataInput == 1) {candidates<-ex.candidates.uk} else {candidates<-datasetCands()%>% 
      select(ID, bg,A1,A2,B1,B2,DR1,DR2,age,dialysis,cPRA, Tier, MS, RRI)}
    if (input$dataInput == 1) {abs.d<-ex.abs} else {abs.d<-datasetAbs()}
    
    validate(
      need(candidates != "", "Please select a candidates data set!")
    )
    
    validate(
      need(abs.d != "", "Please select candidates' HLA antibodies data set!")
    )
    
    dt<-uk_points(DRI = ifelse(driv() <= 0.79, "D1",
                               ifelse(driv() <= 1.12,"D2",
                                      ifelse(driv() <= 1.5,"D3","D4"))), # Donor RisK Index group
                  dA = c(input$a1UK,input$a2UK), # donor's HLA typing'
                  dB = c(input$b1UK,input$b2UK),
                  dDR = c(input$dr1UK,input$dr2UK),
                  dABO = input$daboUK, # donors' blood group
                  dage = input$dageUK, # donors' age
                  cdata = candidates, # data file with candidates
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
                  ptsDial = input$tdUK,
                  a1 = input$aa1UK, # value on HLA match and age combined formula
                  a2 = input$aa2UK, # value on HLA match and age combined formula
                  b1 = input$bb1UK, # value on HLA match and age combined formula
                  b2 = input$bb2UK, # value on HLA match and age combined formula
                  b3 = input$bb3UK, # value on HLA match and age combined formula
                  m = input$mUK, # matchability formula
                  nn = input$nUK, # matchability formula
                  o = input$oUK, # matchability formula
                  mm1 = as.numeric(input$mm1UK), # substrating points for 1 mm
                  mm23 = as.numeric(input$mm23UK), # substrating points for 2-3 mm 
                  mm46 = as.numeric(input$mm46UK), # substrating points for 4-6 mm
                  pts = input$bloodUK, # substrating points for B blood group
                  df.abs = abs.d, # data frame with candidates' HLA antibodies
                  n = 10 # slice first n rows
    )

    dt <- dt %>%
      mutate(txScore = txscore(ageR = age
                               , timeD = dialysis
                               , ageD = donor_age
                               , mmHLA_A = mmA
                               , mmHLA_B = mmB
                               , mmHLA_DR = mmDR)$prob5y
      )
    
    datatable(dt, options = list(pageLength = 5, dom = 'tip'))
    
  })
  
  ## compute nultiple results for UK algorithm
  compute_resmUK <- reactiveVal()
  
  observeEvent(input$GoUK, {
    
    compute_resmUK(NULL)
    
    withProgress(message = 'Calculation in progress, be patient!', {
      for(i in 1:N){
        # Long Running Task
        Sys.sleep(1)
        # Update progress
        incProgress(1/N)
      }
      
      
      if (input$dataInput == 1) {candidates<-ex.candidates.uk %>% 
        select(ID, bg,A1,A2,B1,B2,DR1,DR2,age,dialysis,cPRA,
               Tier, RRI, MS)} else {candidates<-datasetCands()}
      if (input$dataInput == 1) {abs.d<-ex.abs} else {abs.d<-datasetAbs()}
      if (input$dataInput == 1) {donors<-ex.donors.uk %>%
        select(ID, bg, A1, A2, B1, B2, DR1, DR2, age, DRI)} else {donors<-datasetDonors()}
      
      
      validate(
        need(candidates != "", "Please select a candidates data set!")
      )
      
      validate(
        need(abs.d != "", "Please select candidates' HLA antibodies data set!")
      )
      
      validate(
        need(donors != "", "Please select donors' data set!")
      )
      
      # add a column to candidates' file to update respective donors
      candidatesN<-candidates %>% mutate(donor = 'X')
      
      # create a list with the same length of the number of donors
      res <- vector("list", length = dim(donors)[1])
      
      # now the for loop
      for (i in 1:dim(donors)[1]){
        candid<-candidatesN %>% filter(donor == 'X')
        
        res[[i]]<-uk_points(DRI = donors$DRI[i], # Donor RisK Index group
                            dABO = donors$bg[i], # donor's blood group
                            dA = c(donors$A1[i],donors$A2[i]), 
                            dB = c(donors$B1[i],donors$B2[i]), 
                            dDR = c(donors$DR1[i],donors$DR2[i]), # donor's HLA typing'
                            dage = donors$age[i], # donor's age
                            cdata = candid, # data file with candidates
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
                            ptsDial = input$tdUK,
                            a1 = input$aa1UK, # value on HLA match and age combined formula
                            a2 = input$aa2UK, # value on HLA match and age combined formula
                            b1 = input$bb1UK, # value on HLA match and age combined formula
                            b2 = input$bb2UK, # value on HLA match and age combined formula
                            b3 = input$bb3UK, # value on HLA match and age combined formula
                            m = input$mUK, # matchability formula
                            nn = input$nUK, # matchability formula
                            o = input$oUK, # matchability formula
                            mm1 = as.numeric(input$mm1UK), # substrating points for 1 mm
                            mm23 = as.numeric(input$mm23UK), # substrating points for 2-3 mm 
                            mm46 = as.numeric(input$mm46UK), # substrating points for 4-6 mm
                            pts = input$bloodUK, # substrating points for B blood group
                            df.abs = abs.d, # data frame with candidates' HLA antibodies
                            n = 2 # slice first n rows
        ) %>% 
          mutate(donor = donors$ID[i])
        
        candidatesN<-candidatesN %>%
          mutate(donor = case_when(ID %in% res[[i]]$ID ~ donors$ID[i],
                                   TRUE ~ donor))
        
      }
      
      ## bind the results in the list and compute txScore
      dt <- do.call(rbind, res) %>% 
        mutate(txScore = txscore(ageR = age
                                 , timeD = dialysis
                                 , ageD = donor_age
                                 , mmHLA_A = mmA
                                 , mmHLA_B = mmB
                                 , mmHLA_DR = mmDR)$prob5y
        )
      # add to reactiveval
      compute_resmUK(dt)
     
    })
  })
  
  output$resmUK <- renderDataTable({
    compute_resmUK()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadDataUK <- downloadHandler(
    filename = function() {
      paste("UK_results", ".csv", sep = "")
    },
    content = function(file) {
      write.csv2(compute_resmUK(), file, row.names = FALSE, fileEncoding="latin1")
    }
  )
  
  ## Resume dataset results from UK algorithm
  output$resumeUK <-
    render_gt({
      
      validate(
        need(compute_resmUK() != "", "Results will be presented after the run!")
      )
      
      tabsum<-compute_resmUK() %>% 
        select(bg, age, dialysis, cPRA, Tier, mmHLA, txScore) %>% 
        rename(`Blood group` = bg,
               `receptores' age (years)` = age,
               `time on dialysis (months)` = dialysis,
               `Tier` = Tier,
               `HLA miss matchs` = mmHLA,
               TxScore = txScore)
      
      tbl_summary(tabsum) %>% as_gt()
    })
  
#   ######################################################################################
#   ############ ligação entre inputs ############
#   ############   
#   ############ donor's age
#   observeEvent(input$dage,{
#     new <- input$dage
#     updateSliderInput(session, "dageET", value = new) 
#   })
#   
#   observeEvent(input$dageET,{ 
#     new <- input$dageET
#     updateSliderInput(session, "dage", value = new) 
#   })
#   
#   observeEvent(input$dage,{
#     new <- input$dage
#     updateSliderInput(session, "dageLIMA", value = new) 
#   })
#   
#   observeEvent(input$dageLIMA,{ 
#     new <- input$dageLIMA
#     updateSliderInput(session, "dage", value = new) 
#   })
#   
#   observeEvent(input$dage,{
#     new <- input$dage
#     updateSliderInput(session, "dageUK", value = new) 
#   })
#   
#   observeEvent(input$dageUK,{ 
#     new <- input$dageUK
#     updateSliderInput(session, "dage", value = new) 
#   })
#   
#   ############ donor's ABO
#   observeEvent(input$dabo,{
#     new <- input$dabo
#     updateRadioButtons(session, "daboET", selected = new)
#   })
#   
#   observeEvent(input$daboET,{
#     new <- input$daboET
#     updateRadioButtons(session, "dabo", selected = new)
#   })
#   
#   observeEvent(input$dabo,{
#     new <- input$dabo
#     updateRadioButtons(session, "daboLIMA", selected = new)
#   })
#   
#   observeEvent(input$daboLIMA,{
#     new <- input$daboLIMA
#     updateRadioButtons(session, "dabo", selected = new)
#   })
#   
#   observeEvent(input$dabo,{
#     new <- input$dabo
#     updateRadioButtons(session, "daboUK", selected = new)
#   })
#   
#   observeEvent(input$daboUK,{
#     new <- input$daboUK
#     updateRadioButtons(session, "dabo", selected = new)
#   })
#   
# ############ donor's typing HLA-A a1
# observeEvent(input$a1,{
#   new <- input$a1
#   updateTextAreaInput(session, "a1ET", value  = new)
# })
# 
# observeEvent(input$a1ET,{
#   new <- input$a1ET
#   updateTextAreaInput(session, "a1", value  = new)
# })
# 
# observeEvent(input$a1,{
#   new <- input$a1
#   updateTextAreaInput(session, "a1LIMA", value  = new)
# })
# 
# observeEvent(input$a1LIMA,{
#   new <- input$a1LIMA
#   updateTextAreaInput(session, "a1", value  = new)
# })
# 
# observeEvent(input$a1,{
#   new <- input$a1
#   updateTextAreaInput(session, "a1UK", value  = new)
# })
# 
# observeEvent(input$a1UK,{
#   new <- input$a1UK
#   updateTextAreaInput(session, "a1", value  = new)
# })
# 
# ############ donor's typing HLA-A a2
# observeEvent(input$a2,{
#   new <- input$a2
#   updateTextAreaInput(session, "a2ET", value  = new)
# })
# 
# observeEvent(input$a2ET,{
#   new <- input$a2ET
#   updateTextAreaInput(session, "a2", value  = new)
# })
# 
# observeEvent(input$a2,{
#   new <- input$a2
#   updateTextAreaInput(session, "a2LIMA", value  = new)
# })
# 
# observeEvent(input$a2LIMA,{
#   new <- input$a2LIMA
#   updateTextAreaInput(session, "a2", value  = new)
# })
# 
# observeEvent(input$a2,{
#   new <- input$a2
#   updateTextAreaInput(session, "a2UK", value  = new)
# })
# 
# observeEvent(input$a2UK,{
#   new <- input$a2UK
#   updateTextAreaInput(session, "a2", value  = new)
# })
# 
# ############ donor's typing HLA-B b1
# observeEvent(input$b1,{
#   new <- input$b1
#   updateTextAreaInput(session, "b1ET", value  = new)
# })
# 
# observeEvent(input$b1ET,{
#   new <- input$b1ET
#   updateTextAreaInput(session, "b1", value  = new)
# })
# 
# observeEvent(input$b1,{
#   new <- input$b1
#   updateTextAreaInput(session, "b1LIMA", value  = new)
# })
# 
# observeEvent(input$b1LIMA,{
#   new <- input$b1LIMA
#   updateTextAreaInput(session, "b1", value  = new)
# })
# 
# observeEvent(input$b1,{
#   new <- input$b1
#   updateTextAreaInput(session, "b1UK", value  = new)
# })
# 
# observeEvent(input$b1UK,{
#   new <- input$b1UK
#   updateTextAreaInput(session, "b1", value  = new)
# })
# 
# ############ donor's typing HLA-B b2
# observeEvent(input$b2,{
#   new <- input$b2
#   updateTextAreaInput(session, "b2ET", value  = new)
# })
# 
# observeEvent(input$b2ET,{
#   new <- input$b2ET
#   updateTextAreaInput(session, "b2", value  = new)
# })
# 
# observeEvent(input$b2,{
#   new <- input$b2
#   updateTextAreaInput(session, "b2LIMA", value  = new)
# })
# 
# observeEvent(input$b2LIMA,{
#   new <- input$b2LIMA
#   updateTextAreaInput(session, "b2", value  = new)
# })
# 
# observeEvent(input$b2,{
#   new <- input$b2
#   updateTextAreaInput(session, "b2UK", value  = new)
# })
# 
# observeEvent(input$b2UK,{
#   new <- input$b2UK
#   updateTextAreaInput(session, "b2", value  = new)
# })
# 
# ############ donor's typing HLA-DR dr1
# observeEvent(input$dr1,{
#   new <- input$dr1
#   updateTextAreaInput(session, "dr1ET", value  = new)
# })
# 
# observeEvent(input$dr1ET,{
#   new <- input$dr1ET
#   updateTextAreaInput(session, "dr1", value  = new)
# })
# 
# observeEvent(input$dr1,{
#   new <- input$dr1
#   updateTextAreaInput(session, "dr1LIMA", value  = new)
# })
# 
# observeEvent(input$dr1LIMA,{
#   new <- input$dr1LIMA
#   updateTextAreaInput(session, "dr1", value  = new)
# })
# 
# observeEvent(input$dr1,{
#   new <- input$dr1
#   updateTextAreaInput(session, "dr1UK", value  = new)
# })
# 
# observeEvent(input$dr1UK,{
#   new <- input$dr1UK
#   updateTextAreaInput(session, "dr1", value  = new)
# })
# 
# ############ donor's typing HLA-DR dr2
# observeEvent(input$dr2,{
#   new <- input$dr2
#   updateTextAreaInput(session, "dr2ET", value  = new)
# })
# 
# observeEvent(input$dr2ET,{
#   new <- input$dr2ET
#   updateTextAreaInput(session, "dr2", value  = new)
# })
# 
# observeEvent(input$dr2,{
#   new <- input$dr2
#   updateTextAreaInput(session, "dr2LIMA", value  = new)
# })
# 
# observeEvent(input$dr2LIMA,{
#   new <- input$dr2LIMA
#   updateTextAreaInput(session, "dr2", value  = new)
# })
# 
# observeEvent(input$dr2,{
#   new <- input$dr2
#   updateTextAreaInput(session, "dr2UK", value  = new)
# })
# 
# observeEvent(input$dr2UK,{
#   new <- input$dr2UK
#   updateTextAreaInput(session, "dr2", value  = new)
# })
  
}

