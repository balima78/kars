# server functions' file

source("scripts/read_data.R")
source("scripts/compat_fxs.R")
source("scripts/PT_fxs.R")

library(DT)
library(tidyverse)
library(openxlsx)

function(input, output) {
  
  output$ex.cands<- renderDataTable({
    
    datatable(ex.candidates,
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
      need(identical(colnames(data),colnames(ex.candidates)), 
           "Candidates column names are not identical to example data!")
      )
    
    data %>% 
      mutate_at(vars(A1,A2,B1,B2,DR1,DR2),as.character) %>% 
      mutate_at(vars(ID), as.numeric)
    
    
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
      need(identical(colnames(data),colnames(ex.donors)), 
           "Donors column names are not identical to example data!")
    )
    
    data %>% 
      mutate_at(vars(A1,A2,B1,B2,DR1,DR2),as.character) %>% 
      mutate_at(vars(ID,age), as.numeric)
    
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
      mutate_at(vars(ID), as.numeric)
    
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
  
## for pair of candidates selected according to unique donor 
  
  output$res1 <- renderDataTable({
    
    if (input$dataInput == 1) {candidates<-ex.candidates} else {candidates<-datasetCands()}
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

    datatable(dt, options = list(pageLength = 5, dom = 'tip'))
  })

  
  #### to reset PT sidebarpanel
  observeEvent(input$reset_inputPT, {
    shinyjs::reset("side-panelPT")
  })
 
  observeEvent(input$Go, {
    
    pra80 = as.numeric(input$pra8) # points for a PRA equal or higher than 80%
    pra50 = as.numeric(input$pra5) # points for a PRA equal or higher than 50%
    month = input$dialysis # points for each month on dialysis
    points = input$age_dif # points for age difference in PT punctuation table
    itemA = as.numeric(input$a) # points for A) on PT points table
    itemB = as.numeric(input$b) # points for B) on PT points table
    itemC = as.numeric(input$c) # points for C) on PT points table
    itemD = as.numeric(input$d) # points for D) on PT points table
    itemE = as.numeric(input$e) # points for E) on PT points table
   
    output$resm <- renderDataTable({ 
    
    if (input$dataInput == 1) {candidates<-ex.candidates} else {candidates<-datasetCands()}
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
    candidatesN<-candidates %>% mutate(donor = 0)
    
    # create a list with the same length of the number of donors
    res <- vector("list", length = dim(donors)[1])
    
    # now the for loop
    for (i in 1:dim(donors)[1]){
      candid<-candidatesN %>% filter(donor == 0)
      
      res[[i]]<-pt_points(iso = TRUE, # isogroup compatibility
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
    
    ## bind the results in the list
    do.call(rbind, res)
    
    })
  })
  
  # output$resm <- renderDataTable({ 
  #   
  #   compute_resm()
  # })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Folha1", ".csv", sep = "")
    },
    content = function(file) {
      write.csv2(compute_resm(), file, row.names = FALSE, fileEncoding="latin1")
    }
  )
  
  
  ######################### testes para apagar no tab LIMA#######################
  
  
  observeEvent(input$reset_inputET, {
    shinyjs::reset("side-panelET")
  })
  
  
}