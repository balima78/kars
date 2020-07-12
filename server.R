# server functions' file

#source("scripts/read_data.R")
source("scripts/compat_fxs.R")
source("scripts/PT_fxs.R")

library(DT)
library(tidyverse)
library(openxlsx)

function(input, output) {

  output$res1 <- renderDataTable({
    
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
                  df.abs = abs)

    as.data.frame(dt)
  })
  
  datasetInput <- reactive({
    
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
                          pra80 = as.numeric(input$pra8), # points for a PRA equal or higher than 80%
                          pra50 = as.numeric(input$pra5), # points for a PRA equal or higher than 50%
                          month = input$dialysis, # points for each month on dialysis
                          points = input$age_dif, # points for age difference in PT punctuation table
                          itemA = as.numeric(input$a), # points for A) on PT points table
                          itemB = as.numeric(input$b), # points for B) on PT points table
                          itemC = as.numeric(input$c), # points for C) on PT points table
                          itemD = as.numeric(input$d), # points for D) on PT points table
                          itemE = as.numeric(input$e), # points for E) on PT points table
                          df.abs = abs) %>% 
        mutate(donor = donors$ID[i])
      
      candidatesN<-candidatesN %>% 
        mutate(donor = case_when(ID %in% res[[i]]$ID ~ donors$ID[i],
                                 TRUE ~ donor))
      
    }
    
    ## bind the results in the list
    do.call(rbind, res)
    
  })
  
  output$resm <- renderDataTable({ 
    datasetInput()
    })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Folha1", ".csv", sep = "")
    },
    content = function(file) {
      write.csv2(datasetInput(), file, row.names = FALSE, fileEncoding="latin1")
    }
  )
  
  
  ######################### testes para apagar no tab LIMA#######################
  dataframe<-reactive({
    if (is.null(input$datafile))
      return(NULL)                
    data<-read.csv2(input$datafile$datapath)
    data
  })
  output$table <- renderTable({
    head(dataframe())
  })
  
  output$table2 <- renderTable({
    
    req(input$file_cand)
    
    data<-read.csv2(input$file_cand$datapath)

      dt<-pt_points(iso = input$iso, # isogroup compatibility
                  dABO = input$dabo, # donor's blood group
                  dA = c(input$a1,input$a2),
                  dB = c(input$b1,input$b2),
                  dDR = c(input$dr1,input$dr2),
                  dage = input$dage, # donor's age
                  cdata = dataframe(), # data file with candidates
                  pra80 = as.numeric(input$pra8), # points for a PRA equal or higher than 80%
                  pra50 = as.numeric(input$pra5), # points for a PRA equal or higher than 50%
                  month = input$dialysis, # points for each month on dialysis
                  points = input$age_dif, # points for age difference in PT punctuation table
                  itemA = as.numeric(input$a), # points for A) on PT points table
                  itemB = as.numeric(input$b), # points for B) on PT points table
                  itemC = as.numeric(input$c), # points for C) on PT points table
                  itemD = as.numeric(input$d), # points for D) on PT points table
                  itemE = as.numeric(input$e), # points for E) on PT points table
                  df.abs = abs)

    as.data.frame(dt)
  })
  
  
  output$hist <- renderPlot({
    hist(rnorm(input$n))
  })
}