# server functions' file

source("scripts/read_data.R")
source("scripts/compat_fxs.R")
source("scripts/PT_fxs.R")

library(DT)

shinyServer(function(input, output, session) {
  
  output$res <- renderDataTable({

    dt<-pt_points(iso = input$iso, # isogroup compatibility
              dABO = input$dabo, # donor's blood group
              dA = c(input$a1,input$a2),
              dB = c(input$b1,input$b2),
              dDR = c(input$dr1,input$dr2),
              dage = input$dage, # donor's age
              cdata = candidates, # data file with candidates
              pra80 = input$pra80, # points for a PRA equal or higher than 80%
              pra50 = input$pra50, # points for a PRA equal or higher than 50%
              month = input$dialysis, # points for each month on dialysis
              points = input$age_dif, # points for age difference in PT punctuation table
              itemA = input$itemA, # points for A) on PT points table
              itemB = input$itemB, # points for B) on PT points table
              itemC = input$itemC, # points for C) on PT points table
              itemD = input$itemD, # points for D) on PT points table
              itemE = input$itemE, # points for E) on PT points table
              df.abs = abs)

    as.data.frame(dt)
  })
  
}
)