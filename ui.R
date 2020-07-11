library(shiny)
library(shinythemes)
library(plotly)
library(shinydashboard)

# user interface file

shinyUI(
  fluidPage(theme = shinytheme("spacelab"), 
    headerPanel(title ="",
                windowTitle = "HEADS"),
    titlePanel("HEADS | FMUP"),
    
    navbarPage("KARS",
               tabPanel("Portugal", icon = icon("heartbeat"),
                        # Application title
                        #titlePanel("opções para os gráficos"),
                        # Sidebar with options 
                        sidebarPanel(
                          a("Upload files:"),
                          wellPanel(
                            fileInput("file_cand", "upload candidates"),
                            fileInput("file_abs", "upload candidates' antibodies"),
                            fileInput("file_donor", "upload donors")
                            
                          ),
                          a("Define punctuaction for PT algorithm:"),
                          wellPanel(
                            sliderInput("itemA", "no HLA mismatchs",
                                        min = 0, max = 20,
                                        value = 12, step = 1, sep = ""),
                            sliderInput("itemB", "no mismatchs for B and DR",
                                        min = 0, max = 20,
                                        value = 8, step = 1, sep = ""),
                            sliderInput("itemC", "one mismatch for B or DR",
                                        min = 0, max = 20,
                                        value = 4, step = 1, sep = ""),
                            sliderInput("itemD", "one mismatch for B and one for DR",
                                        min = 0, max = 20,
                                        value = 2, step = 1, sep = ""),
                            sliderInput("itemA", "all other possibilities",
                                        min = 0, max = 20,
                                        value = 1, step = 1, sep = ""),
                            sliderInput("pra80", "PRA > 80%",
                                        min = 0, max = 20,
                                        value = 8, step = 1, sep = ""),
                            sliderInput("pra50", "PRA > 50%",
                                        min = 0, max = 20,
                                        value = 4, step = 1, sep = ""),
                            sliderInput("dialysis", "Time on dialysis (in months)",
                                        min = 0, max = 1,
                                        value = 0.1, step = 0.01, sep = ""),
                            sliderInput("age_dif", "Age difference between donor and candidate",
                                        min = 0, max = 20,
                                        value = 4, step = 1, sep = "")
                          )

                        ),
                        # Show a plot of despesas
                        mainPanel(
                          a("Options for one donor"),
                          fluidRow(sliderInput("dage", "Select donor's age:",
                                               min = 18, max = 80,
                                               value = 60, step = 1, sep = "")
                                 ),
                          fluidRow(column(4,
                                          radioButtons("dabo", "Select donor's blood group:",
                                              c("A", "B", "AB", "O"),
                                              inline = TRUE)),
                                   column(4,
                                          checkboxInput("iso", "ABO identical", TRUE))
                                 ),
                          fluidRow(column(2,
                                          textAreaInput("a1", "A1", 1,
                                                        width = 50,
                                                        height = 40)),
                                   column(2, 
                                          textAreaInput("a2", "A2", 2,
                                                        width = 50,
                                                        height = 40)),
                                   column(2,
                                          textAreaInput("b1", "B1", 7,
                                                        width = 50,
                                                        height = 40)),
                                   column(2,
                                          textAreaInput("b2", "B2", 8,
                                                        width = 50,
                                                        height = 40)),
                                   column(2,
                                          textAreaInput("dr1", "DR1", 1,
                                                        width = 50,
                                                        height = 40)),
                                   column(2,
                                          textAreaInput("dr2", "DR2", 3,
                                                        width = 50,
                                                        height = 40))
                                 ),
                          fluidRow(
                            dataTableOutput(outputId = "res")
                                 ),
                          hr()
                        )
                        ),
               ##### novos tab para resultados GHD e outros
               tabPanel("EuroTransplant", icon = icon("medkit"),
                        sidebarPanel(
                          a("xxxx"),
                          wellPanel(
                            sliderInput("anoH", "Seleccione intervalo:",
                                        min = 2000, max = 2018, step = 1, sep = "",
                                        value = c(2000,2018))
                          )
                        ),
                        mainPanel(
                          plotOutput("xxxxx"),
                          hr("xxxx"),
                          plotOutput("distPlot"),
                          a(href="xxxx",""),
                          textOutput("xx"),
                          plotOutput("xxx"),
                          hr("xxxx"),
                          plotOutput("xxx"),
                          a(href="xxx","xxx"),
                          textOutput("xxx")
                        )
               ),
               tabPanel("Lima", icon = icon("file-medical-alt"),
                        mainPanel(
                          plotlyOutput("xxxx"),
                          hr("xxxx"),
                          plotlyOutput("xxx"),
                          hr("xxxx"),
                          br(),
                          hr("xxx"),
                          tableOutput("xxxx"),
                          a(href="", "xxxx")
                        )
               ),
               tabPanel("UK transplant", icon = icon("user-md"),
                        sidebarPanel(
                          a("opções para os gráficos"),
                          wellPanel(sliderInput("anoP", "Selecione ano",
                                                # animate = animationOptions(interval = 1600, loop = TRUE),
                                                min = 2000, max = 2018,
                                                value = 2009, step = 1, sep = ""
                          )
                          )
                        ),
                        mainPanel(
                          titlePanel("título do painel"),
                          plotOutput("xxxxx"),
                          a(href="", ""),
                          plotOutput("sxxxx"),
                          a(href="xxxx", 
                            "xxxx")
                        )
               ),
               tabPanel("Material e Métodos", icon = icon("cogs"),
                        h5("Aqui ficarão descritos os algoritmos utilizados, bem como o tipo de dados")
               )
    )
  )
)