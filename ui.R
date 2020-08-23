library(shinythemes)

library(DT)
library(gtsummary)
library(gt)
library(shinycssloaders)

fluidPage(theme = shinytheme("spacelab"),
          
          headerPanel(title ="", windowTitle = "KARS"),
          
          titlePanel("HEADS | FMUP"),
          
          navbarPage("KARS (version 0.0.1)",
                     tabPanel("Home", icon = icon("home"),
                              sidebarPanel(
                                HTML('<p><img src="kidneys2.jpg" width=300 height=300></p>')
                                ),
                              mainPanel(h1("Kidney Allocation Rules Simulator (KARS)"),
                                        br(),
                                        h5("An application to simulate different kidney allocation rules for kidney allocation for transplantation."),
                                        h5("The greatest challenge of any kidney transplant program lies in finding enough organ donors (in number and quality) for all waitlisted transplant candidates. While we wait for the future that will bring us transplantable artificial kidneys or the possibility to extend organ preservation, allowing us to substantially increase the number of quality organs from marginal donors, we must resign ourselves to a manifestly insufficient supply of organs for the current demand."),
                                        h5("One way to evaluate the implementation of a new deceased donor kidneys’ allocation system (KAS) would be comparing two transplant candidates’ waiting lists. To one list, organs would be allocated by the rules in force, and on the other list, organs would be allocated, applying new rules in evaluation. However, this kind of study design is not feasible due to the deontological questions it raises. Alternatively, we can simulate different candidates waiting lists subjected to different organ’s allocation rules."),
                                        h5("Before implementing new rules, it is necessary to test them in order to dissipate, as much as possible, doubts questioning implementation’s success. The development of this application had as motivation being an aid to clarify the usefulness and efficiency of new set of rules for kidney allocation."),
                                        HTML('<p style="text-align:right">Bruno A Lima, Oficina de Bioestatística, 2020 <i class="fa fa-creative-commons"></i></p>'),
                                        br(),
                                        h4("Disclaimer: "),
                                        h5("This application is intended for research purposes only, not for clinical or commercial use. It is a non-profit service to the scientific community, provided on an 'AS-IS' basis without any warranty, expressed or implied. The authors can not be held liable in any way for the service provided here.")
                                        )
                              ),
                     tabPanel("Upload data", icon = icon("database"),
                              # select data to use
                              sidebarPanel(
                                a("Upload files:"),
                                wellPanel(
                                  radioButtons("dataInput", "", 
                                               list("Load example data"=1, 
                                                    "Upload your files"=2), selected=1),
                                  conditionalPanel(condition="input.dataInput=='2'",
                                                   a("Your data files must be in the exact format as the provided example data"),
                                                   fileInput("file_cand", "upload candidates",
                                                             accept = c("text/csv",
                                                                        "text/comma-separated-values,text/plain",
                                                                        ".csv")),
                                                   fileInput("file_abs", "upload candidates' antibodies",
                                                             accept = c("text/csv",
                                                                        "text/comma-separated-values,text/plain",
                                                                        ".csv")),
                                                   fileInput("file_donor", "upload donors",
                                                             accept = c("text/csv",
                                                                        "text/comma-separated-values,text/plain",
                                                                        ".csv")),
                                                   radioButtons("fileSepDF", "Files' delimiter:", list("Comma"=1, "Tab"=2, "Semicolon"=3, "Space"=4),selected=3)
                                  
                                )
                              )
                              ),
                              # show selected data 
                              mainPanel(
                                conditionalPanel(condition="input.dataInput=='1'",
                                                 h4("Example data for transplant candidates:"),
                                                 dataTableOutput(outputId = "ex.cands")),
                                conditionalPanel(condition="input.dataInput=='2'",
                                                 h4("Uploaded data for transplant candidates:"),
                                                 dataTableOutput(outputId = "sel.cands")),
                                hr(),                 
                                conditionalPanel(condition="input.dataInput=='1'",
                                                 h4("Example data for HLA antibodies from transplant candidates"),
                                                 dataTableOutput(outputId = "ex.abs")),
                                conditionalPanel(condition="input.dataInput=='2'",
                                                 h4("Uploaded data for HLA antibodies from transplant candidates"),
                                                 dataTableOutput(outputId = "sel.abs")),
                                
                                hr(),
                                conditionalPanel(condition="input.dataInput=='1'",
                                                h4("Example data from kidney donors"),
                                                dataTableOutput(outputId = "ex.donors")),
                                conditionalPanel(condition="input.dataInput=='2'",
                                                 h4("Uploaded data from kidney donors"),
                                                 dataTableOutput(outputId = "sel.donors"))
                                
                                
                              )
                              
                     ),
                     tabPanel("Portugal", icon = icon("medrt"),
                              
                              # Sidebar with options 
                              sidebarPanel(
                                shinyjs::useShinyjs(),
                                id = "side-panelPT",
                                
                                a("Define punctuaction for PT algorithm:"),
                                wellPanel(
                                  checkboxInput("iso", "ABO identical", TRUE),
                                  
                                  sliderInput("a", "no HLA mismatchs",
                                              min = 0, max = 20,
                                              value = 12, step = 1, sep = ""),
                                  sliderInput("b", "no mismatchs for B and DR",
                                              min = 0, max = 20,
                                              value = 8, step = 1, sep = ""),
                                  sliderInput("c", "one mismatch for B or DR",
                                              min = 0, max = 20,
                                              value = 4, step = 1, sep = ""),
                                  sliderInput("d", "one mismatch for B and one for DR",
                                              min = 0, max = 20,
                                              value = 2, step = 1, sep = ""),
                                  sliderInput("e", "all other possibilities",
                                              min = 0, max = 20,
                                              value = 1, step = 1, sep = ""),
                                  sliderInput("pra8", "PRA > 80%",
                                              min = 0, max = 20,
                                              value = 8, step = 1, sep = ""),
                                  sliderInput("pra5", "PRA > 50%",
                                              min = 0, max = 20,
                                              value = 4, step = 1, sep = ""),
                                  sliderInput("dialysis", "Time on dialysis (points per month)",
                                              min = 0, max = 1,
                                              value = 0.1, step = 0.01, sep = ""),
                                  sliderInput("age_dif", "Age difference between donor and candidate",
                                              min = 1, max = 20,
                                              value = 4, step = 1, sep = ""),
                                  h6("(points for a candidate younger than 55 and a donor older than 40 or a candidate older than 55 and a donor younger than 60)"),
                                  actionButton("reset_inputPT", "Reset inputs")
                                  )
                                
                              ),
                              # Show results 
                              mainPanel(
                                radioButtons("selectionTypePT", "", 
                                             list("One donor"= 1, 
                                                  "Multiple donors"= 2), selected=1,
                                             inline = TRUE),
                                hr(),
                                conditionalPanel(
                                  condition = "input.selectionTypePT == '1'",
                                  h4("Options for one donor"),
                                  fluidRow(sliderInput("dage", "Select donor's age:",
                                                     min = 18, max = 80,
                                                     value = 60, step = 1, sep = "")
                                         ),
                                  fluidRow(column(4,
                                                radioButtons("dabo", "Select donor's blood group:",
                                                             c("A", "B", "AB", "O"),
                                                             inline = TRUE))
                                           ),
                                  fluidRow(h5("Input HLA typing:"),
                                    column(2,
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
                                    h4("top 10 selected candidates for this specific donor"),
                                    dataTableOutput(outputId = "res1")
                                    )
                                  ),
                                  conditionalPanel(
                                    condition = "input.selectionTypePT == '2'",
                                    actionButton("Go","Select your options and run it!!"),
                                    h6("(it can take several seconds, be patient!)"),
                                    h4("Selected donor-recipient pairs for transplantation:"),
                                    fluidRow(dataTableOutput(outputId = "resm"),
                                             br(),
                                             downloadButton("downloadData", "Download")
                                             ),
                                    fluidRow(
                                      hr(),
                                      h4("Resumed results from PT algorithm:"),
                                      gt_output(outputId = "resumePT") %>% withSpinner()
                                      )
                                    )
                              )
                     ),
                     
                     tabPanel("EuroTransplant", icon = icon("medkit"), #
                              sidebarPanel(
                                shinyjs::useShinyjs(),
                                id = "side-panelET",
                                a("Define punctuaction for ETKAS algorithm:"),
                                wellPanel(
                                  checkboxInput("isoET", "ABO identical", TRUE),
                                  sliderInput("mm0", "0  HLA-A, -B, -DR mismatchs",
                                              min = 0, max = 499,
                                              value = 400, step = 10, sep = ""),
                                  sliderInput("mm1", "1  HLA-A, -B, -DR mismatchs",
                                              min = 0, max = 499,
                                              value = 333.33, step = 10, sep = ""),
                                  sliderInput("mm2", "2  HLA-A, -B, -DR mismatchs",
                                              min = 0, max = 499,
                                              value = 266.67, step = 10, sep = ""),
                                  sliderInput("mm3", "3  HLA-A, -B, -DR mismatchs",
                                              min = 0, max = 499,
                                              value = 200, step = 10, sep = ""),
                                  sliderInput("mm4", "4  HLA-A, -B, -DR mismatchs",
                                              min = 0, max = 499,
                                              value = 133.33, step = 10, sep = ""),
                                  sliderInput("mm5", "5  HLA-A, -B, -DR mismatchs",
                                              min = 0, max = 499,
                                              value = 66.67, step = 10, sep = ""),
                                  sliderInput("mm6", "6  HLA-A, -B, -DR mismatchs",
                                              min = 0, max = 499,
                                              value = 0, step = 10, sep = ""),
                                  sliderInput("tdET", "Time on dialysis (points per month)",
                                              min = 0, max = 10,
                                              value = 2.78, step = 1, sep = ""),
                                  
                                  actionButton("reset_inputET", "Reset inputs")
                                )
                              ),
                              
                              mainPanel(
                                radioButtons("selectionTypeET", "", 
                                             list("One donor"= 1, 
                                                  "Multiple donors"= 2), selected = 1,
                                             inline = TRUE),
                                hr(),
                                radioButtons("hlafreqs", "Select HLA frequencies origin, to compute MMP:", 
                                             list("Portugal"= 1, 
                                                  "EuroTransplant"= 2),
                                             selected = 1, inline = TRUE),
                                conditionalPanel(
                                  condition = "input.selectionTypeET == '1'",
                                  h4("Options for one donor"),
                                  fluidRow(sliderInput("dageET", "Select donor's age:",
                                                     min = 18, max = 80,
                                                     value = 60, step = 1, sep = "")
                                           ),
                                  fluidRow(column(4,
                                                radioButtons("daboET", "Select donor's blood group:",
                                                             c("A", "B", "AB", "O"),
                                                             inline = TRUE))
                                           ),
                                  fluidRow(h5("Input HLA typing"),
                                    column(2,
                                           textAreaInput("a1ET", "A1", 1,
                                                         width = 50,
                                                         height = 40)),
                                    column(2,
                                           textAreaInput("a2ET", "A2", 2,
                                                         width = 50,
                                                         height = 40)),
                                    column(2,
                                           textAreaInput("b1ET", "B1", 7,
                                                         width = 50,
                                                         height = 40)),
                                    column(2,
                                           textAreaInput("b2ET", "B2", 8,
                                                         width = 50,
                                                         height = 40)),
                                    column(2,
                                           textAreaInput("dr1ET", "DR1", 1,
                                                         width = 50,
                                                         height = 40)),
                                    column(2,
                                           textAreaInput("dr2ET", "DR2", 3,
                                                         width = 50,
                                                         height = 40))
                                    ),
                                  fluidRow(
                                    h4("top 10 selected candidates for this specific donor"),
                                    dataTableOutput(outputId = "res1ET")
                                    )
                                  ),
                                conditionalPanel(
                                  condition = "input.selectionTypeET == '2'",
                                  actionButton("GoET","Select your options and run it!!"),
                                  h6("(it can take several seconds, be patient!)"),
                                  h4("Selected donor-recipient pairs for transplantation:"),
                                  fluidRow(dataTableOutput(outputId = "resmET"),
                                           br(),
                                           downloadButton("downloadDataET", "Download")
                                           ),
                                  fluidRow(hr(),
                                           h4("Resumed results from ET algorithm:"),
                                           gt_output(outputId = "resumeET") %>% withSpinner()
                                           )
                                  )
                              )
                     ),
                     
                     tabPanel("Lima", icon = icon("globe"), # file-medical-alt
                              sidebarPanel(
                                radioButtons("dataInput", "", 
                                             list("Load example data"=1, "Upload a file"=2), 
                                             selected=1),
                                conditionalPanel(condition="input.dataInput=='1'",
                                                 h5("texto de exemplo")
                                ),
                                conditionalPanel(condition="input.dataInput=='2'",
                                                 HTML('<br>'),
                                                 h5("Upload a delimited text file (max. 10MB): "),
                                                 #HTML('<i class="fa fa-beer fa-lg"></i>'),
                                                 fileInput("upload", "", multiple = FALSE),
                                                 radioButtons("fileSepDF", "Delimiter:", list("Comma"=1, "Tab"=2, "Semicolon"=3, "Space"=4),selected=2),
                                                 
                                                 conditionalPanel(condition="input.fileSepDF!='1'",
                                                                  checkboxInput(inputId = "decimal", label = "Use comma as decimal", value = FALSE)
                                                 ),
                                                 
                                                 HTML('<br>'),
                                                 HTML('<p>You can upload your data as separated by comma, tab, semicolon or space.</p>'),
                                                 HTML('<p><b>Note</b>: First row must be header.</p>')
                                )
                              )
                     ),
                     
                     tabPanel("UK transplant", icon = icon("user-md"),
                              sidebarPanel(
                                a("xxxx"),
                                wellPanel(
                                  sliderInput("anoH", "Seleccione intervalo:",
                                              min = 2000, max = 2018, step = 1, sep = "",
                                              value = c(2000,2018))
                                )
                              ),
                              
                              mainPanel()
                     ),
                     
                     tabPanel("Material e Métodos", icon = icon("cogs"),
                              h5("Aqui ficarão descritos os algoritmos utilizados, bem como o tipo de dados")
                     )
          )
)