library(shinythemes)
library(shinybusy)

library(DT)
library(gtsummary)
library(gt)
library(shinycssloaders)

fluidPage(theme = shinytheme("spacelab"),
          
          headerPanel(title ="", windowTitle = "KARS"),
          
          #titlePanel("HEADS | FMUP"),

          titlePanel(a(href="http://bioestatisticas.wixsite.com/bioestatisticas", target="_blank",
                         img(src='ob.jpg', align = "right",height=60,width=150))),
          
          a(href="https://txor.netlify.app/", target="_blank",
            h1("Transplants Open Registry (TxOR)")),
          
          navbarPage("KARS (version 0.1.0)",
                     tabPanel("Home", icon = icon("home"),
                              sidebarPanel(
                                HTML('<p><img src="kidneys2.jpg" width=300 height=300></p>')
                                ),
                              mainPanel(h1("Kidney Allocation Rules Simulator (KARS)"),
                                        br(),
                                        h5("An application to simulate different kidney allocation rules for transplantation."),
                                        h5("The greatest challenge of any kidney transplant program lies in finding enough organ donors (in number and quality) for all waitlisted transplant candidates. While we wait for the future that will bring us transplantable artificial kidneys or the possibility to extend organ preservation, allowing us to substantially increase the number of quality organs from marginal donors, we must resign ourselves to a manifestly insufficient supply of organs for the current demand."),
                                        h5("One way to evaluate the implementation of a new deceased donor kidneys’ allocation system (KAS) would be comparing two transplant candidates’ waiting lists. To one list, organs would be allocated by the rules in force, and on the other list, organs would be allocated, applying new rules in evaluation. However, this kind of study design is not feasible due to the deontological questions it raises. Alternatively, we can simulate different candidates waiting lists subjected to different organ’s allocation rules."),
                                        h5("Before implementing new rules, it is necessary to test them in order to dissipate, as much as possible, doubts questioning implementation’s success. The development of this application had as motivation being an aid to clarify the usefulness and efficiency of new set of rules for kidney allocation."),
                                        HTML('<p style="text-align:right">Bruno A Lima, Oficina de Bioestatística, 2020 <i class="fa fa-creative-commons"></i></p>'),
                                        HTML('<a href="mailto:bioestatisticas@gmail.com">Just e-mail me!</a>'),
                                        br(),
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
                                                   h6("Your data files must be in the exact format as the provided in the example data, unless they are also for the UK algorithm. In this case, candidates' dataset must also have the columns 'Tier', 'MS' and 'RRI' while donors' dataset must have the column 'DRI'"),
                                                   radioButtons("ukfiles", "Select 'UK' if the files are also for the UK algorithm", list("UK"=1, "others"=2),selected=2, inline = TRUE),
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
                                    add_busy_spinner(spin = "fading-circle", position = "full-page"),
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
                                  add_busy_spinner(spin = "fading-circle", position = "full-page"),
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
                     
                     tabPanel("Lima et al", icon = icon("globe"), # file-medical-alt
                              sidebarPanel(
                                shinyjs::useShinyjs(),
                                id = "side-panelLima",
                                a("Define values for Lima's algorithm:"),
                                wellPanel(
                                  checkboxInput("isoLIMA", "ABO identical", TRUE),
                                  numericInput("td3q", "3rd quantile time on dialysis", 62,
                                               1, 99, 1),
                                  numericInput("td2q", "median time on dialysis", 48,
                                               1, 99, 1),
                                  actionButton("reset_inputLIMA", "Reset inputs")
                                  
                                ),
                                HTML('<p><img src="ColorSystem.jpg" width=300 height=300></p>'),
                                a(href="http://www.bbg01.com/cdn/clientes/spnefro/pjnh/46/artigo_14.pdf", 
                                  "Bruno A. Lima, Miguel Mendes, Helena Alves. Kidney transplant allocation in Portugal. Port J Nephrol Hypert, 27(4), 2013: 313-316"),
                              ),
                              
                              mainPanel(
                                radioButtons("selectionTypeLIMA", "", 
                                             list("One donor"= 1, 
                                                  "Multiple donors"= 2), selected = 1,
                                             inline = TRUE),
                                
                                conditionalPanel(
                                  condition = "input.selectionTypeLIMA == '1'",
                                  h4("Options for one donor"),
                                  fluidRow(sliderInput("dageLIMA", "Select donor's age:",
                                                       min = 18, max = 80,
                                                       value = 60, step = 1, sep = "")
                                  ),
                                  fluidRow(column(4,
                                                  radioButtons("daboLIMA", "Select donor's blood group:",
                                                               c("A", "B", "AB", "O"),
                                                               inline = TRUE))
                                  ),
                                  fluidRow(h5("Input HLA typing"),
                                           column(2,
                                                  textAreaInput("a1LIMA", "A1", 1,
                                                                width = 50,
                                                                height = 40)),
                                           column(2,
                                                  textAreaInput("a2LIMA", "A2", 2,
                                                                width = 50,
                                                                height = 40)),
                                           column(2,
                                                  textAreaInput("b1LIMA", "B1", 7,
                                                                width = 50,
                                                                height = 40)),
                                           column(2,
                                                  textAreaInput("b2LIMA", "B2", 8,
                                                                width = 50,
                                                                height = 40)),
                                           column(2,
                                                  textAreaInput("dr1LIMA", "DR1", 1,
                                                                width = 50,
                                                                height = 40)),
                                           column(2,
                                                  textAreaInput("dr2LIMA", "DR2", 3,
                                                                width = 50,
                                                                height = 40))
                                  ),
                                  fluidRow(
                                    h4("top 10 selected candidates for this specific donor"),
                                    dataTableOutput(outputId = "res1LIMA")
                                  )
                                ),
                                conditionalPanel(
                                  condition = "input.selectionTypeLIMA == '2'",
                                  add_busy_spinner(spin = "fading-circle", position = "full-page"),
                                  actionButton("GoLIMA","Select your options and run it!!"),
                                  h6("(it can take several seconds, be patient!)"),
                                  h4("Selected donor-recipient pairs for transplantation:"),
                                  fluidRow(dataTableOutput(outputId = "resmLIMA"),
                                           br(),
                                           downloadButton("downloadDataLIMA", "Download")
                                  ),
                                  fluidRow(hr(),
                                           h4("Resumed results from Lima's algorithm:"),
                                           gt_output(outputId = "resumeLIMA") %>% withSpinner()
                                  )
                                )
                              )
                     ),
                     
                     tabPanel("UK transplant", icon = icon("user-md"),
                              sidebarPanel(
                                shinyjs::useShinyjs(),
                                id = "side-panelUK",
                                a("Define values for UK Transplant:"),
                                wellPanel(
                                  #checkboxInput("isoUK", "ABO identical", TRUE),
                                  
                                  sliderInput("tdUK", "Time on dialysis (points per day)",
                                              min = 0.1, max = 3,
                                              value = 1, step = 0.1, sep = ""),
                                  
                                  # sliderInput("multipleUK", "multiplyer for Donor-recipient risk matrix:",
                                  #             min = 0.1, max = 2,
                                  #             value = 1, step = 0.1, sep = ""),
                                  DTOutput('tableDRriskUK'),
                                  h5("Donor-Recipient risk index combinations."),
                                  
                                  sliderInput("bb1UK", "b1",
                                              min = 1000, max = 2000,
                                              value = 1200, step = 100, sep = ""),
                                  sliderInput("aa1UK", "a1",
                                              min = 2000, max = 3000,
                                              value = 2300, step = 100, sep = ""),
                                  sliderInput("bb2UK", "b2",
                                              min = 100, max = 1000,
                                              value = 750, step = 50, sep = ""),
                                  sliderInput("aa2UK", "a2",
                                              min = 1000, max = 2000,
                                              value = 1500, step = 100, sep = ""),
                                  sliderInput("bb3UK", "b3",
                                              min = 100, max = 1000,
                                              value = 400, step = 100, sep = ""),
                                  h5("HLA match and age combined"),
                                  h6("For each HLA mismatch level, points are defined as:"),
                                  h6("Level1 = b1*COS(age/18)+a1"),
                                  h6("Level2 = b2*COS(age/18)+a2"),
                                  h6("Level3+4 = b3*COS(age/18)"),
                                  
                                  sliderInput("mUK", "m",
                                              min = 10, max = 100,
                                              value = 40, step = 10, sep = ""),
                                  sliderInput("nUK", "n",
                                              min = 1, max = 10,
                                              value = 4.5, step = 0.1, sep = ""),
                                  sliderInput("oUK", "o",
                                              min = 1, max = 10,
                                              value = 4.7, step = 0.1, sep = ""),
                                  h5("Matchability"),
                                  h6("Points are defined as:"),
                                  h6("m * (1 + (Match score / n)^o)"),
                                  plotOutput("matchability"),
                                  
                                  sliderInput("mm0UK", "Total HLA mismatch = 0",
                                              min = -100, max = 100,
                                              value = 0, step = 50, sep = ""),
                                  sliderInput("mm1UK", "Total HLA mismatch = 1",
                                              min = -500, max = 0,
                                              value = -100, step = 50, sep = ""),
                                  sliderInput("mm23UK", "Total HLA mismatch = 2-3",
                                              min = -500, max = 0,
                                              value = -150, step = 50, sep = ""),
                                  sliderInput("mm46UK", "Total HLA mismatch = 4-6",
                                              min = -500, max = 0,
                                              value = -250, step = 50, sep = ""),
                                              
                                  sliderInput("bloodUK", "Blood group match",
                                              min = -2000, max = -100,
                                              value = -1000, step = 100, sep = ""),
                                  h6("negative points are allocate for blood group B candidates when the donor is group O (Tier B only)"),
                                  
                                  actionButton("reset_inputUK", "Reset inputs")
                                  )
                                ),
                              
                              mainPanel(
                                radioButtons("selectionTypeUK", "", 
                                             list("One donor"= 1, 
                                                  "Multiple donors"= 2), selected = 1,
                                             inline = TRUE),
                                
                                conditionalPanel(
                                  condition = "input.selectionTypeUK == '1'",
                                  h4("Options for one donor"),
                                  fluidRow(column(6,
                                                  sliderInput("dageUK", "Select donor's age:",
                                                       min = 18, max = 80,
                                                       value = 60, step = 1, sep = "")),
                                           column(6,
                                                  sliderInput("dheightUK", "Select donor's height:",
                                                              min = 140, max = 199,
                                                              value = 175, step = 1, sep = ""))
                                           ),
                                  fluidRow(column(4,
                                                  radioButtons("dhtUK", "Did the donor have an history of hipertension?:",
                                                               c("No", "Yes"),
                                                               inline = TRUE)),
                                           column(4,
                                                  radioButtons("dsexUK", "Select Donor's gender:",
                                                               c("Male", "Female"),
                                                               inline = TRUE)),
                                           column(4,
                                                  radioButtons("dcmvUK", "Is the donor CMV +ve?:",
                                                               c("No", "Yes"),
                                                               inline = TRUE))
                                           ),
                                  fluidRow(column(6,
                                                  sliderInput("dgfrUK", "Select donor's eGFR:",
                                                       min = 80, max = 120,
                                                       value = 100, step = 1, sep = "")),
                                           column(6,
                                                   sliderInput("dhospUK", "Select donor's days in Hospital:",
                                                       min = 0, max = 120,
                                                       value = 30, step = 1, sep = ""))
                                           ),
                                  h5("Donor Risk Index is calculated for each donor using the above 7 risk factors."),
                                  h5("The donor is then categorised in to one of 4 groups: D1 (<0.79); D2 (0.79-1.12); D3 (1.12-1.5); D4 (>1.5)"),
                                  fluidRow(textOutput(outputId = "dri")),
                                  br(),
                                  fluidRow(radioButtons("daboUK", "Select donor's blood group:",
                                                        c("A", "B", "AB", "O"),
                                                        inline = TRUE)
                                  ),
                                  fluidRow(h5("Input HLA typing"),
                                           column(2,
                                                  textAreaInput("a1UK", "A1", 1,
                                                                width = 50,
                                                                height = 40)),
                                           column(2,
                                                  textAreaInput("a2UK", "A2", 2,
                                                                width = 50,
                                                                height = 40)),
                                           column(2,
                                                  textAreaInput("b1UK", "B1", 7,
                                                                width = 50,
                                                                height = 40)),
                                           column(2,
                                                  textAreaInput("b2UK", "B2", 8,
                                                                width = 50,
                                                                height = 40)),
                                           column(2,
                                                  textAreaInput("dr1UK", "DR1", 1,
                                                                width = 50,
                                                                height = 40)),
                                           column(2,
                                                  textAreaInput("dr2UK", "DR2", 3,
                                                                width = 50,
                                                                height = 40))
                                  ),
                                  fluidRow(
                                    h4("top 10 selected candidates for this specific donor"),
                                    dataTableOutput(outputId = "res1UK")
                                  )
                                ),
                                conditionalPanel(
                                  condition = "input.selectionTypeUK == '2'",
                                  add_busy_spinner(spin = "fading-circle", position = "full-page"),
                                  actionButton("GoUK","Select your options and run it!!"),
                                  h6("(it can take several seconds, be patient!)"),
                                  h4("Selected donor-recipient pairs for transplantation:"),
                                  fluidRow(dataTableOutput(outputId = "resmUK"),
                                           br(),
                                           downloadButton("downloadDataUK", "Download")
                                  ),
                                  fluidRow(hr(),
                                           h4("Resumed results from UK Transplant:"),
                                           gt_output(outputId = "resumeUK") %>% withSpinner()
                                  )
                                )
                              )
                     ),
                     
                     tabPanel("Material and Methods", icon = icon("cogs"),
                              a(href="https://kars-manual.netlify.app", "KARS app's users' manual is available from here"),
                              br(),
                              br(),
                              h5("When possible, kidney transplantation is the preferred treatment for end stage renal disease patients. 
                                 The number of patients waiting for a kidney transplant is much higher than 
                                 the number of available organs, so it is imperative to define organ allocation systems 
                                 that guarantee an equitable distribution of this scarce resource."),
                              h5("The ability to predict kidney transplant success at time of organ allocation is an essential leverage 
                                 if we want to minimize the number of patients who return to an already overcrowded waiting list for transplantation."),
                              h5("Since evidence-based medicine is increasingly used as the standard to define good practices in healthcare, 
                                 there is a need to develop prognostic tools that can be used in decision making. 
                                 Therefore, the definition of deceased donors’ kidneys allocation rules on transplantation must be supported 
                                 by simulation exercises that allows foreseeing, as much as possible, the consequences of these rules."), 
                              h5("Kidney Allocation Rules Simulator (KARS) application allows testing different kidney transplant allocation systems 
                                 with different donors and transplant candidates’ datasets."),
                              h5("In this application it is possible to simulate allocation rules implemented in Portugal, 
                                 in countries within Eurtotransplant, in the United Kingdom,  
                                 and a system previously suggested by Lima et al. 
                                 Also, KARS application allows changing the original punctuation used by the mentioned allocation systems, 
                                 in order to simulate how these changes are translated in the type of candidates selected for transplantation."), 
                              h5("As inputs, this application has three data files: a file with transplant candidates’ information, a file with candidates’ anti-HLA antibodies, and a file with donors’ characteristics. 
                                 As output it is returned a file with donor-recipient pairs selected according to the kidney allocation system simulated."),
                              h5("For each one of the models (Portugal, EuroTransplant, Lima et al, UK transplant), 
                                 transplant candidates are excluded considering ABO compatibility and Virtual Crossmatch and 
                                 the remaining are ordered for each donor according to the model’s rules."),
                              h5("For the option ‘One donor’, top 10 candidates are displayed regarding the donor characteristics; 
                                 for the option ‘Multiple donors’ each donor is attributed to de 2 best classified candidates and 
                                 at the end we will have a dataset with the number of candidates equal to the double of the number of donors."),
                              h5("A resume of the characteristics of the selected candidates is displayed allowing for the comparison between models."),
                              HTML("<h5>With the same candidates and donors’ files we will have different outputs for each simulation we run, 
                                 and for each output we will have a resume of the selected donor-recipients pairs as: 
                                 recipients’ blood groups, frequencies median ages, median time on dialyses, cPRA frequencies, HLA mismatches, and median transplant score (TxScore) as a measure of good transplant outcome. 
                                 The TxScore is compute as described by <a href='https://pubmed.ncbi.nlm.nih.gov/27391198/'>Molnar, et al</a> as the estimated 5 year probability to mortality or graft failure. 
                                 For this score it is assumed that all donors have no diabetes and are not extended criteria donors.
                                 Also, as default, it is assumed that all recepients are white, have no coronary artery disease, have no diabetes and their end stage renal disease is unknown. 
                                 Likewise, we can compare TxScores based on donors' and recipients' ages, recipients time on dialysis and HLA mismacths between donor and recipient.</h5>"),
                              h5("When seeking for waste reduction while ensuring a fair distribution of organs from deceased donors, 
                                 the definition of rules for the selection of donor-recipient pairs in renal transplantation must be based on evidences supported by data.  
                                 With this purpose, we also need to be able to predict transplant outcomes to define the best allocation rules."),
                              br(),
                              a(href="https://kars-manual.netlify.app", "KARS app's users' manual is available from here")
                     )
          )
)