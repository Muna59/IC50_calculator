# Shiny application to calculate slope, intercept, and ic50 values 
# ========================================================================
library(shiny)
library(shinythemes)

# ========================================================================
# UI
ui <- fluidPage(
               theme = shinytheme("sandstone"),
               navbarPage("IC50 calculator",
               tabPanel("Calculate IC50",              # first tab
                  sidebarLayout(
                  sidebarPanel(
                      fileInput("ic50_raw", "Upload data (.csv file):",
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"))
                              ),
                  mainPanel(downloadButton("downloadRes", "Download"),
                            tableOutput("ic50_res"))  
                               )),
              tabPanel("User manual",                 # second tab
                 mainPanel(h3("Introduction"),
                       p("The half maximal inhibitory concentration (IC50) 
                         is a quantitative measure of the concentration of 
                         an inhibitory substance that is needed to inhibit 
                         a biological component of a process by 50%. It 
                         has been widely implemented in several 
                         experiments, such as MTT assay and DPPH radical 
                         scavenging activity."),
                       
                           h3("IC50 calculator"),
                      p("IC50 calculator is a web app that is used to
                         determine IC50 values for linear data. This can
                         be done in simple steps:"),
                      
                           h4("1) Data Preparation:"),
                      p("Prepare the data based on the following format:"),
                      img(src = "data_format.png"),
                      p(" "),
                      p("The first column (samples) should contain the 
                         sample names. Each row will correspond to a 
                         single sample. The app will calculate the IC50 
                         values of each row separately. Thus, samples 
                         from different treatments could be placed in the
                         same file."),
                      p("The first row in the following columns should 
                         contain the values of the concentration of tested
                         substance. The concentration should be written
                         as a number without units."),
                      p("The values of the percentage of inhibition or 
                         percentage of viability should be written in the
                         cells corresponding to them without writing % sign."),
                      p("An example data is provided in the following 
                         table, where two drugs were tested (drug A and 
                         drug B) at different concentrations ranging from
                         100 to 6.25. Each treatment has 3 replicas."),
                      img(src = "data_example.png"),
                      p(" "),
                      p("The file should be saved as CSV (Comma 
                        delimited)(*.csv)"),
                      
                          h4("2) Upload data:"),
                      p("In (Calculate IC50) tab, click on 'Browse' button 
                         and select the prepared file. The IC50 calculation
                         will be done and the results will be presented 
                         immediatley. The slope, y-intercept, and IC50 
                        values will be shown for each sample."),
                      img(src = "result_example.png"),
                      p(" "),
                      h4("3) Download data:"),
                      p("You can click on 'Download' button to download the result.")
             ))
  )
  
)
server <- function(input, output, session){
  
  ic50_dat <- reactive({    
    req(input$ic50_raw, file.exists(input$ic50_raw$datapath))
    
    dat <- read.csv(input$ic50_raw$datapath, 
                    check.names = FALSE, 
                    header = FALSE, 
                    row.names = 1)
    i <- 2:nrow(dat)
    a <- vector(mode = "numeric", length = nrow(dat)-1)
    b <- vector(mode = "numeric", length = nrow(dat)-1)
    for(i in 2:nrow(dat)){
      b[i-1] <- lm(as.numeric(dat[i,])~as.numeric(dat[1,]))$coefficients[1]
      a[i-1] <- lm(as.numeric(dat[i,])~as.numeric(dat[1,]))$coefficients[2]
    }
    ic50 <- (50-b)/a
    res <- cbind(dat[-1,], a, b, ic50)
    colnames(res) <- c(dat[1, ], "Slope", "Y-intercept", "IC50")
    res
  })
  output$downloadRes <- downloadHandler(
    filename = function() {
      paste("IC50", ".csv")
    },
    content = function(file) {
      write.csv(ic50_dat(), file, row.names = TRUE)
    }
  )
  output$ic50_res <- renderTable({
    req(ic50_dat())
    ic50_dat()
  }, rownames = TRUE)  # show row names

  
}

shinyApp(ui = ui, server = server)
