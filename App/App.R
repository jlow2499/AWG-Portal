library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(scales)
library(ggthemes)
library(DT)
library(rhandsontable)
library(plotly)
library(rjson)

Vendor_ <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/INPUT/Vendor_.csv")

currmonth = format(Sys.Date()-1,"%B %Y")
RGR.Database <- read.csv("//KNX3IT/AWG Management/RGR/RGR Database.csv", stringsAsFactors=FALSE)
NSABAN <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/AWG_AR.csv", stringsAsFactors=TRUE)
VPUTIN <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/AWG_CLIENT.csv", stringsAsFactors=TRUE)
NINE <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/Theta.csv", stringsAsFactors=TRUE)


# save the results to a file
saveData <- function(data) {
  fileName <- "ManualRGRs.csv"
  
  write.csv(x = data, file = file.path(responsesDir, fileName),
            row.names = FALSE, quote = TRUE)
}

loadData <- function() {
  if (exists("responses")) {
    responses
  }
}
RVW <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/ManualRGRs.csv", stringsAsFactors=FALSE)

responses <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/ManualRGRs.csv", stringsAsFactors=FALSE)
Graves <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/Graves.csv", stringsAsFactors=FALSE)
responses <- rbind(Graves,responses)
responses$Date <- as.Date(responses$Date,"%m/%d/%Y")
responses$Date <- format(responses$Date,"%m/%d/%Y")
responses <- responses[!duplicated(responses[c("File","Credit_AR","Date")]),]
responseout <- responses[responses$Checked == "Yes",]
responses <- responses[responses$Checked == "No",]
responsesDir <- file.path("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data")

Logged = FALSE;
PASSWORD <- data.frame(Brukernavn = c("KCRUZE","GHURD","JRODERMUND","JBELL","AHEINRICH","MROSE","JLOWHORN","THAMPTON","BGARRETT",'TBOLTON','AMENDES','MSCOTT'),
                       Passord = c("CRUZIN","GHURD2017","JRODERMUND2017","JBELL2017","AHEINRICH2017","MROSE2017","Rjys-548","DnGLJAmBAWG108","BGARRETT2017",'NEWDAY17','ASROMA10','MSCOTT2017'))


files <- RGR.Database[RGR.Database$Month %in% currmonth,]
files <- as.character(files$Debtor)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("AWG Portal",tabName="AWG",icon=icon("arrow-circle-right")),
    menuItem("Vendor Needs",tabName="vendor",
             menuSubItem("Vendor Table",tabName="vendortable"),
             menuSubItem("Pivot Table",tabName="vendorpivot"),
             menuSubItem("Vendor Data Download Tool","vendordownload"),
             menuSubItem("Sweep Tool",tabName="vendorsweep")
    ),
    menuItem("RGR Needs",tabName="RGR",
             menuSubItem("Pivot Table",tabName="rgrpivot"),
             menuSubItem("RGR Data Download Tool","rgrdownload"),
             menuSubItem("Sweep Tool",tabName="Sweep"),
             menuSubItem("Manual RGR Submission Tool",tabName="rgrsub"),
             menuSubItem("Manual RGR Approval Tool",tabName="rgrapproval"),
             menuSubItem("Manual RGR Approvals/Denials",tabName="Denials")),
    menuItem("Non RGR/Vendor POE Needs",tabName="POE",
             menuSubItem("POE Pivot",tabName="poepivot"),
             menuSubItem("POE Download",tabName="massdown")),
    menuItem("POE Vendor Search",tabName="joesearch"),
    menuItem("POE Mass Vendor Search",tabName="dirtbag"),
    menuItem("Not Swept Reason Search Tool",tabName='search'),
    menuItem("Mass Not Swept Reason Search",tabName="mass"),
    menuItem("Activations Report",tabName="ACTRPT",icon=icon("credit-card"),
             menuSubItem("AWG AR Breakdown",tabName="ARBRK"),
        #     menuSubItem("WG19 Worklist",tabName="WGWORK"),
             menuSubItem("MGR Statistics", tabName="MGRStats"),
             menuSubItem("Office Breakdown",tabName="OFFBRK"),
             menuSubItem("Activations to Rehab Report",tabName="ACTTORHB"),
             menuSubItem("Total Activations Chart",tabName="d3plot")
             ),
    menuItem('AWG KPI',tabName='KPI')
    
  )
)

body <- dashboardBody(
  tags$head(tags$style(
    type = 'text/css',
    '#test{ overflow-x: scroll; }'
  )),
  
  tabItems(
    tabItem(tabName="AWG",h1("Powered By:"),img(src='Capture.png')),
    
    tabItem(tabName="vendortable",
              dataTableOutput("vendorsummary")
                ),
    tabItem(tabName="vendorpivot",
            tags$head(
              tags$style(
                HTML("
                     #myScrollBox1{ 
                     overflow: auto;
                      height: 700px;
                     }
                     ")
                )
                ),
            fluidRow(
              div(id="myScrollBox1",     
                  rpivotTableOutput("pivot1")))
            
                ),
    tabItem(tabName="vendordownload",
            fluidRow(
              column(width=4,selectInput("vendoff","Select Office(s)",choices=levels(as.factor(Vendor_$Office)))),
              column(width=4,selectInput("vendvend","Select Vendor(s)",choices="",selected="",multiple=T)),
              column(width=4,selectInput("venddate","Select Batch Date(s)",choices="",selected="",multiple=T))
              
            ),
            fluidRow(column(width=5),downloadButton('VENDORDownload', 'Download Vendor Data'))
            
            
    ),
    
    tabItem(tabName="rgrpivot",
            tags$head(
              tags$style(
                HTML("
                     #myScrollBox2{ 
                      overflow: auto;
                      height: 700px;
                     }
                     ")
                )
                ),
            fluidRow(
              div(id="myScrollBox2",     
                  rpivotTableOutput("pivot2")))
            
                ),
    
    tabItem(tabName="rgrsub",
            fluidRow(column(width=12,box(width=12,title="Instructions","You will need to save your upload file #s as a CSV. Additionally the headers of the CSV file have to be exact. The headers you will need to include are: File, Credit_AR, and Reason. Please do not put SWEEPABLE for the reason, this tells us nothing about the account. Try to be as specific as possible so we do not spend our time searching for the reason. Once you have uploaded your file in the correct format a Submit button will appear. Click this button to submit your requests. To submit another request you will need to refresh your browser. Email Jeremiah Lowhorn if you have any questions or issues with this feature. "))),
            fluidRow(column(width=3),
                     column(width=3,fileInput("file2","Choose CSV File",accept="csv")),column(width=3,uiOutput("submits"))),
            
            
            dataTableOutput("form"),
            shinyjs::hidden(
              div(
                id = "save_msg2",
                h3("Thanks, your data has been saved!")
              )
            )
    ),
    
    tabItem(tabName="rgrapproval",
            bootstrapPage(
              tagList(
                tags$head(
                )
              ),
              
              ## Login module;
              div(class = "login",
                  uiOutput("uiLogin"),
                  textOutput("pass")
              ), 
              
              fluidRow(uiOutput("rgrsave")),
              rHandsontableOutput("hot"),
              shinyjs::hidden(
                div(
                  id = "save_msg",
                  h3("Thanks, your data has been saved!")
                )
              )
              
              
            )),
    tabItem(tabName="Denials",
            fluidRow(column(width=3,actionButton("Refresh","Refresh")),column(width=3,dateInput("DDate","Filter Date")),column(width=3,downloadButton('APPROVALSWEEP', 'Download All Manual RGRs'))),
            dataTableOutput("resout")
    ),
    
    
    
    tabItem(tabName="poepivot",
            tags$head(
              tags$style(
                HTML("
                     #myScrollBox{ 
                     overflow-y: scroll; 
                     overflow-x: scroll; 
                     height:700px;
                     }
                     ")
                )
                ),
            fluidRow(
              div(id="myScrollBox",     
                  rpivotTableOutput("pivot3")))
            
                ),
    tabItem(tabName="massdown",
            column(width=4),column(width=4,downloadButton('allpoedown', 'Download All POE Accounts'))),
    
    tabItem(tabName="search",
            fluidRow(numericInput("files","Enter File Number to Search:",value=Vendor_$File[1],min =1,max=9999999)),
            fluidRow(
              textOutput("reason")),
            fluidRow(textOutput("type"))),
    tabItem(tabName="mass",
            fluidRow(box(title="Instructions","You will need to save your upload file #s as a CSV. To do this paste your data with the header CM_FILENO into cell A1 in Excel. Once your files are in Excel click Save As and select CSV. The name of the file does not matter just as long as it is a CSV. Once you have uploaded your data you will be able to redownload the data which will include the reason AWG cannot sweep the account and if it is a Vendor, RGR, or regular POE file. Any account with a POE in the 2 screen can be searched with this tool.")),
            fluidRow(column(width=4,fileInput("file1","Choose CSV File",accept="csv")),
                     column(width=1),
                     column(width=4, downloadButton('updown', 'Download Your Data!'),textOutput("jlow"))),
            fluidRow(dataTableOutput("test"))
    ),
    
    tabItem(tabName="vendorsweep",
            bootstrapPage(
              
              tagList(
                tags$head(
                )
              ),
              
              ## Login module;
              div(class = "login",
                  uiOutput("uiLogin2"),
                  textOutput("pass2")
              ), 
              
              
              
              fluidRow(column(width=3),column(width=3,uiOutput("VENDSWEEPTOOL")),
                       column(width=3,uiOutput("VENDORDWNTOOL"))
                       
              ),
              dataTableOutput("VENDORSWEEP")
            )
            
    ),
    tabItem(tabName="Sweep",
            bootstrapPage(
              
              tagList(
                tags$head(
                )
              ),
              
              ## Login module;
              div(class = "login",
                  uiOutput("uiLogin3"),
                  textOutput("pass3")
              ), 
              
              
              
              fluidRow(column(width=3),column(width=3,uiOutput("RGRSWEEPTOOL")),
                       column(width=3,uiOutput("RGRDWNTOOL"))
                       
              ),
              dataTableOutput("RGRSWEEP"),
              shinyjs::hidden(
                div(
                  id = "save_msg3",
                  h3("Thanks, your sweeps have been saved to the database!")
                )
              )
            )
    ),
    tabItem(tabName="rgrdownload",
            column(width=4),column(width=4,downloadButton('sweepnrgr', 'Download All RGR Accounts'))),
    
    tabItem(tabName="ARBRK",
            fluidRow(
              column(width=4,h2(textOutput("TimeZ"))),
              column(width=4,
                     selectInput("armonth","Select Month",choices=c("October 2016","November 2016","December 2016","January 2017","February 2017",
                                                                    "March 2017","April 2017","May 2017","June 2017","July 2017", 'August 2017',
                                                                    'September 2017','October 2017', "No WG19 Sent"),selected=format(Sys.Date()-1,"%B %Y"))
                     )
              ),
            fluidRow(
            dataTableOutput("SABAN")
            )
            
            
            ),
    tabItem(tabName="OFFBRK",
    fluidRow(
      column(width=4),
      column(width=4,
             selectInput("ofmonth","Select Month",c("October 2016","November 2016","December 2016","January 2017","February 2017",
                                                    "March 2017","April 2017","May 2017","June 2017","July 2017",'August 2017',
                                                    'September 2017','October 2017'),selected=format(Sys.Date()-1,"%B %Y"))
      )
    ),
    fluidRow(
      dataTableOutput("PUTIN")
    )),
    tabItem(tabName="ACTTORHB",
            plotlyOutput("plot")),
    tabItem(tabName="d3plot",
   
          fluidRow(column(width=4),h2("Activations by Month Faceted by Office")),
           fluidRow(
            includeHTML("index.html")
           ),
           fluidRow(h2("Learn D3.js at:",
                        tags$a(href="https://d3js.org/","d3js.org")))
    
            ),
    tabItem(tabName="MGRStats",
            fluidRow(column(width=4),
                     column(width=4,
                            selectInput(inputId="MGRStatsMth",label="Select Month",choices=c("October 2016","November 2016","December 2016","January 2017","February 2017",
                                                                                             "March 2017","April 2017","May 2017","June 2017","July 2017",'August 2017',
                                                                                             'September 2017','October 2017'),selected=format(Sys.Date()-1,"%B %Y"))
                            )),
            fluidRow(dataTableOutput("jimmybuffet"))
            

            ),
    tabItem(tabName="joesearch",
            fluidRow(numericInput("filez","Enter File Number to Search:",value=Vendor_$File[1],min =1,max=9999999)),
                DT::dataTableOutput("joetable")
            
            
    ),
    
    tabItem(tabName="WGWORK",
            DT::dataTableOutput("wg19worklist")),
    tabItem(tabName="KPI",
            DT::dataTableOutput("lowkpi"))

    ))





ui <- dashboardPage(
  dashboardHeader(title="AWG Portal",
                  dropdownMenuOutput("messageMenu"))
  ,
  sidebar,
  body,
  shinyjs::useShinyjs()
)

server <- function(input, output,session) {
  
  
  kpi <- reactive({
    gentry <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/gentry.csv",
                       stringsAsFactors=FALSE)
    gentry <- gentry %>% mutate(Resolution = Percent_W_GAR + Percent_Paying)
    gentry <- plyr::rename(gentry,c('Days_For_GAR'='Days for GAR',
                                    'Accounts_W_GAR'='Accounts With GAR',
                                    'Percent_W_GAR'='Percent With GAR',
                                    'Percent_Paying'='Percent Paying',
                                    'Resolution'='Total Resolution Rate'))
    
    gentry
        
  })
  
  output$lowkpi <- DT::renderDataTable({
    table <- datatable(kpi(),extensions = c('FixedColumns','Buttons','ColReorder','KeyTable','RowReorder','Scroller'), rownames=FALSE,class = 'cell-border stripe',filter="top",
                       options = list(
                         searching=TRUE,
                         autoWidth=TRUE,
                         keys=TRUE,
                         scroller=T,
                         scrollY=450,
                         rowReorder=T,
                         colReorder = TRUE,
                         paging=T,
                         dom = 'Bfrtip',
                         scrollX=T,
                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print',I('colvis'))
                       ))
    
    table <- formatPercentage(table,"Total Resolution Rate",digits=2)
    table <- formatPercentage(table,"Percent Paying",digits=2)
    table <- formatPercentage(table,"Percent With GAR",digits=2)
    
    
    
    table
  }) 
  
  
  
  responses <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/ManualRGRs.csv", stringsAsFactors=FALSE)
  Graves <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/Graves.csv", stringsAsFactors=FALSE)
  responses <- rbind(Graves,responses)
  responses$Date <- as.Date(responses$Date,"%m/%d/%Y")
  responses$Date <- format(responses$Date,"%m/%d/%Y")
  responses <- responses[!duplicated(responses[c("File","Credit_AR","Date")]),]
  responseout <- responses[responses$Checked == "Yes",]
  responses <- responses[responses$Checked == "No",]
  
  
  observeEvent(input$vendoff,
               updateSelectInput(session,"vendvend","Select Vendor(s)",
                                 choices=as.character(Vendor_$Vendor[Vendor_$Office==input$vendoff]),
                                 selected=as.character(Vendor_$Vendor[Vendor_$Office==input$vendoff])
               ))
  
  string1 <- reactive({
    a <- c(as.character(input$vendvend[1]),as.character(input$vendvend[2]),as.character(input$vendvend[3]),as.character(input$vendvend[4]))
    a<- a[!is.na(a)]
  })
  
  observeEvent(input$vendvend,
               updateSelectInput(session,"venddate","Select Batch Date(s)",
                                 choices=as.character(Vendor_$Batch.Date[Vendor_$Office==input$vendoff & Vendor_$Vendor%in%string1()]),
                                 selected=as.character(Vendor_$Batch.Date[Vendor_$Office==input$vendoff & Vendor_$Vendor%in%string1()])
               ))
  string2 <- reactive({
    a <- c(as.character(input$venddate[1]),as.character(input$venddate[2]),as.character(input$venddate[3]),as.character(input$venddate[4]),as.character(input$venddate[5]),
           as.character(input$venddate[6]),as.character(input$venddate[7]),as.character(input$venddate[8]),as.character(input$venddate[9]),as.character(input$venddate[10]),
           as.character(input$venddate[11]),as.character(input$venddate[12]),as.character(input$venddate[13]),as.character(input$venddate[14]),as.character(input$venddate[15]),
           as.character(input$venddate[16]))
    a <- a[!is.na(a)]
    
    
  })
  
  
  
  
  vendorDWNDATA <- reactive({
    a<- Vendor_[Vendor_$Office==input$vendoff & Vendor_$Vendor%in%string1() & Vendor_$Batch.Date %in% string2(),]
    a
  })
  
  output$VENDORDownload <- downloadHandler(
    filename = function() { 
      paste("VendorData", '.csv', sep='') 
    },
    content = function(file) {
      write.csv(vendorDWNDATA(), file)
    }
  )
  
  
  searchs <- reactive({
    SearchD <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/INPUT/SearchD.csv", stringsAsFactors=FALSE)
    
    a <- SearchD[SearchD$CM_FILENO %in% input$files,]
    a
    
  })
  
  output$reason <- renderText({
    paste0("This has not been swept because:"," ",searchs()$Reason)
  })
  output$type <- renderText({
    paste0("This is a"," ",searchs()$Type," ","file.")
  })
  
  lowvendpiv <- reactive({
    VendorPivot <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/INPUT/VendorPivot.csv", stringsAsFactors=FALSE)
    VendorPivot
    
  })
  
  
  output$pivot1 <- renderRpivotTable({
    rpivotTable(data = lowvendpiv(),
                rows=c("Office","Manager.Name","Supervisor.Name"),
                cols="Reason",
                width="50%")
  })
  
  lohornrgrpivot <- reactive({
    RGRPivot <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/INPUT/RGRPivot.csv", stringsAsFactors=FALSE)
    RGRPivot
    
  })
  
  output$pivot2 <- renderRpivotTable({
    rpivotTable(data = lohornrgrpivot(),
                rows=c("off","Manager.Name","Supervisor.Name"),
                cols="Reason",
                width="50%")
  })
  
  lowpoepivot <- reactive({
    POEPivot <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/INPUT/POEPivot.csv", stringsAsFactors=FALSE)
    POEPivot
    })
  
  
  output$pivot3 <- renderRpivotTable({
    
    rpivotTable(data = lowpoepivot(),
                rows=c("CM_CLIENT","Manager.Name","Supervisor.Name"),
                cols="Reason",
                width="50%")
  })
  
  
  summs <- reactive({
    Summary <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/Summary.csv",stringsAsFactors=T)
    Summary <- plyr::rename(Summary,c("Batch.Date"="Batch Date",
                                      "Files.Received"="Files Received",
                                      "Files.Activated"="Files Activated",
                                      "Percent.Activated"="Percent Activated",
                                      "Voluntary.Payers"="Voluntary Payers",
                                      "Percent.Paying"="Percent Paying",
                                      "Orders.Sent"="Orders Sent",
                                      "Activated.With.Order"="Activated With Order",
                                      "GAR.Payment.in.Last.30.Days"="GAR Payment in Last 30 Days",
                                      "GAR.Payment.in.Last.90.Days"="GAR Payment in Last 90 Days"))
    
    Summary
  })

  output$vendorsummary <- DT::renderDataTable({
    table <- datatable(summs(),extensions = c('FixedColumns','Buttons','ColReorder','KeyTable','RowReorder','Scroller'), rownames=FALSE,class = 'cell-border stripe',filter="top",
                       options = list(
                         searching=TRUE,
                         autoWidth=TRUE,
                         keys=TRUE,
                         scroller=T,
                         scrollY=450,
                         rowReorder=T,
                         colReorder = TRUE,
                         paging=T,
                         dom = 'Bfrtip',
                         scrollX=T,
                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print',I('colvis'))
                       ))
    
    table <- formatPercentage(table,"Percent Activated",digits=2)
    table <- formatPercentage(table,"Percent Paying",digits=2)
    table <- formatPercentage(table,"Activated With Order",digits=2)
    
    table
  })
  
  upload <- reactive({
    SearchD <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/INPUT/SearchD.csv", stringsAsFactors=FALSE)
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    a <- read.csv(inFile$datapath, header=T, sep=",")
    SearchD$CM_FILENO  <- as.character(SearchD$CM_FILENO)
    a$CM_FILENO <- as.character(a$CM_FILENO)
    a<- left_join(a,SearchD,by="CM_FILENO")
    a <- a[,!names(a) %in% c("CM_FILENO.x","CM_FILENO.y")]
    a$Type <- as.factor(a$Type)
    a$Reason <- as.factor(a$Reason)
    a
    
  })
  
  output$test <- DT::renderDataTable({
    table <- datatable(upload(),extensions = c('FixedColumns','Buttons','ColReorder','KeyTable','RowReorder','Scroller'), rownames=FALSE,class = 'cell-border stripe',filter="top",
                       options = list(
                         searching=TRUE,
                         autoWidth=TRUE,
                         keys=TRUE,
                         scroller=T,
                         scrollY=450,
                         rowReorder=T,
                         colReorder = TRUE,
                         paging=T,
                         dom = 'Bfrtip',
                         scrollX=T,
                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print',I('colvis'))
                       ))
    
    table
  })
  
  output$updown <- downloadHandler(
    filename = function() { 
      paste("VendorData", '.csv', sep='') 
    },
    content = function(file) {
      write.csv(upload(), file)
    }
  )
  
  
  upload2 <- reactive({
    
    inFile <- input$file2
    
    if (is.null(inFile))
      return(NULL)
    
    a <- read.csv(inFile$datapath, header=T, sep=",")
    
    if(names(select(a,File)) == "File" & names(select(a,Credit_AR)) == "Credit_AR" & names(select(a,Reason)) == "Reason"){
      a <- select(a,File,Credit_AR,Reason)
      a$Date <- format(Sys.Date(),"%m/%d/%Y")
      a$Response <- "No"
      a$Reason_Denied <- "Null"
      a$Checked <- "No"
      a
    }else{
      a <- NULL
    }
  })
  
  
  output$submits <- renderUI({
    if (!is.null(upload2())) {
      actionButton("Sub","Submit")
      
    }
  })
  
  finalD <- reactive({
    
    inFile <- input$file2
    
    if (is.null(inFile))
      return(NULL)
    
    a <- read.csv(inFile$datapath, header=T, sep=",")
    
    
    a <- select(a,File,Credit_AR,Reason)
    a$Date <- format(Sys.Date(),"%m/%d/%Y")
    a$Response <- "No"
    a$Reason_Denied <- "Null"
    a$Checked <- "No"
    a <- rbind(responseout,responses,a)    
    a
    
    
    
  })
  
  observeEvent(input$file2,{
    responses <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/ManualRGRs.csv", stringsAsFactors=FALSE)
    Graves <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/Graves.csv", stringsAsFactors=FALSE)
    responses <- rbind(Graves,responses)
    responses$Date <- as.Date(responses$Date,"%m/%d/%Y")
    responses$Date <- format(responses$Date,"%m/%d/%Y")
    responses <- responses[!duplicated(responses[c("File","Credit_AR","Date")]),]
    responseout <- responses[responses$Checked == "Yes",]
    responses <- responses[responses$Checked == "No",]
  })
  
  
  
  observeEvent(input$Sub,{
    saveData(finalD())
    responses <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/ManualRGRs.csv", stringsAsFactors=FALSE)
    Graves <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/Graves.csv", stringsAsFactors=FALSE)
    responses <- rbind(Graves,responses)
    responses$Date <- as.Date(responses$Date,"%m/%d/%Y")
    responses$Date <- format(responses$Date,"%m/%d/%Y")
    responses <- responses[!duplicated(responses[c("File","Credit_AR","Date")]),]
    responseout <- responses[responses$Checked == "Yes",]
    responses <- responses[responses$Checked == "No",]
  })
  
  
  output$form <- DT::renderDataTable({
    table <- datatable(upload2(),extensions = c('FixedColumns','Buttons','ColReorder','KeyTable','RowReorder','Scroller'), rownames=FALSE,class = 'cell-border stripe',filter="top",
                       options = list(
                         searching=TRUE,
                         autoWidth=TRUE,
                         keys=TRUE,
                         scroller=T,
                         scrollY=450,
                         rowReorder=T,
                         colReorder = TRUE,
                         paging=T,
                         dom = 'Bfrtip',
                         scrollX=T,
                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print',I('colvis'))
                       ))
    table
  })
  
  
  
  
  
  
  source("www/Login.R",  local = TRUE)
  
  observe({
    if (USER$Logged == TRUE) {
      output$rgrsave <- renderUI({
        actionButton("Save","Save Table")
      })
      
      responses <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/ManualRGRs.csv", stringsAsFactors=FALSE)
      Graves <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/Graves.csv", stringsAsFactors=FALSE)
      responses <- rbind(Graves,responses)
      responses$Date <- as.Date(responses$Date,"%m/%d/%Y")
      responses$Date <- format(responses$Date,"%m/%d/%Y")
      responses <- responses[!duplicated(responses[c("File","Credit_AR","Date")]),]
      responseout <- responses[responses$Checked == "Yes",]
      responses <- responses[responses$Checked == "No",]
      
      
      values <- reactiveValues()
      observe({
        if (!is.null(input$hot)) {
          values[["previous"]] <- isolate(values[["responses"]])
          responses = hot_to_r(input$hot)
        } else {
          if (is.null(values[["responses"]]))
            responses <- responses[!duplicated(responses[c("File","Credit_AR","Date")]),]
         
          else
            responses <- values[["responses"]]
          responses <- responses[!duplicated(responses[c("File","Credit_AR","Date")]),]
        }
        values[["responses"]] <- responses[!duplicated(responses[c("File","Credit_AR","Date")]),]
      })
      
      output$hot <- renderRHandsontable({
        a <- responses[!duplicated(responses[c("File","Credit_AR","Date")]),]
        rhandsontable(responses)
      })
      observeEvent(input$Save, {
        finalDF <- isolate(values[["responses"]])
        finalDF <- rbind(finalDF,responseout)
        write.csv(finalDF, file = file.path(responsesDir, "Graves.csv"),
                  row.names = FALSE, quote = TRUE)
        
   #     Sys.sleep(2)
        
    #    responses <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/ManualRGRs.csv", stringsAsFactors=FALSE)
     #   Graves <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/Graves.csv", stringsAsFactors=FALSE)
      #  responses <- rbind(Graves,responses)
       # responses$Date <- as.Date(responses$Date,"%m/%d/%Y")
        #responses$Date <- format(responses$Date,"%m/%d/%Y")
        #responses <- responses[!duplicated(responses[c("File","Credit_AR","Date")]),]
        #Sys.sleep(1)
        #write.csv(responses,"//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/ManualRGRs.csv",row.names=F)
              
      })
      
      observeEvent(input$Save,{
        
        shinyjs::hide("hot")
        shinyjs::hide("rgrsave")
        shinyjs::show("save_msg")
        
      })
      
      
    }})
  
  
  observeEvent(input$Refresh, {
   # Sys.sleep(1)
    responses <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/ManualRGRs.csv", stringsAsFactors=FALSE)
    Graves <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/Graves.csv", stringsAsFactors=FALSE)
    responses <- rbind(Graves,responses)
    responses$Date <- as.Date(responses$Date,"%m/%d/%Y")
    responses$Date <- format(responses$Date,"%m/%d/%Y")
    responses <- responses[!duplicated(responses[c("File","Credit_AR","Date")]),]
    responseout <- responses[responses$Checked == "Yes",]
    responses <- responses[responses$Checked == "No",]
    shinyjs::reset("resout")
    
  })
  
  
  resouttable <- reactive({
    ARMAST <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/INPUT/ARMAST.csv")
    ARMAST$Credit_AR <- as.character(ARMAST$Credit_AR)
    a <- responseout
    a$Credit_AR <- as.character(a$Credit_AR)
    a$Response <- as.factor(a$Response)
    a <- left_join(a,ARMAST,by="Credit_AR")
    a$Date <- as.Date(a$Date,"%m/%d/%Y")
    a <- a[a$Date == as.character(input$DDate),]
    a <- a[!duplicated(a[c("File","Credit_AR","Reason","Date")]),]
    a
    
  })
  
  
  output$resout <- DT::renderDataTable({
    table <- datatable(resouttable(),extensions = c('FixedColumns','Buttons','ColReorder','KeyTable','RowReorder','Scroller'), rownames=FALSE,class = 'cell-border stripe',filter="top",
                       options = list(
                         searching=TRUE,
                         autoWidth=TRUE,
                         keys=TRUE,
                         scroller=T,
                         scrollY=450,
                         rowReorder=T,
                         colReorder = TRUE,
                         paging=T,
                         dom = 'Bfrtip',
                         scrollX=T,
                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print',I('colvis'))
                       ))
    
    table
  })
  
  observeEvent(input$Sub,{
    
    shinyjs::hide("form")
    shinyjs::hide("file2")
    shinyjs::hide("Sub")
    shinyjs::show("save_msg2")
    
  })
  
  
  
  
  
  
  source("www/Login2.R",  local = TRUE)
  
  observe({
    if (USER2$Logged == TRUE) {
      
      output$VENDSWEEPTOOL <- renderUI({  
        
        actionButton("vendSweep","Sweep!")
      }) 
      
      output$VENDORDWNTOOL <- renderUI({
        
        downloadButton("vendsweepdwn","Download Sweeps")
      })
      
  lowvendsweep <- reactive({
    vendorsweep <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/INPUT/vendorsweep.csv", stringsAsFactors=FALSE)
    vendorsweep
    
  })
      
      output$VENDORSWEEP <- DT::renderDataTable({
        table <- datatable(lowvendsweep(),extensions = c('FixedColumns','Buttons','ColReorder','KeyTable','RowReorder','Scroller'), rownames=FALSE,class = 'cell-border stripe',filter="top",
                           options = list(
                             searching=TRUE,
                             autoWidth=TRUE,
                             keys=TRUE,
                             scroller=T,
                             scrollY=450,
                             rowReorder=T,
                             colReorder = TRUE,
                             paging=T,
                             dom = 'Bfrtip',
                             scrollX=T,
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print',I('colvis'))
                           ))
        
        table
      })
      
      
      
    }
  })
  
  lowvendsweep2 <- reactive({
    vendorsweep <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/INPUT/vendorsweep.csv", stringsAsFactors=FALSE)
    vendorsweep
    
  })
  
  
  output$vendsweepdwn <- downloadHandler(
    filename = function() { 
      paste("VendorSweeps", '.csv', sep='') 
    },
    content = function(file) {
      write.csv(lowvendsweep2(), file)
    }
  )
  
  
  
  
  
  
  
  
  source("www/Login3.R",  local = TRUE)
  
lowrgrsweep <- reactive({
  
  rgrsweep <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/INPUT/rgrsweep.csv", stringsAsFactors=FALSE)
  rgrsweep <- rename(rgrsweep,Debtor=CM_FILENO,CRAR=ED_AWG_CREDIT_AR,Vendor.File.=Vendor)
  rgrsweep <- select(rgrsweep,Month,Debtor, CRAR,Vendor.File.)
  
  
  rgrsweep
})
  
  observe({
    if (USER3$Logged == TRUE) {
      
      output$RGRSWEEPTOOL <- renderUI({  
        
        actionButton("rgrSweep","Sweep!")
      }) 
      
      output$RGRDWNTOOL <- renderUI({
        
        downloadButton("rgrsweepdwn","Download Sweeps")
      })
      
      
      
      output$RGRSWEEP <- DT::renderDataTable({
        table <- datatable(lowrgrsweep(),extensions = c('FixedColumns','Buttons','ColReorder','KeyTable','RowReorder','Scroller'), rownames=FALSE,class = 'cell-border stripe',filter="top",
                           options = list(
                             searching=TRUE,
                             autoWidth=TRUE,
                             keys=TRUE,
                             scroller=T,
                             scrollY=450,
                             rowReorder=T,
                             colReorder = TRUE,
                             paging=T,
                             dom = 'Bfrtip',
                             scrollX=T,
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print',I('colvis'))
                           ))
        
        table
      })
      
      observeEvent(input$rgrSweep,{
      
        RGR.Database <- read.csv("//KNX3IT/AWG Management/RGR/RGR Database.csv", stringsAsFactors=FALSE)     
        RGRs <- rbind(RGR.Database,lowrgrsweep())
                
        RGRs <- RGRs[!duplicated(RGRs[c("Month","Debtor","CRAR")]) ,]
        
        shinyjs::hide("rgrSweep")
        shinyjs::hide("RGRSWEEP")
        shinyjs::hide("rgrsweepdwn")
        shinyjs::show("save_msg3")
        write.csv(RGRs,"//KNX3IT/AWG Management/RGR/RGR Database.csv",row.names=FALSE)
        
      })
      
      
      
    }
  })
  
  
  
  output$rgrsweepdwn <- downloadHandler(
    filename = function() { 
      paste("RGRSweeps", '.csv', sep='') 
    },
    content = function(file) {
      write.csv(lowrgrsweep(), file)
    }
  )
  
  
  output$APPROVALSWEEP <- downloadHandler(
    filename = function() { 
      paste("Manual Submissions", '.csv', sep='') 
    },
    content = function(file) {
      write.csv(RVW, file)
    }
  )
  
  saban <- reactive({
    NSABAN <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/AWG_AR.csv", stringsAsFactors=TRUE)
    
    NSABAN <-  select(NSABAN,A.R, manager,Month, InitiatedActivations,MonthlyActivations,Efficiency,
                               TotalGAR,TotalVol,TotalCollected,Resolutions,
                                SuccessRate)
    
    NSABAN <- plyr::rename(NSABAN,c("InitiatedActivations"="Initiated Activations",
                                    "MonthlyActivations"="Activations Towards Budget",
                                    "TotalGAR"="Total GAR $",
                                    "TotalVol"="Total Voluntary Direct",
                                    "TotalCollected"="Total Direct Collect",
                                    "Resolutions"="Total Payers",
                                    "SuccessRate"="Success Rate",
                                    "A.R"="Collector",
                                    "manager"="Manager"))
   
    NSABAN <- NSABAN[NSABAN$Month == input$armonth,]
    
    NSABAN
    
  })
  
  
  output$SABAN <- DT::renderDataTable({
    
    
    table <- datatable(saban(),extensions = c('FixedColumns','Buttons','ColReorder','KeyTable','RowReorder','Scroller'), rownames=FALSE,class = 'cell-border stripe',filter="top",
                       options = list(
                         searching=TRUE,
                         autoWidth=TRUE,
                         keys=TRUE,
                         scroller=T,
                         scrollY=450,
                         rowReorder=T,
                         colReorder = TRUE,
                         paging=T,
                         dom = 'Bfrtip',
                         scrollX=T,
                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print',I('colvis'))
                       ))
    
    table <- formatCurrency(table,"Total GAR $")
    table <- formatCurrency(table,"Total Voluntary Direct")
    table <- formatCurrency(table,"Total Direct Collect")
    table <- formatPercentage(table,"Success Rate",digits=2)
    table <- formatPercentage(table,"Efficiency",digits=2)
    
    table
  })
  
  putin <- reactive({
    
    VPUTIN <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/AWG_CLIENT.csv", stringsAsFactors=TRUE)
    
    
    VPUTIN <- VPUTIN %>% select(CM_CLIENT,Month, MonthlyActivations,TotalActivations,
                                OW1_Sent,OW2_Sent,TotalGAR,TotalVol,TotalCollected,Resolutions,
                                SuccessRate)
    
    VPUTIN <- plyr::rename(VPUTIN,c("CM_CLIENT"="Client",
                                    "MonthlyActivations"="Activations",
                                    "TotalActivations"="Activations Since Start",
                  
                                    "OW1_Sent"="First Orders Sent",
                                    "OW2_Sent"="Second Orders Sent",
                                    "TotalGAR"="Total GAR $",
                                    "TotalVol"="Total Voluntary Direct",
                                    "TotalCollected"="Total Direct Collect",
                                    "Resolutions"="Total Payers",
                                    "SuccessRate"="Success Rate"))
    VPUTIN <- VPUTIN[VPUTIN$Client %in% c("7222","C7222","B7222","W7222","S7222","8035"),]
    VPUTIN <- VPUTIN[!is.na(VPUTIN$Month),]
    VPUTIN <- VPUTIN[VPUTIN$Month == input$ofmonth,]
    
    VPUTIN
    
  })
  
  
  output$PUTIN <- DT::renderDataTable({
    table <- datatable(putin(),extensions = c('FixedColumns','Buttons','ColReorder','KeyTable','RowReorder','Scroller'), rownames=FALSE,class = 'cell-border stripe',filter="top",
                       options = list(
                         searching=TRUE,
                         autoWidth=TRUE,
                         keys=TRUE,
                         scroller=T,
                         scrollY=450,
                         rowReorder=T,
                         colReorder = TRUE,
                         paging=T,
                         dom = 'Bfrtip',
                         scrollX=T,
                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print',I('colvis'))
                       ))
    
    table <- formatCurrency(table,"Total GAR $")
    table <- formatCurrency(table,"Total Voluntary Direct")
    table <- formatCurrency(table,"Total Direct Collect")
    table <- formatPercentage(table,"Success Rate",digits=2)
    
    
    table
  })  
  
  
  
  
  
  lowhornrgr <- reactive({
    RGR <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/INPUT/RGR_.csv", stringsAsFactors=FALSE)
    RGR
  })
  
  
  
  output$sweepnrgr <- downloadHandler(
    filename = function() { 
      paste("Manual Submissions", '.csv', sep='') 
    },
    content = function(file) {
      write.csv(lowhornrgr(), file)
    }
  )
jlowpoe <- reactive({
  
  POE <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/INPUT/POE_.csv", stringsAsFactors=FALSE)
})  

  output$allpoedown <- downloadHandler(
    
    filename = function() { 
      paste("Master_POE", '.csv', sep='') 
    },
    content = function(file) {
      write.csv(jlowpoe(), file)
    }
  )
  
  

  
  output$plot <- renderPlotly({
    NINE <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/Theta.csv", stringsAsFactors=TRUE)
    
    p <- NINE %>% count(Month, RHBMONTH)
    p <- p[!p$RHBMONTH %in%"",]
    
    acts <- NINE %>%
      group_by(Month) %>%
      summarize(Activations = n())
    
    p <- left_join(p,acts,by="Month")
    rm(acts)
    
    p$Month <- factor(p$Month,levels= c("January 2017",
                                        "February 2017","March 2017",
                                        "April 2017","May 2017",
                                        "June 2017","July 2017",'August 2017',
                                        'September 2017','October 2017'))
    p$RHBMONTH <- factor(p$RHBMONTH,levels= c("January 2017",
                                              "February 2017","March 2017",
                                              "April 2017","May 2017",
                                              "June 2017","July 2017",'August 2017',
                                              'September 2017','October 2017'))
    
    p <- p[!is.na(p$RHBMONTH),]
    p <- p[!is.na(p$Month),]
    p <- p %>%
      mutate(Percent = n/Activations)
    
   p <- ggplot(p,aes(x=RHBMONTH,y=n,fill=RHBMONTH)) + geom_bar(stat = "identity") + facet_grid(.~Month) +
      xlab("") + ylab("") + 
     theme(axis.title=element_text(size=8,color="black")) + theme_igray() + 
      scale_colour_tableau() + ggtitle("Rehabs by Activation Month") + theme(axis.text.x = element_text(angle = 90)) +
     theme(plot.margin = unit(c(1,1,1,1), "cm"))

     
    
    plot <- (p)
    plot
  })
  
  observeEvent(input$run,{
    
    data <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Application File/www/data.csv")
    data <- toJSON(data)
    session$sendCustomMessage(type="jsondata",data)
    
    })
  
  longjohnsilver <- reactive({
    MGRStats <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/MGRStats.csv")
    MGRStats <- MGRStats[MGRStats$Month %in% input$MGRStatsMth,]
    MGRStats <- select(MGRStats,-TotalCollected,-TotalVol,-TotalGAR)
    MGRStats <- plyr::rename(MGRStats,c("manager"="Manager",
                                        "Monthly_Activations"="Monthly Activations",
                                        "TotalActivations"="Total Activations",
                                        "WG19_Sent"="WG19 Sent",
                                        "OW1_Sent" = "OW1 Sent",
                                        "OW2_Sent" = "OW2 Sent",
                                        "SuccessRate" = "Success Rate"))
  
  })
  
  
  output$jimmybuffet <- DT::renderDataTable({
    table <- datatable(longjohnsilver(),extensions = c('FixedColumns','Buttons','ColReorder','KeyTable','RowReorder','Scroller'), rownames=FALSE,class = 'cell-border stripe',filter="top",
                       options = list(
                         searching=TRUE,
                         autoWidth=TRUE,
                         keys=TRUE,
                         scroller=T,
                         scrollY=450,
                         rowReorder=T,
                         colReorder = TRUE,
                         paging=T,
                         dom = 'Bfrtip',
                         scrollX=T,
                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print',I('colvis'))
                       ))
    

    table <- formatPercentage(table,"Success Rate",digits=2)
    
    
    table
  })  
  
  output$TimeZ <- renderText({
    time <- read.table("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/time.csv", header=TRUE, dec=",", quote="\"", stringsAsFactors=FALSE)
    time <- time[1,1]
    time
  })
  
  
  joe <- reactive({
    POEMAST <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/POEMAST.csv", stringsAsFactors=F)
    POEMAST <- POEMAST[POEMAST$FILE == input$filez,]
   # POEMAST <- select(POEMAST -BATCH)
    POEMAST <- plyr::rename(POEMAST,c("CM_CLIENT"="Client",
                                      "TYPE"="Type",
                                      "EMP.VER"="Emp Ver",
                                      "FILE"="File",
                                      "EMPLOYER_NAME"="Employer",
                                      "EMPLOYER_ADDRESS1"="Address 1",
                                      "EMPLOYER_ADDRESS2"="Address 2",
                                      "EMPLOYER_CITY"="City",
                                      "EMPLOYER_STATE"="State",
                                      "EMPLOYER_POSTAL_CODE"="Zip",
                                      "POE.PHONE"="Phone"))
   POEMAST <- POEMAST[,-2]
   
  })
  
  output$joetable <- DT::renderDataTable({
    table <- datatable(joe(),extensions = c('FixedColumns','Buttons','ColReorder','KeyTable','RowReorder','Scroller'), rownames=FALSE,class = 'cell-border stripe',filter="top",
                       options = list(
                         searching=TRUE,
                         autoWidth=TRUE,
                         keys=TRUE,
                         scroller=T,
                         scrollY=450,
                         rowReorder=T,
                         colReorder = TRUE,
                         paging=T,
                         dom = 'Bfrtip',
                         scrollX=T,
                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print',I('colvis'))
                       ))
  
    
    
    table
  })  
  
  nineteenwork <- reactive({
    WorkList <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/WorkList.csv")
    WorkList$ED_AWG_CREDIT_AR <- as.factor(as.character(WorkList$ED_AWG_CREDIT_AR))
    WorkList$ED_ACCT_ACT_DATE <- as.Date(WorkList$ED_ACCT_ACT_DATE,"%Y-%m-%d")
    WorkList
  })
  
  output$wg19worklist <- DT::renderDataTable({
    table <- datatable(nineteenwork(),extensions = c('FixedColumns','Buttons','ColReorder','KeyTable','RowReorder','Scroller'), rownames=FALSE,class = 'cell-border stripe',filter="top",
                       options = list(
                         searching=TRUE,
                         autoWidth=TRUE,
                         keys=TRUE,
                         scroller=T,
                         scrollY=450,
                         rowReorder=T,
                         colReorder = TRUE,
                         paging=T,
                         dom = 'Bfrtip',
                         scrollX=T,
                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print',I('colvis'))
                       ))
    
    
    
    table
  })  
  

  
}
shinyApp(ui, server)
