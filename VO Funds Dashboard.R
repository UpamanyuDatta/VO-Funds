setwd("C:/Users/Upamanyu/Documents/R Codes/VO Funds New/Dashboard")
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe") 

rm(list=ls())

library(dplyr)
library(ggplot2)
library(rgdal)
library(rgeos)
library(maptools)
library(rio)
library(magrittr)
library(highcharter)
library(DT)
library(htmlwidgets)
library(leaflet)
library(shiny)
library(shinythemes)

load("Cooked.RData")

Block_Facesheet<-mutate(Block_Facesheet, `Geography`=`Block`)%>%
  arrange(`District`, `Block`)
Block<-arrange(Block, `District`, `Block`)
District_Facesheet<-mutate(District_Facesheet, `Geography`=`District`)%>%
  arrange(`District`)
District<-arrange(District, `District`)

SHG_Recoveries<-arrange(SHG_Recoveries, `District`, `Block`, `CLF`, `VO`, `SHG`)
SHG_Vouchers<-arrange(SHG_Vouchers, `District`, `Block`, `CLF`, `VO`, `SHG`)

VO_Funds<-arrange(VO_Funds, `District`, `Block`, `CLF`, `VO`)
VO_Recoveries<-arrange(VO_Recoveries, `District`, `Block`, `CLF`, `VO`)

CLF_Level<-arrange(CLF_Level, District, Block, CLF)

x=c("Fund Available (Avg)", "Income (Avg)", "ICF Rotation (Avg)", "HRF Rotation (Avg)", "FSF Rotation (Avg)", "VO Repayment (Avg).ICF", "SHG Repayment (Avg).ICF",  "VO Recovery (Avg).HRF", "SHG Recovery (Avg).HRF", "VO Recovery (Avg).FSF", "SHG Recovery (Avg).FSF", "VO Recovery (Avg).GENERAL", "SHG Recovery (Avg).GENERAL")
y=c( "Extreme VO Monitoring", "Fund Available", "Income", "Funds: Not Delivered", "Funds: Not Delivered @ SHG", "Funds: Diverted", "Funds: Excess", "Performance: ICF", "Performance: ICF Loans", "Performance: ICF Loans @ SHG", "Performance: HRF", "Performance: HRF Loans", "Performance: HRF Loans @ SHG", "Performance: FSF", "Performance: FSF Loans", "Performance: FSF Loans @ SHG", "Performance: GENERAL Loans", "Performance: GENERAL Loans @ SHG" ,"Dormancy @ SHG")
z=c( "Extreme VO Monitoring", "Extreme VO Monitoring @ CLF", "Fund Available", "Funds: Not Delivered", "Funds: Not Delivered @ SHG", "Funds: Diverted", "Funds: Excess", "Performance: ICF Loans", "Performance: ICF Loans @ SHG", "Performance: HRF Loans", "Performance: HRF Loans @ SHG", "Performance: FSF Loans", "Performance: FSF Loans @ SHG", "Performance: GENERAL Loans", "Performance: GENERAL Loans @ SHG" ,"Dormancy @ SHG")


ui<-shinyUI(navbarPage(h4(strong("JEEViKA Dashboard: VO Funds")),
                       theme=shinytheme("cosmo"),
                       tabPanel(h6(strong("Home- Glance @ VO Fund Positions")),
                                sidebarLayout(
                                  sidebarPanel(
                                    width=3, 
                                    selectInput("glance", label="Select View", choices=c("By Project", "By District"), selected=NULL, multiple = FALSE),
                                    br(),                 
                                    selectInput("parametersglance", label="Select Topic", choices=y, selected=NULL, multiple = FALSE),
                                    br(),
                                    actionButton("triggerglance", "Glance It..", width="100%"),
                                    br(),
                                    br(),
                                    downloadButton("downloadglance", paste("Download @ A Glance: ",Sys.Date()))
                                  ),
                                  mainPanel(
                                    DT::dataTableOutput("glancetable", width = "100%", height = "auto")
                                  )
                                )
                       ),
                       tabPanel(h6(strong("VO Fund Positions - GIS")),
                                sidebarLayout(
                                  sidebarPanel(
                                    width=3, 
                                    selectInput("mapindicator", label="Select Indicator", choices=x, multiple = FALSE),
                                    actionButton("triggermap", "Map It..", width="100%")
                                  ),
                                  mainPanel(leafletOutput("Map", width = "100%", height = 500)
                                  )
                                )
                       ),
                       tabPanel(h6(strong("VO Fund Positions & Problems - Charts")),
                                sidebarLayout(
                                  sidebarPanel(width=3, 
                                               p("Select Geography"),
                                               br(),
                                               selectInput("districthc", label="Select District(s)", choices=District$`District`, selected=NULL, multiple = TRUE),
                                               br(),                 
                                               uiOutput("blockselect"),
                                               br(),
                                               selectInput("chartindicator", label="Select Topic", choices=y, selected=NULL, multiple = FALSE),
                                               actionButton("triggerchart", "Chart It..", width="100%")
                                  ),
                                  mainPanel(
                                    highchartOutput("hc",height = "500px")
                                  )
                                )
                       ),
                       tabPanel(h6(strong("VO Fund Problems - Escalations")),
                                sidebarLayout(
                                  sidebarPanel(
                                    width=3, 
                                    p("Select Geography"),
                                    br(),
                                    selectInput("districtDT", label="Select District(s)", choices=District$`District`, selected=NULL, multiple = TRUE),
                                    br(),                 
                                    uiOutput("blockselectDT"),
                                    br(),
                                    uiOutput("clfselectDT"),
                                    br(),
                                    selectInput("topicDT", label="Select Topic", choices=z, selected=NULL, multiple = FALSE),
                                    actionButton("triggerDT", "Detail It..", width="100%"),
                                    br(),
                                    br(),
                                    downloadButton("downloadDT", paste("Download Escalations:",Sys.Date()))
                                  ),
                                  mainPanel(
                                    DT::dataTableOutput("table", width = "100%", height = "auto")
                                  )
                                )
                       ),
                       tabPanel(h6(strong("VO Fund Problems - Hindi Escalations")),
                                sidebarLayout(
                                  sidebarPanel(
                                    width=3, 
                                    p("Select Geography"),
                                    br(),
                                    selectInput("districtDTH", label="Select District(s)", choices=District$`District`, selected=NULL, multiple = TRUE),
                                    br(),                 
                                    uiOutput("blockselectDTH"),
                                    br(),
                                    uiOutput("clfselectDTH"),
                                    br(),
                                    selectInput("topicDTH", label="Select Topic", choices=z, selected=NULL, multiple = FALSE),
                                    actionButton("triggerDTH", "Detail It..", width="100%"),
                                    br(),
                                    br(),
                                    downloadButton("downloadDTH", paste("Download Escalations:",Sys.Date()))
                                  ),
                                  mainPanel(
                                    DT::dataTableOutput("tableH", width = "100%", height = "auto")
                                  )
                                )
                       ),
                       header=h6(p("Brought to you by ", strong(a("JEEViKA", href="http://223.30.251.85:8090/")), " and the ", strong(a("Social Observatory", href="http://so-prototype.webflow.io/"))),align="center")
))


server<-function(input, output, session){
  
  #########################################################################   ######################################################################### 
  
  glancedatabase<-reactive({
    if(input$glance=="By Project"){
      switch(input$parametersglance,
             "Extreme VO Monitoring"=select(Scheme_Facesheet, `Scheme`,`VOs: Extreme`, `VOs Reporting`, `CLFs: Reporting`, `CLFs: Ready`),
             
             "Fund Available"=select(Scheme_Facesheet, `Scheme`, `VOs Reporting`, `Fund Available (Avg)`),
             "Income"=select(Scheme_Facesheet, `Scheme`, `VOs Reporting`, `Income (Avg)`),
             "Funds: Not Delivered"=select(Scheme_Facesheet, `Scheme`, `VOs Reporting`, `VOs: ICF Not Received`, `Ratio VOs: ICF Not Received`, `VOs: HRF Not Received`, `Ratio VOs: HRF Not Received`, `VOs: FSF Not Received`, `Ratio VOs: FSF Not Received`),
             "Funds: Not Delivered @ SHG"=select(Scheme_Facesheet, `Scheme`, `SHGs Reporting`, `SHGs: No ICF`, `Ratio SHGs: No ICF`, `SHGs: No FSF`, `Ratio SHGs: No FSF`, `SHGs: No RF`, `Ratio SHGs: No RF`, `SHGs: No GL`, `Ratio SHGs: No GL`),
             "Funds: Diverted"=select(Scheme_Facesheet, `Scheme`, `VOs Reporting`, `VOs: Diversion for ICF`, `Ratio VOs: Diversion for ICF`, `VOs: Diversion for HRF`, `Ratio VOs: Diversion for HRF`,  `VOs: Diversion for FSF`, `Ratio VOs: Diversion for FSF`),
             "Funds: Excess"=select(Scheme_Facesheet, `Scheme`, `VOs Reporting`, `VOs: Excess ICF`, `Ratio VOs: Excess ICF`, `VOs: Excess FSF`, `Ratio VOs: Excess FSF`, `VOs: Excess RF`, `Ratio VOs: Excess RF`),
             
             "Performance: ICF"=select(Scheme_Facesheet, `Scheme`, `VOs Reporting`, `ICF Rotation (Avg)`, `ICF Received (Avg)`, `ICF Utilized (Avg)`, `ICF Repaid to VO (Avg)`, `ICF Repaid by VO (Avg)`),
             "Performance: ICF Loans"=select(Scheme_Facesheet, `Scheme`, `VOs Reporting.ICF`, `VO Disbursed (Avg).ICF`, `VO Recovered (Avg).ICF`, `VO Recovery (Avg).ICF`, `VO Repayment (Avg).ICF`, `Non-Recovering VOs.ICF`, `Ratio Non-Recovering VOs.ICF`),
             "Performance: ICF Loans @ SHG"=select(Scheme_Facesheet, `Scheme`, `SHGs Reporting.ICF`, `SHG Borrowed (Avg).ICF`, `SHG Repaid (Avg).ICF`, `SHG Recovery (Avg).ICF`, `SHG Repayment (Avg).ICF`, `Non-Paying SHGs.ICF`, `Ratio Non-Paying SHGs.ICF`),
             
             "Performance: HRF"=select(Scheme_Facesheet, `Scheme`, `VOs Reporting`, `HRF Rotation (Avg)`, `HRF Received (Avg)`, `HRF Utilized (Avg)`, `HRF Repaid to VO (Avg)`),
             "Performance: HRF Loans"=select(Scheme_Facesheet, `Scheme`, `VOs Reporting.HRF`, `VO Disbursed (Avg).HRF`, `VO Recovered (Avg).HRF`, `VO Recovery (Avg).HRF`, `Non-Recovering VOs.HRF`, `Ratio Non-Recovering VOs.HRF`),
             "Performance: HRF Loans @ SHG"=select(Scheme_Facesheet, `Scheme`, `SHGs Reporting.HRF`, `SHG Borrowed (Avg).HRF`, `SHG Repaid (Avg).HRF`, `SHG Recovery (Avg).HRF`, `SHG Repayment (Avg).HRF`, `Non-Paying SHGs.HRF`, `Ratio Non-Paying SHGs.HRF`),
             
             "Performance: FSF"=select(Scheme_Facesheet, `Scheme`, `VOs Reporting`, `FSF Rotation (Avg)`, `FSF Received (Avg)`, `FSF Utilized (Avg)`, `FSF Repaid to VO (Avg)`),
             "Performance: FSF Loans"=select(Scheme_Facesheet, `Scheme`, `VOs Reporting.FSF`, `VO Disbursed (Avg).FSF`, `VO Recovered (Avg).FSF`, `VO Recovery (Avg).FSF`, `Non-Recovering VOs.FSF`, `Ratio Non-Recovering VOs.FSF`),
             "Performance: FSF Loans @ SHG"=select(Scheme_Facesheet, `Scheme`, `SHGs Reporting.FSF`, `SHG Borrowed (Avg).FSF`, `SHG Repaid (Avg).FSF`, `SHG Recovery (Avg).FSF`, `SHG Repayment (Avg).FSF`, `Non-Paying SHGs.FSF`, `Ratio Non-Paying SHGs.FSF`),
             
             "Performance: GENERAL Loans"=select(Scheme_Facesheet, `Scheme`, `VOs Reporting.GENERAL`, `VO Disbursed (Avg).GENERAL`, `VO Recovered (Avg).GENERAL`, `VO Recovery (Avg).GENERAL`, `Non-Recovering VOs.GENERAL`, `Ratio Non-Recovering VOs.GENERAL`),
             "Performance: GENERAL Loans @ SHG"=select(Scheme_Facesheet, `Scheme`, `SHGs Reporting.GENERAL`, `SHG Borrowed (Avg).GENERAL`, `SHG Repaid (Avg).GENERAL`, `SHG Recovery (Avg).GENERAL`, `SHG Repayment (Avg).GENERAL`, `Non-Paying SHGs.GENERAL`, `Ratio Non-Paying SHGs.GENERAL`),
             
             "Dormancy @ SHG"=select(Scheme_Facesheet, `Scheme`, `SHGs Reporting`, `SHGs: Dormant`, `Ratio SHGs: Dormant`, `SHGs: No Transactions`, `Ratio SHGs: No Transactions`)
      )
    } else {
      switch(input$parametersglance,
             "Extreme VO Monitoring"=select(District_Facesheet, `District`,`VOs: Extreme`, `VOs Reporting`, `CLFs: Reporting`, `CLFs: Ready`),
             
             "Fund Available"=select(District_Facesheet, `District`, `VOs Reporting`, `Fund Available (Avg)`, `Rank Funds Available`),
             "Income"=select(District_Facesheet, `District`, `VOs Reporting`, `Income (Avg)`, `Rank Income`),
             "Funds: Not Delivered"=select(District_Facesheet, `District`, `VOs Reporting`, `VOs: ICF Not Received`, `Ratio VOs: ICF Not Received`, `Rank_Ratio VOs: ICF Not Received`, `VOs: HRF Not Received`, `Ratio VOs: HRF Not Received`, `Rank_Ratio VOs: HRF Not Received`, `VOs: FSF Not Received`, `Ratio VOs: FSF Not Received`, `Rank_Ratio VOs: FSF Not Received`),
             "Funds: Not Delivered @ SHG"=select(District_Facesheet, `District`, `SHGs Reporting`, `SHGs: No ICF`, `Ratio SHGs: No ICF`, `Rank_Ratio SHGs: No ICF`, `SHGs: No FSF`, `Ratio SHGs: No FSF`, `Rank_Ratio SHGs: No FSF`, `SHGs: No RF`, `Ratio SHGs: No RF`, `Rank_Ratio SHGs: No RF`, `SHGs: No GL`, `Ratio SHGs: No GL`, `Rank_Ratio SHGs: No GL`),
             "Funds: Diverted"=select(District_Facesheet, `District`, `VOs Reporting`, `VOs: Diversion for ICF`, `Ratio VOs: Diversion for ICF`, `Rank_Ratio VOs: Diversion for ICF`, `VOs: Diversion for HRF`, `Ratio VOs: Diversion for HRF`, `Rank_Ratio VOs: Diversion for HRF`,  `VOs: Diversion for FSF`, `Ratio VOs: Diversion for FSF`, `Rank_Ratio VOs: Diversion for FSF`),
             "Funds: Excess"=select(District_Facesheet, `District`, `VOs Reporting`, `VOs: Excess ICF`, `Ratio VOs: Excess ICF`, `Rank_Ratio VOs: Excess ICF`, `VOs: Excess FSF`, `Ratio VOs: Excess FSF`, `Rank_Ratio VOs: Excess FSF`, `VOs: Excess RF`, `Ratio VOs: Excess RF`, `Rank_Ratio VOs: Excess RF`),
             
             "Performance: ICF"=select(District_Facesheet, `District`, `VOs Reporting`, `ICF Rotation (Avg)`, `ICF Received (Avg)`, `ICF Utilized (Avg)`, `ICF Repaid to VO (Avg)`, `ICF Repaid by VO (Avg)`),
             "Performance: ICF Loans"=select(District_Facesheet, `District`, `VOs Reporting.ICF`, `VO Disbursed (Avg).ICF`, `VO Recovered (Avg).ICF`, `VO Recovery (Avg).ICF`, `VO Repayment (Avg).ICF`, `Non-Recovering VOs.ICF`, `Ratio Non-Recovering VOs.ICF`, `Rank_Ratio Non-Recovering VOs.ICF`),
             "Performance: ICF Loans @ SHG"=select(District_Facesheet, `District`, `SHGs Reporting.ICF`, `SHG Borrowed (Avg).ICF`, `SHG Repaid (Avg).ICF`, `SHG Recovery (Avg).ICF`, `SHG Repayment (Avg).ICF`, `Non-Paying SHGs.ICF`, `Ratio Non-Paying SHGs.ICF`, `Rank_Ratio Non-Paying SHGs.ICF`),
             
             "Performance: HRF"=select(District_Facesheet, `District`, `VOs Reporting`, `HRF Rotation (Avg)`, `HRF Received (Avg)`, `HRF Utilized (Avg)`, `HRF Repaid to VO (Avg)`),
             "Performance: HRF Loans"=select(District_Facesheet, `District`, `VOs Reporting.HRF`, `VO Disbursed (Avg).HRF`, `VO Recovered (Avg).HRF`, `VO Recovery (Avg).HRF`, `Non-Recovering VOs.HRF`, `Ratio Non-Recovering VOs.HRF`, `Rank_Ratio Non-Recovering VOs.HRF`),
             "Performance: HRF Loans @ SHG"=select(District_Facesheet, `District`, `SHGs Reporting.HRF`, `SHG Borrowed (Avg).HRF`, `SHG Repaid (Avg).HRF`, `SHG Recovery (Avg).HRF`, `SHG Repayment (Avg).HRF`, `Non-Paying SHGs.HRF`, `Ratio Non-Paying SHGs.HRF`, `Rank_Ratio Non-Paying SHGs.HRF`),
             
             "Performance: FSF"=select(District_Facesheet, `District`, `VOs Reporting`, `FSF Rotation (Avg)`, `FSF Received (Avg)`, `FSF Utilized (Avg)`, `FSF Repaid to VO (Avg)`),
             "Performance: FSF Loans"=select(District_Facesheet, `District`, `VOs Reporting.FSF`, `VO Disbursed (Avg).FSF`, `VO Recovered (Avg).FSF`, `VO Recovery (Avg).FSF`, `Non-Recovering VOs.FSF`, `Ratio Non-Recovering VOs.FSF`),
             "Performance: FSF Loans @ SHG"=select(District_Facesheet, `District`, `SHGs Reporting.FSF`, `SHG Borrowed (Avg).FSF`, `SHG Repaid (Avg).FSF`, `SHG Recovery (Avg).FSF`, `SHG Repayment (Avg).FSF`, `Non-Paying SHGs.FSF`, `Ratio Non-Paying SHGs.FSF`, `Rank_Ratio Non-Paying SHGs.FSF`),
             
             "Performance: GENERAL Loans"=select(District_Facesheet, `District`, `VOs Reporting.GENERAL`, `VO Disbursed (Avg).GENERAL`, `VO Recovered (Avg).GENERAL`, `VO Recovery (Avg).GENERAL`, `Non-Recovering VOs.GENERAL`, `Ratio Non-Recovering VOs.GENERAL`),
             "Performance: GENERAL Loans @ SHG"=select(District_Facesheet, `District`, `SHGs Reporting.GENERAL`, `SHG Borrowed (Avg).GENERAL`, `SHG Repaid (Avg).GENERAL`, `SHG Recovery (Avg).GENERAL`, `SHG Repayment (Avg).GENERAL`, `Non-Paying SHGs.GENERAL`, `Ratio Non-Paying SHGs.GENERAL`, `Rank_Ratio Non-Paying SHGs.GENERAL`),
             
             "Dormancy @ SHG"=select(District_Facesheet, `District`, `SHGs Reporting`, `SHGs: Dormant`, `Ratio SHGs: Dormant`, `Rank_Ratio SHGs: Dormant`, `SHGs: No Transactions`, `Ratio SHGs: No Transactions`, `Rank_Ratio SHGs: No Transactions`)
      )
    }
  })
  
  
  observe({
    input$triggerglance
    isolate({
      dataset<-glancedatabase()
      output$glancetable<-DT::renderDataTable(dataset, escape=TRUE, style="bootstrap")
      
      output$downloadglance<-downloadHandler(filename=function(){
        paste(input$parametersglance, " @ ", Sys.Date(), ".csv", sep=" ")},
        content = function(file) {
          export(dataset, format="csv", file)
        }
      )
    })
  })
  
  #########################################################################   ######################################################################### 
  
  output$Map<-renderLeaflet({
    leaflet(DistrictGIS)%>%
      setView(86, 25.75, 7)%>%
      addProviderTiles("OpenStreetMap.HOT")
  })
  
  isolate({
    colorpal<-reactive({
      colorNumeric("YlOrRd", DistrictGIS@data[,input$mapindicator])
    })
    
    text2<-reactive({
      switch(input$mapindicator,
             "Fund Available (Avg)"=paste(DistrictGIS@data$DISTRICT, "<br>","<br>", "VOs Reporting: ", DistrictGIS@data$`VOs Reporting`, "<br>","<br>", "Fund: Rs ", DistrictGIS@data$`Fund Available (Avg)`,  "<br>","<br>", "Gross ICF in VO: Rs ", DistrictGIS@data$`(Gross) ICF in VO (Avg)`,  "<br>","<br>", "HRF in VO: Rs", DistrictGIS@data$`HRF in VO (Avg)`, "<br>","<br>", "FSF in VO: Rs", DistrictGIS@data$`FSF in VO (Avg)`,  "<br>","<br>", "RF in VO: Rs", DistrictGIS@data$`RF in VO (Avg)`),
             "Income (Avg)"=paste(DistrictGIS@data$DISTRICT, "<br>","<br>", "VOs Reporting: ", DistrictGIS@data$`VOs Reporting`, "<br>","<br>", "Income: Rs ", DistrictGIS@data$`Income (Avg)`),
             
             "ICF Rotation (Avg)"=paste(DistrictGIS@data$DISTRICT, "<br>","<br>", "VOs Reporting: ", DistrictGIS@data$`VOs Reporting`, "<br>","<br>", "ICF Received: Rs ", DistrictGIS@data$`ICF Received (Avg)`,  "<br>","<br>", "ICF Utilized: Rs ", DistrictGIS@data$`ICF Utilized (Avg)`,  "<br>","<br>", "GL Disbursed: Rs ", DistrictGIS@data$`GL Disbursed (Avg)`, "<br>","<br>", "ICF Rotation: ", DistrictGIS@data$`ICF Rotation (Avg)`,"%"),
             "HRF Rotation (Avg)"=paste(DistrictGIS@data$DISTRICT, "<br>","<br>", "VOs Reporting: ", DistrictGIS@data$`VOs Reporting`, "<br>","<br>", "HRF Received: Rs ", DistrictGIS@data$`HRF Received (Avg)`,  "<br>","<br>", "HRF Utilized: Rs ", DistrictGIS@data$`HRF Utilized (Avg)`, "<br>","<br>", "HRF Rotation: ", DistrictGIS@data$`HRF Rotation (Avg)`,"%"),
             "FSF Rotation (Avg)"=paste(DistrictGIS@data$DISTRICT, "<br>","<br>", "VOs Reporting: ", DistrictGIS@data$`VOs Reporting`, "<br>","<br>", "FSF Received: Rs ", DistrictGIS@data$`FSF Received (Avg)`,  "<br>","<br>", "FSF Utilized: Rs ", DistrictGIS@data$`FSF Utilized (Avg)`, "<br>","<br>", "FSF Rotation: ", DistrictGIS@data$`FSF Rotation (Avg)`,"%"),
             
             "VO Repayment (Avg).ICF"=paste(DistrictGIS@data$DISTRICT, "<br>","<br>", "VOs Reporting: ", DistrictGIS@data$`VOs Reporting.ICF`, "<br>","<br>", "VO Disbursed: Rs ", DistrictGIS@data$`VO Disbursed (Avg).ICF`,"<br>","<br>", "VO Recovered: Rs ", DistrictGIS@data$`VO Recovered (Avg).ICF`, "<br>","<br>", "VO Recovery Rate: ", DistrictGIS@data$`VO Recovery (Avg).ICF`, "%", "<br>","<br>", "VO Repayment Rate: ", DistrictGIS@data$`VO Repayment (Avg).ICF`, "%", "<br>","<br>", "Non-Recovering VOs: ", DistrictGIS@data$`Non-Recovering VOs.ICF`),
             "VO Recovery (Avg).HRF"=paste(DistrictGIS@data$DISTRICT, "<br>","<br>", "VOs Reporting: ", DistrictGIS@data$`VOs Reporting.HRF`, "<br>","<br>", "VO Disbursed: Rs ", DistrictGIS@data$`VO Disbursed (Avg).HRF`,"<br>","<br>", "VO Recovered: Rs ", DistrictGIS@data$`VO Recovered (Avg).HRF`, "<br>","<br>", "VO Recovery Rate: ", DistrictGIS@data$`VO Recovery (Avg).HRF`, "%", "<br>","<br>", "Non-Recovering VOs: ", DistrictGIS@data$`Non-Recovering VOs.HRF`),
             "VO Recovery (Avg).FSF"=paste(DistrictGIS@data$DISTRICT, "<br>","<br>", "VOs Reporting: ", DistrictGIS@data$`VOs Reporting.FSF`, "<br>","<br>", "VO Disbursed: Rs ", DistrictGIS@data$`VO Disbursed (Avg).FSF`,"<br>","<br>", "VO Recovered: Rs ", DistrictGIS@data$`VO Recovered (Avg).FSF`, "<br>","<br>", "VO Recovery Rate: ", DistrictGIS@data$`VO Recovery (Avg).FSF`, "%","<br>","<br>", "Non-Recovering VOs: ", DistrictGIS@data$`Non-Recovering VOs.FSF`),
             "VO Recovery (Avg).GENERAL"=paste(DistrictGIS@data$DISTRICT, "<br>","<br>", "VOs Reporting: ", DistrictGIS@data$`VOs Reporting.GENERAL`, "<br>","<br>", "VO Disbursed: Rs ", DistrictGIS@data$`VO Disbursed (Avg).GENERAL`,"<br>","<br>", "VO Recovered: Rs ", DistrictGIS@data$`VO Recovered (Avg).GENERAL`, "<br>","<br>", "VO Recovery Rate: ", DistrictGIS@data$`VO Recovery (Avg).GENERAL`, "%","<br>","<br>", "Non-Recovering VOs: ", DistrictGIS@data$`Non-Recovering VOs.GENERAL`),
             
             "SHG Repayment (Avg).ICF"=paste(DistrictGIS@data$DISTRICT, "<br>","<br>", "SHGs Reporting: ", DistrictGIS@data$`SHGs Reporting.ICF`, "<br>","<br>", "SHG Borrowed: Rs ", DistrictGIS@data$`SHG Borrowed (Avg).ICF`,"<br>","<br>", "SHG Repaid: Rs ", DistrictGIS@data$`SHG Repaid (Avg).ICF`, "<br>","<br>", "SHG Recovery Rate: ", DistrictGIS@data$`SHG Recovery (Avg).ICF`, "%", "<br>","<br>", "SHG Repayment Rate: ", DistrictGIS@data$`SHG Repayment (Avg).ICF`, "%", "<br>","<br>", "Non-Paying SHGs: ", DistrictGIS@data$`Non-Paying SHGs.ICF`),
             "SHG Recovery (Avg).HRF"=paste(DistrictGIS@data$DISTRICT, "<br>","<br>", "SHGs Reporting: ", DistrictGIS@data$`SHGs Reporting.HRF`, "<br>","<br>", "SHG Borrowed: Rs ", DistrictGIS@data$`SHG Borrowed (Avg).HRF`,"<br>","<br>", "SHG Repaid: Rs ", DistrictGIS@data$`SHG Repaid (Avg).HRF`, "<br>","<br>", "SHG Recovery Rate: ", DistrictGIS@data$`SHG Recovery (Avg).HRF`, "%", "<br>","<br>", "Non-Paying SHGs: ", DistrictGIS@data$`Non-Paying SHGs.HRF`),
             "SHG Recovery (Avg).FSF"=paste(DistrictGIS@data$DISTRICT, "<br>","<br>", "SHGs Reporting: ", DistrictGIS@data$`SHGs Reporting.FSF`, "<br>","<br>", "SHG Borrowed: Rs ", DistrictGIS@data$`SHG Borrowed (Avg).FSF`,"<br>","<br>", "SHG Repaid: Rs ", DistrictGIS@data$`SHG Repaid (Avg).FSF`, "<br>","<br>", "SHG Recovery Rate: ", DistrictGIS@data$`SHG Recovery (Avg).FSF`, "%", "<br>","<br>", "Non-Paying SHGs: ", DistrictGIS@data$`Non-Paying SHGs.FSF`),
             "SHG Recovery (Avg).GENERAL"=paste(DistrictGIS@data$DISTRICT, "<br>","<br>", "SHGs Reporting: ", DistrictGIS@data$`SHGs Reporting.GENERAL`, "<br>","<br>", "SHG Borrowed: Rs ", DistrictGIS@data$`SHG Borrowed (Avg).GENERAL`,"<br>","<br>", "SHG Repaid: Rs ", DistrictGIS@data$`SHG Repaid (Avg).GENERAL`, "<br>","<br>", "SHG Recovery Rate: ", DistrictGIS@data$`SHG Recovery (Avg).GENERAL`, "%", "<br>","<br>", "Non-Paying SHGs: ", DistrictGIS@data$`Non-Paying SHGs.GENERAL`)
      )
    })
  })
  
  observe({
    input$triggermap
    isolate({
      pal<-colorpal()
      popuptext<-text2()
      
      leafletProxy("Map", data=DistrictGIS)%>%
        clearShapes%>%
        clearControls%>%
        clearMarkers%>%
        addPolygons(opacity=.5, weight=1, fillColor=pal(DistrictGIS@data[,input$mapindicator]), fillOpacity = .6)%>%
        addLegend(pal=pal, values=(DistrictGIS@data[,input$mapindicator]), "bottomright", bins=5)%>%
        addMarkers(lng=~V1, lat=~V2, popup=paste(popuptext))
    })
  })
  
  #########################################################################   ######################################################################### 
  
  output$blockselect<-renderUI({
    if(is.null(input$districthc))
      return("Select District Above")
    if(length(input$districthc)>1)
      return("To Activate Block View, select only One District above")
    block_name<-filter(Block, `District`==input$districthc)%>%select(`Block`)
    selectInput("blockhc", "Select Block(s)", choices = block_name, multiple=TRUE, selected=NULL)
  })
  
  slicer<-isolate({
    reactive({
      if(!is.null(input$blockhc)){
        subset(Block_Facesheet, `District` %in% input$districthc & `Block` %in% input$blockhc)
      } else{
        subset(District_Facesheet, `District` %in% input$districthc)
      }
    })
  })
  
  
  output$hc<-renderHighchart({
    input$triggerchart
    isolate({
      content<-slicer()
      if(input$chartindicator=="Extreme VO Monitoring"){
          hc<-highchart()%>%
            hc_chart(type="column")%>%
            hc_add_theme(hc_theme_chalk())%>%
            hc_add_serie(name="Extreme VOs", data=content$`VOs: Extreme`, color="red")%>%
            hc_add_serie(name="VOs Reporting", data=content$`VOs Reporting`, color="plum")%>%
            hc_add_serie(name="CLFs Reporting", data=content$`CLFs: Reporting`, color="plum")%>%
            hc_add_serie(name="CLFs Ready", data=content$`CLFs: Ready`, color="chartreuse")%>%
            hc_xAxis(categories=content$`Geography`)
        } else {
          if(input$chartindicator=="Fund Available"){
          hc<-highchart()%>%
          hc_chart(type="column")%>%
          hc_add_theme(hc_theme_chalk())%>%
          hc_add_serie(name="VOs Reporting", data=content$`VOs Reporting`, color="plum")%>%
          hc_add_serie(name="Fund Available", data=content$`Fund Available (Avg)`, color="red")%>%
          hc_xAxis(categories=content$`Geography`)
      } else {
        if(input$chartindicator=="Income"){
          hc<-highchart()%>%
            hc_chart(type="column")%>%
            hc_add_theme(hc_theme_chalk())%>%
            hc_add_serie(name="VOs Reporting", data=content$`VOs Reporting`, color="plum")%>%
            hc_add_serie(name="Income", data=content$`Income (Avg)`, color="chartreuse")%>%
            hc_xAxis(categories=content$`Geography`) 
        } else {
          if(input$chartindicator=="Funds: Not Delivered"){
            hc<-highchart()%>%
              hc_chart(type="column")%>%
              hc_add_theme(hc_theme_chalk())%>%
              hc_add_serie(name="VOs Reporting", data=content$`VOs Reporting`, color="plum")%>%
              hc_add_serie(name="ICF", data=content$`VOs: ICF Not Received`, color="red")%>%
              hc_add_serie(name="HRF", data=content$`VOs: HRF Not Received`, color="red")%>%
              hc_add_serie(name="FSF", data=content$`VOs: FSF Not Received`, color="red")%>%
              hc_xAxis(categories=content$`Geography`) 
          } else {
            if(input$chartindicator=="Funds: Not Delivered @ SHG"){
              hc<-highchart()%>%
                hc_chart(type="column")%>%
                hc_add_theme(hc_theme_chalk())%>%
                hc_add_serie(name="SHGs Reporting", data=content$`SHGs Reporting`, color="plum")%>%
                hc_add_serie(name="ICF", data=content$`SHGs: No ICF`, color="red")%>%
                hc_add_serie(name="FSF", data=content$`SHGs: No FSF`, color="red")%>%
                hc_add_serie(name="RF", data=content$`SHGs: No RF`, color="red")%>%
                hc_add_serie(name="GL", data=content$`SHGs: No GL`, color="red")%>%
                hc_xAxis(categories=content$`Geography`) 
            } else {
              if(input$chartindicator=="Funds: Diverted"){
                hc<-highchart()%>%
                  hc_chart(type="column")%>%
                  hc_add_theme(hc_theme_chalk())%>%
                  hc_add_serie(name="VOs Reporting", data=content$`VOs Reporting`, color="plum")%>%
                  hc_add_serie(name="ICF", data=content$`VOs: Diversion for ICF`, color="red")%>%
                  hc_add_serie(name="HRF", data=content$`VOs: Diversion for HRF`, color="red")%>%
                  hc_add_serie(name="FSF", data=content$`VOs: Diversion for FSF`, color="red")%>%
                  hc_xAxis(categories=content$`Geography`) 
              } else {
                if(input$chartindicator=="Funds: Excess"){
                  hc<-highchart()%>%
                    hc_chart(type="column")%>%
                    hc_add_theme(hc_theme_chalk())%>%
                    hc_add_serie(name="VOs Reporting", data=content$`VOs Reporting`, color="plum")%>%
                    hc_add_serie(name="ICF", data=content$`VOs: Excess ICF`, color="red")%>%
                    hc_add_serie(name="FSF", data=content$`VOs: Excess FSF`, color="red")%>%
                    hc_add_serie(name="RF", data=content$`VOs: Excess RF`, color="red")%>%
                    hc_xAxis(categories=content$`Geography`) 
                } else {
                  if(input$chartindicator=="Performance: ICF"){
                    hc<-highchart()%>%
                      hc_chart(type="column")%>%
                      hc_add_theme(hc_theme_chalk())%>%
                      hc_add_serie(name="VOs Reporting", data=content$`VOs Reporting`, color="plum")%>%
                      hc_add_serie(name="Rotation Ratio", data=content$`ICF Rotation (Avg)`, color="chartreuse")%>%
                      hc_add_serie(name="Received", data=content$`ICF Received (Avg)`, color="white")%>%
                      hc_add_serie(name="Utilized", data=content$`ICF Utilized (Avg)`, color="cornflowerblue")%>%
                      hc_add_serie(name="Repaid TO VO", data=content$`ICF Repaid to VO (Avg)`, color="white")%>%
                      hc_add_serie(name="Repaid BY VO", data=content$`ICF Repaid by VO (Avg)`, color="cornflowerblue")%>%
                      hc_xAxis(categories=content$`Geography`)  
                  } else {
                    if(input$chartindicator=="Performance: ICF Loans"){
                      hc<-highchart()%>%
                        hc_chart(type="column")%>%
                        hc_add_theme(hc_theme_chalk())%>%
                        hc_add_serie(name="VOs Reporting", data=content$`VOs Reporting.ICF`, color="plum")%>%
                        hc_add_serie(name="Repayment %", data=content$`VO Repayment (Avg).ICF`, color="chartreuse")%>%
                        hc_add_serie(name="Recovery %", data=content$`VO Recovery (Avg).ICF`, color="chartreuse")%>%
                        hc_add_serie(name="Disbursed", data=content$`VO Disbursed (Avg).ICF`, color="cornflowerblue")%>%
                        hc_add_serie(name="Recovered", data=content$`VO Recovered (Avg).ICF`, color="white")%>%
                        hc_add_serie(name="Non-Recovering VOs", data=content$`Non-Recovering VOs.ICF`, color="red")%>%
                        hc_xAxis(categories=content$`Geography`)  
                    } else {
                      if(input$chartindicator=="Performance: ICF Loans @ SHG"){
                        hc<-highchart()%>%
                          hc_chart(type="column")%>%
                          hc_add_theme(hc_theme_chalk())%>%
                          hc_add_serie(name="SHGs Reporting", data=content$`SHGs Reporting.ICF`, color="plum")%>%
                          hc_add_serie(name="Repayment %", data=content$`SHG Repayment (Avg).ICF`, color="chartreuse")%>%
                          hc_add_serie(name="Recovery %", data=content$`SHG Recovery (Avg).ICF`, color="chartreuse")%>%
                          hc_add_serie(name="Disbursed", data=content$`SHG Borrowed (Avg).ICF`, color="cornflowerblue")%>%
                          hc_add_serie(name="Recovered", data=content$`SHG Repaid (Avg).ICF`, color="white")%>%
                          hc_add_serie(name="Non-Paying SHGs", data=content$`Non-Paying SHGs.ICF`, color="red")%>%
                          hc_xAxis(categories=content$`Geography`)  
                      } else {
                        if(input$chartindicator=="Performance: HRF Loans"){
                          hc<-highchart()%>%
                            hc_chart(type="column")%>%
                            hc_add_theme(hc_theme_chalk())%>%
                            hc_add_serie(name="VOs Reporting", data=content$`VOs Reporting.HRF`, color="plum")%>%
                            hc_add_serie(name="Recovery %", data=content$`VO Recovery (Avg).HRF`, color="chartreuse")%>%
                            hc_add_serie(name="Disbursed", data=content$`VO Disbursed (Avg).HRF`, color="cornflowerblue")%>%
                            hc_add_serie(name="Recovered", data=content$`VO Recovered (Avg).HRF`, color="white")%>%
                            hc_add_serie(name="Non-Recovering VOs", data=content$`Non-Recovering VOs.HRF`, color="red")%>%
                            hc_xAxis(categories=content$`Geography`) 
                        } else {
                          if(input$chartindicator=="Performance: HRF Loans @ SHG"){
                            hc<-highchart()%>%
                              hc_chart(type="column")%>%
                              hc_add_theme(hc_theme_chalk())%>%
                              hc_add_serie(name="SHGs Reporting", data=content$`SHGs Reporting.HRF`, color="plum")%>%
                              hc_add_serie(name="Recovery %", data=content$`SHG Recovery (Avg).HRF`, color="chartreuse")%>%
                              hc_add_serie(name="Disbursed", data=content$`SHG Borrowed (Avg).HRF`, color="cornflowerblue")%>%
                              hc_add_serie(name="Recovered", data=content$`SHG Repaid (Avg).HRF`, color="white")%>%
                              hc_add_serie(name="Non-Paying SHGs", data=content$`Non-Paying SHGs.HRF`, color="red")%>%
                              hc_xAxis(categories=content$`Geography`)  
                          } else {
                            if(input$chartindicator=="Performance: FSF Loans"){
                              hc<-highchart()%>%
                                hc_chart(type="column")%>%
                                hc_add_theme(hc_theme_chalk())%>%
                                hc_add_serie(name="VOs Reporting", data=content$`VOs Reporting.FSF`, color="plum")%>%
                                hc_add_serie(name="Recovery %", data=content$`VO Recovery (Avg).FSF`, color="chartreuse")%>%
                                hc_add_serie(name="Disbursed", data=content$`VO Disbursed (Avg).FSF`, color="cornflowerblue")%>%
                                hc_add_serie(name="Recovered", data=content$`VO Recovered (Avg).FSF`, color="white")%>%
                                hc_add_serie(name="Non-Recovering VOs", data=content$`Non-Recovering VOs.FSF`, color="red")%>%
                                hc_xAxis(categories=content$`Geography`) 
                            } else {
                              if(input$chartindicator=="Performance: FSF Loans @ SHG"){
                                hc<-highchart()%>%
                                  hc_chart(type="column")%>%
                                  hc_add_theme(hc_theme_chalk())%>%
                                  hc_add_serie(name="SHGs Reporting", data=content$`SHGs Reporting.FSF`, color="plum")%>%
                                  hc_add_serie(name="Recovery %", data=content$`SHG Recovery (Avg).FSF`, color="chartreuse")%>%
                                  hc_add_serie(name="Disbursed", data=content$`SHG Borrowed (Avg).FSF`, color="cornflowerblue")%>%
                                  hc_add_serie(name="Recovered", data=content$`SHG Repaid (Avg).FSF`, color="white")%>%
                                  hc_add_serie(name="Non-Paying SHGs", data=content$`Non-Paying SHGs.FSF`, color="red")%>%
                                  hc_xAxis(categories=content$`Geography`) 
                              } else {
                                if(input$chartindicator=="Performance: GENERAL Loans"){
                                  hc<-highchart()%>%
                                    hc_chart(type="column")%>%
                                    hc_add_theme(hc_theme_chalk())%>%
                                    hc_add_serie(name="VOs Reporting", data=content$`VOs Reporting.GENERAL`, color="plum")%>%
                                    hc_add_serie(name="Recovery %", data=content$`VO Recovery (Avg).GENERAL`, color="chartreuse")%>%
                                    hc_add_serie(name="Disbursed", data=content$`VO Disbursed (Avg).GENERAL`, color="cornflowerblue")%>%
                                    hc_add_serie(name="Recovered", data=content$`VO Recovered (Avg).GENERAL`, color="white")%>%
                                    hc_add_serie(name="Non-Recovering VOs", data=content$`Non-Recovering VOs.GENERAL`, color="red")%>%
                                    hc_xAxis(categories=content$`Geography`) 
                                } else {
                                  if(input$chartindicator=="Performance: GENERAL Loans @ SHG"){
                                    hc<-highchart()%>%
                                      hc_chart(type="column")%>%
                                      hc_add_theme(hc_theme_chalk())%>%
                                      hc_add_serie(name="SHGs Reporting", data=content$`SHGs Reporting.GENERAL`, color="plum")%>%
                                      hc_add_serie(name="Recovery %", data=content$`SHG Recovery (Avg).GENERAL`, color="chartreuse")%>%
                                      hc_add_serie(name="Disbursed", data=content$`SHG Borrowed (Avg).GENERAL`, color="cornflowerblue")%>%
                                      hc_add_serie(name="Recovered", data=content$`SHG Repaid (Avg).GENERAL`, color="white")%>%
                                      hc_add_serie(name="Non-Paying SHGs", data=content$`Non-Paying SHGs.GENERAL`, color="red")%>%
                                      hc_xAxis(categories=content$`Geography`)
                                  } else {
                                    if(input$chartindicator=="Dormancy @ SHG"){
                                      hc<-highchart()%>%
                                        hc_chart(type="column")%>%
                                        hc_add_theme(hc_theme_chalk())%>%
                                        hc_add_serie(name="SHGs Reporting", data=content$`SHGs Reporting`, color="plum")%>%
                                        hc_add_serie(name="Transacting", data=content$`SHGs: Transacting`, color="white")%>%
                                        hc_add_serie(name="Dormant", data=content$`SHGs: Dormant`, color="red")%>%
                                        hc_add_serie(name="No Transactions", data=content$`SHGs: No Transactions`, color="red")%>%
                                        hc_xAxis(categories=content$`Geography`)
                                    }
                                    } 
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    })
  })
  
  #########################################################################   ######################################################################### 
  
  output$blockselectDT<-renderUI({
    if(is.null(input$districtDT))
      return("Select District, for Blocks")
    if(length(input$districtDT)>1)
      return("To Activate Block View, select only One District above")
    block_name<-filter(Block, `District`==input$districtDT)%>%select(`Block`)
    selectInput("blockDT", "Select Block(s)", choices = block_name, multiple=TRUE, selected=NULL)
  })
  
  output$clfselectDT<-renderUI({
    if(is.null(input$districtDT))
      return("Select District, for CLFs")
    if(is.null(input$blockDT))
      return("Select Block, for CLFs")
    if(length(input$districtDT)>1 | length(input$blockDT)>1 )
      return("To Activate CLF View, select only One District & Block")
    clf_name<-filter(CLF, `District`==input$districtDT & `Block`==input$blockDT)%>%select(`CLF`)
    selectInput("clfDT", "Select CLF(s)", choices = clf_name, multiple=TRUE, selected=NULL)
  })
  
  database<-reactive({
    if(is.null(input$districtDT)){
      switch(input$topicDT,
             "Extreme VO Monitoring"=filter(VO_Funds, `District` %in% input$districtDT &  (Alert1!="" | Alert2!="" | Alert3!="" | Alert4!="" | Alert5!="" | Alert6!=""| Alert7!=""))%>%select(`District`, `Block`, `CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `Alert1`, `Alert2`, `Alert3`, `Alert4`, `Alert5`, `Alert6`, `Alert7`),
             "Extreme VO Monitoring @ CLF"=filter(CLF_Level, `District` %in% input$districtDT)%>%select(`District`, `Block`, `CLF`, `CLF: Ready`, `VOs Federated`, `VOs: Allowed`, `VOs: Extreme`),
             
             "Fund Available"=filter(VO_Funds,`District` %in% input$districtDT & `Funds in VO`>=200000 & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select(`District`, `Block`, `CLF`, `VO`, `Funds in VO`),
             
             "Funds: Not Delivered"=filter(VO_Funds,`District` %in% input$districtDT &  (`ICF Received?`=="No" | `FSF Received?`=="No" | `HRF Received?`=="No") & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select(`District`, `Block`, `CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `ICF Received?`, `HRF Received?`, `FSF Received?`),
             "Funds: Not Delivered @ SHG"=filter(SHG_Vouchers,`District` %in% input$districtDT &  (`ICF Received?`=="No" | `FSF Received?`=="No" | `RF Received?`=="No" | `GL Received?`=="No"))%>%select(`District`, `Block`, `CLF`, `Panchayat`, `Village`, `VO`, `Last Voucher VO`, `SHG`, `SHG DoF`, `Last Voucher SHG`, `ICF Received?`, `FSF Received?`, `RF Received?`, `GL Received?`),
             
             "Funds: Diverted"=filter(VO_Funds,`District` %in% input$districtDT &  (`Fund Diversion: ICF?`=="Yes" | `Fund Diversion: HRF?`=="Yes" | `Fund Diversion: FSF?`=="Yes") & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select(`District`, `Block`, `CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `Fund Diversion: ICF?`, `Fund Diversion: HRF?`, `Fund Diversion: FSF?`),
             "Funds: Excess"=filter(VO_Funds,`District` %in% input$districtDT &  (`Excess ICF?`=="Yes" | `Excess RF?`=="Yes" | `Excess FSF?`=="Yes") & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select(`District`, `Block`, `CLF`, `VO`, `VO DoF`, `Last Voucher VO`,  `Excess ICF?`,`Excess RF?`,`Excess FSF?`),
             
             "Performance: ICF Loans"=filter(VO_Recoveries,`District` %in% input$districtDT &  Loan=="ICF" & (`VO Disbursed`-`VO Recovered`>0) & (period>3| `VO Repayment`<90))%>%select(`District`, `Block`, `CLF`,`VO`, `VO DoF`, `Last Voucher VO`, `VO Last Recovered`, `VO Disbursed`, `VO Recovered`, `VO Recovery`, `VO Repayment`),
             "Performance: ICF Loans @ SHG"=filter(SHG_Recoveries,`District` %in% input$districtDT &  Loan=="ICF" & (`SHG Borrowed`-`SHG Repaid`>0) & (period>3| `SHG Repayment`<90))%>%select(`District`, `Block`, `CLF`, `Panchayat`, `Village`, `VO`, `VO DoF`, `Last Voucher VO`, `SHG`, `SHG Last Repay`, `SHG Borrowed`, `SHG Repaid`, `SHG Recovery`, `SHG Repayment`),
             
             "Performance: HRF Loans"=filter(VO_Recoveries,`District` %in% input$districtDT &  Loan=="HRF" & (`VO Disbursed`-`VO Recovered`>0) & period>3)%>%select(`District`, `Block`, `CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `VO Last Recovered`, `VO Disbursed`, `VO Recovered`, `VO Recovery`),
             "Performance: HRF Loans @ SHG"=filter(SHG_Recoveries,`District` %in% input$districtDT &  Loan=="HRF" & (`SHG Borrowed`-`SHG Repaid`>0) & period>3)%>%select(`District`, `Block`, `CLF`, `Panchayat`, `Village`, `VO`, `VO DoF`, `Last Voucher VO`, `SHG`, `SHG Last Repay`, `SHG Borrowed`, `SHG Repaid`, `SHG Recovery`),
             
             "Performance: FSF Loans"=filter(VO_Recoveries,`District` %in% input$districtDT &  Loan=="FSF" & (`VO Disbursed`-`VO Recovered`>0) & period>3)%>%select(`District`, `Block`, `CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `VO Last Recovered`, `VO Disbursed`, `VO Recovered`, `VO Recovery`),
             "Performance: FSF Loans @ SHG"=filter(SHG_Recoveries,`District` %in% input$districtDT &  Loan=="FSF" & (`SHG Borrowed`-`SHG Repaid`>0) & period>3)%>%select(`District`, `Block`, `CLF`, `Panchayat`, `Village`, `VO`, `VO DoF`, `Last Voucher VO`, `SHG`, `SHG Last Repay`, `SHG Borrowed`, `SHG Repaid`, `SHG Recovery`),
             
             "Performance: GENERAL Loans"=filter(VO_Recoveries,`District` %in% input$districtDT &  Loan=="GENERAL" & (`VO Disbursed`-`VO Recovered`>0) & period>3)%>%select(`District`, `Block`, `CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `VO Last Recovered`, `VO Disbursed`, `VO Recovered`, `VO Recovery`),
             "Performance: GENERAL Loans @ SHG"=filter(SHG_Recoveries,`District` %in% input$districtDT &  Loan=="GENERAL" & (`SHG Borrowed`-`SHG Repaid`>0) & period>3)%>%select(`District`, `Block`, `CLF`, `Panchayat`, `Village`, `VO`, `VO DoF`, `Last Voucher VO`, `SHG`, `SHG Last Repay`, `SHG Borrowed`, `SHG Repaid`, `SHG Recovery`),
             
             "Dormancy @ SHG"=filter(SHG_Vouchers,`District` %in% input$districtDT &  Alert %in% c("Dormant 3 months", "No Transactions"))%>%select(`District`, `Block`, `CLF`, `Panchayat`, `Village`, `VO`, `Last Voucher VO`, `Alert`, `SHG`, `Last Voucher SHG`)
             )
    } else {
      if(is.null(input$blockDT) & !is.null(input$districtDT)){
        switch(input$topicDT,
               "Extreme VO Monitoring"=filter(VO_Funds, `District` %in% input$districtDT &  (Alert1!="" | Alert2!="" | Alert3!="" | Alert4!="" | Alert5!="" | Alert6!=""| Alert7!=""))%>%select(`District`, `Block`, `CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `Alert1`, `Alert2`, `Alert3`, `Alert4`, `Alert5`, `Alert6`, `Alert7`),
               "Extreme VO Monitoring @ CLF"=filter(CLF_Level, `District` %in% input$districtDT)%>%select(`District`, `Block`, `CLF`, `CLF: Ready`, `VOs Federated`, `VOs: Allowed`, `VOs: Extreme`),
               
               "Fund Available"=filter(VO_Funds,`District` %in% input$districtDT & `Funds in VO`>=200000 & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select(`District`, `Block`, `CLF`, `VO`, `Funds in VO`),
               
               "Funds: Not Delivered"=filter(VO_Funds,`District` %in% input$districtDT &  (`ICF Received?`=="No" | `FSF Received?`=="No" | `HRF Received?`=="No") & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select(`District`, `Block`, `CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `ICF Received?`, `HRF Received?`, `FSF Received?`),
               "Funds: Not Delivered @ SHG"=filter(SHG_Vouchers,`District` %in% input$districtDT &  (`ICF Received?`=="No" | `FSF Received?`=="No" | `RF Received?`=="No" | `GL Received?`=="No"))%>%select(`District`, `Block`, `CLF`, `Panchayat`, `Village`, `VO`, `Last Voucher VO`, `SHG`, `SHG DoF`, `Last Voucher SHG`, `ICF Received?`, `FSF Received?`, `RF Received?`, `GL Received?`),
               
               "Funds: Diverted"=filter(VO_Funds,`District` %in% input$districtDT &  (`Fund Diversion: ICF?`=="Yes" | `Fund Diversion: HRF?`=="Yes" | `Fund Diversion: FSF?`=="Yes") & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select(`District`, `Block`, `CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `Fund Diversion: ICF?`, `Fund Diversion: HRF?`, `Fund Diversion: FSF?`),
               "Funds: Excess"=filter(VO_Funds,`District` %in% input$districtDT &  (`Excess ICF?`=="Yes" | `Excess RF?`=="Yes" | `Excess FSF?`=="Yes") & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select(`District`, `Block`, `CLF`, `VO`, `VO DoF`, `Last Voucher VO`,  `Excess ICF?`,`Excess RF?`,`Excess FSF?`),
               
               "Performance: ICF Loans"=filter(VO_Recoveries,`District` %in% input$districtDT &  Loan=="ICF" & (`VO Disbursed`-`VO Recovered`>0) & (period>3| `VO Repayment`<90))%>%select(`District`, `Block`, `CLF`,`VO`, `VO DoF`, `Last Voucher VO`, `VO Last Recovered`, `VO Disbursed`, `VO Recovered`, `VO Recovery`, `VO Repayment`),
               "Performance: ICF Loans @ SHG"=filter(SHG_Recoveries,`District` %in% input$districtDT &  Loan=="ICF" & (`SHG Borrowed`-`SHG Repaid`>0) & (period>3| `SHG Repayment`<90))%>%select(`District`, `Block`, `CLF`, `Panchayat`, `Village`, `VO`, `VO DoF`, `Last Voucher VO`, `SHG`, `SHG Last Repay`, `SHG Borrowed`, `SHG Repaid`, `SHG Recovery`, `SHG Repayment`),
               
               "Performance: HRF Loans"=filter(VO_Recoveries,`District` %in% input$districtDT &  Loan=="HRF" & (`VO Disbursed`-`VO Recovered`>0) & period>3)%>%select(`District`, `Block`, `CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `VO Last Recovered`, `VO Disbursed`, `VO Recovered`, `VO Recovery`),
               "Performance: HRF Loans @ SHG"=filter(SHG_Recoveries,`District` %in% input$districtDT &  Loan=="HRF" & (`SHG Borrowed`-`SHG Repaid`>0) & period>3)%>%select(`District`, `Block`, `CLF`, `Panchayat`, `Village`, `VO`, `VO DoF`, `Last Voucher VO`, `SHG`, `SHG Last Repay`, `SHG Borrowed`, `SHG Repaid`, `SHG Recovery`),
               
               "Performance: FSF Loans"=filter(VO_Recoveries,`District` %in% input$districtDT &  Loan=="FSF" & (`VO Disbursed`-`VO Recovered`>0) & period>3)%>%select(`District`, `Block`, `CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `VO Last Recovered`, `VO Disbursed`, `VO Recovered`, `VO Recovery`),
               "Performance: FSF Loans @ SHG"=filter(SHG_Recoveries,`District` %in% input$districtDT &  Loan=="FSF" & (`SHG Borrowed`-`SHG Repaid`>0) & period>3)%>%select(`District`, `Block`, `CLF`, `Panchayat`, `Village`, `VO`, `VO DoF`, `Last Voucher VO`, `SHG`, `SHG Last Repay`, `SHG Borrowed`, `SHG Repaid`, `SHG Recovery`),
               
               "Performance: GENERAL Loans"=filter(VO_Recoveries,`District` %in% input$districtDT &  Loan=="GENERAL" & (`VO Disbursed`-`VO Recovered`>0) & period>3)%>%select(`District`, `Block`, `CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `VO Last Recovered`, `VO Disbursed`, `VO Recovered`, `VO Recovery`),
               "Performance: GENERAL Loans @ SHG"=filter(SHG_Recoveries,`District` %in% input$districtDT &  Loan=="GENERAL" & (`SHG Borrowed`-`SHG Repaid`>0) & period>3)%>%select(`District`, `Block`, `CLF`, `Panchayat`, `Village`, `VO`, `VO DoF`, `Last Voucher VO`, `SHG`, `SHG Last Repay`, `SHG Borrowed`, `SHG Repaid`, `SHG Recovery`),
               
               "Dormancy @ SHG"=filter(SHG_Vouchers,`District` %in% input$districtDT &  Alert %in% c("Dormant 3 months", "No Transactions"))%>%select(`District`, `Block`, `CLF`, `Panchayat`, `Village`, `VO`, `Last Voucher VO`, `Alert`, `SHG`, `Last Voucher SHG`)
        )
      } else{
        if(!is.null(input$blockDT) & !is.null(input$districtDT) & is.null(input$clfDT))
          switch(input$topicDT,
                 "Extreme VO Monitoring"=filter(VO_Funds, `District` %in% input$districtDT & `Block` %in% input$blockDT &  (Alert1!="" | Alert2!="" | Alert3!="" | Alert4!="" | Alert5!="" | Alert6!=""| Alert7!=""))%>%select( `Block`, `CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `Alert1`, `Alert2`, `Alert3`, `Alert4`, `Alert5`, `Alert6`, `Alert7`),
                 "Extreme VO Monitoring @ CLF"=filter(CLF_Level, `District` %in% input$districtDT & `Block` %in% input$blockDT)%>%select( `Block`, `CLF`, `CLF: Ready`, `VOs Federated`, `VOs: Allowed`, `VOs: Extreme`),
                 
                 "Fund Available"=filter(VO_Funds,`District` %in% input$districtDT & `Block` %in% input$blockDT & `Funds in VO`>=200000 & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select( `Block`, `CLF`,  `VO`, `Funds in VO`),
                 
                 "Funds: Not Delivered"=filter(VO_Funds,`District` %in% input$districtDT & `Block` %in% input$blockDT & (`ICF Received?`=="No" | `FSF Received?`=="No" | `HRF Received?`=="No") & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select( `Block`, `CLF`,`VO`, `VO DoF`, `Last Voucher VO`, `ICF Received?`, `HRF Received?`, `FSF Received?`),
                 "Funds: Not Delivered @ SHG"=filter(SHG_Vouchers,`District` %in% input$districtDT & `Block` %in% input$blockDT & (`ICF Received?`=="No" | `FSF Received?`=="No" | `RF Received?`=="No" | `GL Received?`=="No"))%>%select( `Block`, `CLF`, `Panchayat`, `Village`, `VO`, `Last Voucher VO`, `SHG`, `SHG DoF`, `Last Voucher SHG`, `ICF Received?`, `FSF Received?`, `RF Received?`, `GL Received?`),
                 
                 "Funds: Diverted"=filter(VO_Funds,`District` %in% input$districtDT & `Block` %in% input$blockDT & (`Fund Diversion: ICF?`=="Yes" | `Fund Diversion: HRF?`=="Yes" | `Fund Diversion: FSF?`=="Yes") & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select( `Block`, `CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `Fund Diversion: ICF?`, `Fund Diversion: HRF?`, `Fund Diversion: FSF?`),
                 "Funds: Excess"=filter(VO_Funds,`District` %in% input$districtDT & `Block` %in% input$blockDT & (`Excess ICF?`=="Yes" | `Excess RF?`=="Yes" | `Excess FSF?`=="Yes") & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select( `Block`, `CLF`,`VO`, `VO DoF`, `Last Voucher VO`,  `Excess ICF?`,`Excess RF?`,`Excess FSF?`),
                 
                 "Performance: ICF Loans"=filter(VO_Recoveries,`District` %in% input$districtDT & `Block` %in% input$blockDT & Loan=="ICF" & (`VO Disbursed`-`VO Recovered`>0) & (period>3| `VO Repayment`<90))%>%select( `Block`, `CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `VO Last Recovered`, `VO Disbursed`, `VO Recovered`, `VO Recovery`, `VO Repayment`),
                 "Performance: ICF Loans @ SHG"=filter(SHG_Recoveries,`District` %in% input$districtDT & `Block` %in% input$blockDT & Loan=="ICF" & (`SHG Borrowed`-`SHG Repaid`>0) & (period>3| `SHG Repayment`<90))%>%select( `Block`, `CLF`, `Panchayat`, `Village`, `VO`, `VO DoF`, `Last Voucher VO`, `SHG`, `SHG Last Repay`, `SHG Borrowed`, `SHG Repaid`, `SHG Recovery`, `SHG Repayment`),
                 
                 "Performance: HRF Loans"=filter(VO_Recoveries,`District` %in% input$districtDT & `Block` %in% input$blockDT & Loan=="HRF" & (`VO Disbursed`-`VO Recovered`>0) & period>3)%>%select( `Block`, `CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `VO Last Recovered`, `VO Disbursed`, `VO Recovered`, `VO Recovery`),
                 "Performance: HRF Loans @ SHG"=filter(SHG_Recoveries,`District` %in% input$districtDT & `Block` %in% input$blockDT & Loan=="HRF" & (`SHG Borrowed`-`SHG Repaid`>0) & period>3)%>%select( `Block`, `CLF`, `Panchayat`, `Village`, `VO`, `VO DoF`, `Last Voucher VO`, `SHG`, `SHG Last Repay`, `SHG Borrowed`, `SHG Repaid`, `SHG Recovery`),
                 
                 "Performance: FSF Loans"=filter(VO_Recoveries,`District` %in% input$districtDT & `Block` %in% input$blockDT & Loan=="FSF" & (`VO Disbursed`-`VO Recovered`>0) & period>3)%>%select( `Block`, `CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `VO Last Recovered`, `VO Disbursed`, `VO Recovered`, `VO Recovery`),
                 "Performance: FSF Loans @ SHG"=filter(SHG_Recoveries,`District` %in% input$districtDT & `Block` %in% input$blockDT & Loan=="FSF" & (`SHG Borrowed`-`SHG Repaid`>0) & period>3)%>%select( `Block`, `CLF`, `Panchayat`, `Village`, `VO`, `VO DoF`, `Last Voucher VO`, `SHG`, `SHG Last Repay`, `SHG Borrowed`, `SHG Repaid`, `SHG Recovery`),
                 
                 "Performance: GENERAL Loans"=filter(VO_Recoveries,`District` %in% input$districtDT & `Block` %in% input$blockDT & Loan=="GENERAL" & (`VO Disbursed`-`VO Recovered`>0) & period>3)%>%select( `Block`, `CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `VO Last Recovered`, `VO Disbursed`, `VO Recovered`, `VO Recovery`),
                 "Performance: GENERAL Loans @ SHG"=filter(SHG_Recoveries,`District` %in% input$districtDT & `Block` %in% input$blockDT & Loan=="GENERAL" & (`SHG Borrowed`-`SHG Repaid`>0) & period>3)%>%select( `Block`, `CLF`, `Panchayat`, `Village`, `VO`, `VO DoF`, `Last Voucher VO`, `SHG`, `SHG Last Repay`, `SHG Borrowed`, `SHG Repaid`, `SHG Recovery`),
                 
                 "Dormancy @ SHG"=filter(SHG_Vouchers,`District` %in% input$districtDT & `Block` %in% input$blockDT & Alert %in% c("Dormant 3 months", "No Transactions"))%>%select( `Block`, `CLF`, `Panchayat`, `Village`, `VO`, `Last Voucher VO`, `Alert`, `SHG`, `Last Voucher SHG`)          )
        else(
          switch(input$topicDT,
                 "Extreme VO Monitoring"=filter(VO_Funds, `District` %in% input$districtDT & `Block` %in% input$blockDT & `CLF` %in% input$clfDT &  (Alert1!="" | Alert2!="" | Alert3!="" | Alert4!="" | Alert5!="" | Alert6!=""| Alert7!=""))%>%select(`CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `Alert1`, `Alert2`, `Alert3`, `Alert4`, `Alert5`, `Alert6`, `Alert7`),
                 "Extreme VO Monitoring @ CLF"=filter(CLF_Level, `District` %in% input$districtDT & `Block` %in% input$blockDT & `CLF` %in% input$clfDT )%>%select(`CLF`, `CLF: Ready`, `VOs Federated`, `VOs: Allowed`, `VOs: Extreme`),
                 
                 "Fund Available"=filter(VO_Funds,`District` %in% input$districtDT & `Block` %in% input$blockDT & `CLF` %in% input$clfDT & `Funds in VO`>=200000 & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select( `CLF`,  `VO`, `Funds in VO`),
                 
                 "Funds: Not Delivered"=filter(VO_Funds,`District` %in% input$districtDT & `Block` %in% input$blockDT & `CLF` %in% input$clfDT & (`ICF Received?`=="No" | `FSF Received?`=="No" | `HRF Received?`=="No") & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select( `CLF`,`VO`, `VO DoF`, `Last Voucher VO`, `ICF Received?`, `HRF Received?`, `FSF Received?`),
                 "Funds: Not Delivered @ SHG"=filter(SHG_Vouchers,`District` %in% input$districtDT & `Block` %in% input$blockDT & `CLF` %in% input$clfDT & (`ICF Received?`=="No" | `FSF Received?`=="No" | `RF Received?`=="No" | `GL Received?`=="No"))%>%select( `CLF`, `Panchayat`, `Village`, `VO`, `Last Voucher VO`, `SHG`, `SHG DoF`, `Last Voucher SHG`, `ICF Received?`, `FSF Received?`, `RF Received?`, `GL Received?`),
                 
                 "Funds: Diverted"=filter(VO_Funds,`District` %in% input$districtDT & `Block` %in% input$blockDT & `CLF` %in% input$clfDT & (`Fund Diversion: ICF?`=="Yes" | `Fund Diversion: HRF?`=="Yes" | `Fund Diversion: FSF?`=="Yes") & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select( `CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `Fund Diversion: ICF?`, `Fund Diversion: HRF?`, `Fund Diversion: FSF?`),
                 "Funds: Excess"=filter(VO_Funds,`District` %in% input$districtDT & `Block` %in% input$blockDT & `CLF` %in% input$clfDT & (`Excess ICF?`=="Yes" | `Excess RF?`=="Yes" | `Excess FSF?`=="Yes") & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select( `CLF`,`VO`, `VO DoF`, `Last Voucher VO`,  `Excess ICF?`,`Excess RF?`,`Excess FSF?`),
                 
                 "Performance: ICF Loans"=filter(VO_Recoveries,`District` %in% input$districtDT & `Block` %in% input$blockDT & `CLF` %in% input$clfDT & Loan=="ICF" & (`VO Disbursed`-`VO Recovered`>0) & (period>3| `VO Repayment`<90))%>%select( `CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `VO Last Recovered`, `VO Disbursed`, `VO Recovered`, `VO Recovery`, `VO Repayment`),
                 "Performance: ICF Loans @ SHG"=filter(SHG_Recoveries,`District` %in% input$districtDT & `Block` %in% input$blockDT & `CLF` %in% input$clfDT & Loan=="ICF" & (`SHG Borrowed`-`SHG Repaid`>0) & (period>3| `SHG Repayment`<90))%>%select( `CLF`, `Panchayat`, `Village`, `VO`, `VO DoF`, `Last Voucher VO`, `SHG`, `SHG Last Repay`, `SHG Borrowed`, `SHG Repaid`, `SHG Recovery`, `SHG Repayment`),
                 
                 "Performance: HRF Loans"=filter(VO_Recoveries,`District` %in% input$districtDT & `Block` %in% input$blockDT & `CLF` %in% input$clfDT & Loan=="HRF" & (`VO Disbursed`-`VO Recovered`>0) & period>3)%>%select( `CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `VO Last Recovered`, `VO Disbursed`, `VO Recovered`, `VO Recovery`),
                 "Performance: HRF Loans @ SHG"=filter(SHG_Recoveries,`District` %in% input$districtDT & `Block` %in% input$blockDT & `CLF` %in% input$clfDT & Loan=="HRF" & (`SHG Borrowed`-`SHG Repaid`>0) & period>3)%>%select( `CLF`, `Panchayat`, `Village`, `VO`, `VO DoF`, `Last Voucher VO`, `SHG`, `SHG Last Repay`, `SHG Borrowed`, `SHG Repaid`, `SHG Recovery`),
                 
                 "Performance: FSF Loans"=filter(VO_Recoveries,`District` %in% input$districtDT & `Block` %in% input$blockDT & `CLF` %in% input$clfDT & Loan=="FSF" & (`VO Disbursed`-`VO Recovered`>0) & period>3)%>%select( `CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `VO Last Recovered`, `VO Disbursed`, `VO Recovered`, `VO Recovery`),
                 "Performance: FSF Loans @ SHG"=filter(SHG_Recoveries,`District` %in% input$districtDT & `Block` %in% input$blockDT & `CLF` %in% input$clfDT & Loan=="FSF" & (`SHG Borrowed`-`SHG Repaid`>0) & period>3)%>%select( `CLF`, `Panchayat`, `Village`, `VO`, `VO DoF`, `Last Voucher VO`, `SHG`, `SHG Last Repay`, `SHG Borrowed`, `SHG Repaid`, `SHG Recovery`),
                 
                 "Performance: GENERAL Loans"=filter(VO_Recoveries,`District` %in% input$districtDT & `Block` %in% input$blockDT & `CLF` %in% input$clfDT & Loan=="GENERAL" & (`VO Disbursed`-`VO Recovered`>0) & period>3)%>%select( `CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `VO Last Recovered`, `VO Disbursed`, `VO Recovered`, `VO Recovery`),
                 "Performance: GENERAL Loans @ SHG"=filter(SHG_Recoveries,`District` %in% input$districtDT & `Block` %in% input$blockDT & `CLF` %in% input$clfDT & Loan=="GENERAL" & (`SHG Borrowed`-`SHG Repaid`>0) & period>3)%>%select( `CLF`, `Panchayat`, `Village`, `VO`, `VO DoF`, `Last Voucher VO`, `SHG`, `SHG Last Repay`, `SHG Borrowed`, `SHG Repaid`, `SHG Recovery`),
                 
                 "Dormancy @ SHG"=filter(SHG_Vouchers,`District` %in% input$districtDT & `Block` %in% input$blockDT & `CLF` %in% input$clfDT & Alert %in% c("Dormant 3 months", "No Transactions"))%>%select( `CLF`, `Panchayat`, `Village`, `VO`, `Last Voucher VO`, `Alert`, `SHG`, `Last Voucher SHG`)
          )
        )
      }
    }
  })
  
  
  observe({
  input$triggerDT
    isolate({
      dataset<-database()
      output$table<-DT::renderDataTable(dataset, filter="top", escape=TRUE, style="bootstrap")
      
      output$downloadDT<-downloadHandler(filename=function(){
        paste(input$topicDT, " @ ", Sys.Date(), ".xlsx", sep=" ")},
        content = function(file) {
          export(dataset, format="xlsx", file)
        }
      )
    })
  }) 
  
  #######################################################################################################################################33  
  
  #########################################################################   ######################################################################### 
  
  output$blockselectDTH<-renderUI({
    if(is.null(input$districtDTH))
      return("Select District, for Blocks")
    if(length(input$districtDTH)>1)
      return("To Activate Block View, select only One District above")
    block_name<-filter(Block, `District`==input$districtDTH)%>%select(`Block`)
    selectInput("blockDTH", "Select Block(s)", choices = block_name, multiple=TRUE, selected=NULL)
  })
  
  output$clfselectDTH<-renderUI({
    if(is.null(input$districtDTH))
      return("Select District, for CLFs")
    if(is.null(input$blockDTH))
      return("Select Block, for CLFs")
    if(length(input$districtDTH)>1 | length(input$blockDTH)>1 )
      return("To Activate CLF View, select only One District & Block")
    clf_name<-filter(CLF, `District`==input$districtDTH & `Block`==input$blockDTH)%>%select(`CLF`)
    selectInput("clfDTH", "Select CLF(s)", choices = clf_name, multiple=TRUE, selected=NULL)
  })
  
  databaseH<-reactive({
    if(is.null(input$districtDTH)){
      switch(input$topicDTH,
             "Extreme VO Monitoring"=filter(VO_Funds, `District` %in% input$districtDTH & (Alert1!="" | Alert2!="" | Alert3!="" | Alert4!="" | Alert5!="" | Alert6!=""| Alert7!=""))%>%select(`District`, `Block`, `CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `Alert1`, `Alert2`, `Alert3`, `Alert4`, `Alert5`, `Alert6`, `Alert7`),
             "Extreme VO Monitoring @ CLF"=filter(CLF_Level, `District` %in% input$districtDTH)%>%select(`District`, `Block`, `CLF`, `CLF: Ready`, `VOs Federated`, `VOs: Allowed`, `VOs: Extreme`),
             
             "Fund Available"=filter(VO_Funds, `District` %in% input$districtDTH & `Funds in VO`>=200000 & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select(`District`, `Block`, 8, 14, 84),
             
             "Funds: Not Delivered"=filter(VO_Funds, `District` %in% input$districtDTH & (`ICF Received?`=="No" | `FSF Received?`=="No" | `HRF Received?`=="No") & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select(`District`, `Block`, 8, 14, 82, 83, 50, 52, 54),
             "Funds: Not Delivered @ SHG"=filter(SHG_Vouchers, `District` %in% input$districtDTH &  (`ICF Received?`=="No" | `FSF Received?`=="No" | `RF Received?`=="No" | `GL Received?`=="No"))%>%select(`District`, `Block`, 13, 15, 39, 21, 38, 40, 29, 31, 33, 35),
             
             "Funds: Diverted"=filter(VO_Funds, `District` %in% input$districtDTH & (`Fund Diversion: ICF?`=="Yes" | `Fund Diversion: HRF?`=="Yes" | `Fund Diversion: FSF?`=="Yes") & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select(`District`, `Block`, 8, 14, 82, 83, 62, 64, 66),
             "Funds: Excess"=filter(VO_Funds, `District` %in% input$districtDTH & (`Excess ICF?`=="Yes" | `Excess RF?`=="Yes" | `Excess FSF?`=="Yes") & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select(`District`, `Block`, 8, 14, 82, 83, 56, 58, 60),
             
             "Performance: ICF Loans"=filter(VO_Recoveries, `District` %in% input$districtDTH & Loan=="ICF" & (`VO Disbursed`-`VO Recovered`>0) & (period>3| `VO Repayment`<90))%>%select(`District`, `Block`, 9, 12, 27, 28, 22, 23, 24, 25, 26),
             "Performance: ICF Loans @ SHG"=filter(SHG_Recoveries, `District` %in% input$districtDTH & Loan=="ICF" & (`SHG Borrowed`-`SHG Repaid`>0) & (period>3| `SHG Repayment`<90))%>%select(`District`, `Block`, 9, 12, 31, 33, 18, 26, 27, 28, 29, 30),
             
             "Performance: HRF Loans"=filter(VO_Recoveries, `District` %in% input$districtDTH & Loan=="HRF" & (`VO Disbursed`-`VO Recovered`>0) & period>3)%>%select(`District`, `Block`, 9, 12, 27, 28, 22, 23, 24, 25),
             "Performance: HRF Loans @ SHG"=filter(SHG_Recoveries, `District` %in% input$districtDTH & Loan=="HRF" & (`SHG Borrowed`-`SHG Repaid`>0) & period>3)%>%select(`District`, `Block`, 9, 12, 31, 33, 18, 26, 27, 28, 29),
             
             "Performance: FSF Loans"=filter(VO_Recoveries, `District` %in% input$districtDTH & Loan=="FSF" & (`VO Disbursed`-`VO Recovered`>0) & period>3)%>%select(`District`, `Block`, 9, 12, 27, 28, 22, 23, 24, 25),
             "Performance: FSF Loans @ SHG"=filter(SHG_Recoveries, `District` %in% input$districtDTH & Loan=="FSF" & (`SHG Borrowed`-`SHG Repaid`>0) & period>3)%>%select(`District`, `Block`, 9, 12, 31, 33, 18, 26, 27, 28, 29),
             
             "Performance: GENERAL Loans"=filter(VO_Recoveries, `District` %in% input$districtDTH & Loan=="GENERAL" & (`VO Disbursed`-`VO Recovered`>0) & period>3)%>%select(`District`, `Block`, 9, 12, 27, 28, 22, 23, 24, 25),
             "Performance: GENERAL Loans @ SHG"=filter(SHG_Recoveries, `District` %in% input$districtDTH & Loan=="GENERAL" & (`SHG Borrowed`-`SHG Repaid`>0) & period>3)%>%select(`District`, `Block`, 9, 12, 31, 33, 18, 26, 27, 28, 29),
             
             "Dormancy @ SHG"=filter(SHG_Vouchers, `District` %in% input$districtDTH & Alert %in% c("Dormant 3 months", "No Transactions"))%>%select(`District`, `Block`, 13, 15, 39, 36, 21, 40)
      )
    } else {
      if(is.null(input$blockDTH) & !is.null(input$districtDTH)){
        switch(input$topicDTH,
               "Extreme VO Monitoring"=filter(VO_Funds, `District` %in% input$districtDTH & (Alert1!="" | Alert2!="" | Alert3!="" | Alert4!="" | Alert5!="" | Alert6!=""| Alert7!=""))%>%select(`District`, `Block`, `CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `Alert1`, `Alert2`, `Alert3`, `Alert4`, `Alert5`, `Alert6`, `Alert7`),
               "Extreme VO Monitoring @ CLF"=filter(CLF_Level, `District` %in% input$districtDTH)%>%select(`District`, `Block`, `CLF`, `CLF: Ready`, `VOs Federated`, `VOs: Allowed`, `VOs: Extreme`),
               
               "Fund Available"=filter(VO_Funds, `District` %in% input$districtDTH & `Funds in VO`>=200000 & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select(`District`, `Block`, 8, 14, 84),
               
               "Funds: Not Delivered"=filter(VO_Funds, `District` %in% input$districtDTH & (`ICF Received?`=="No" | `FSF Received?`=="No" | `HRF Received?`=="No") & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select(`District`, `Block`, 8, 14, 82, 83, 50, 52, 54),
               "Funds: Not Delivered @ SHG"=filter(SHG_Vouchers, `District` %in% input$districtDTH &  (`ICF Received?`=="No" | `FSF Received?`=="No" | `RF Received?`=="No" | `GL Received?`=="No"))%>%select(`District`, `Block`, 13, 15, 39, 21, 38, 40, 29, 31, 33, 35),
               
               "Funds: Diverted"=filter(VO_Funds, `District` %in% input$districtDTH & (`Fund Diversion: ICF?`=="Yes" | `Fund Diversion: HRF?`=="Yes" | `Fund Diversion: FSF?`=="Yes") & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select(`District`, `Block`, 8, 14, 82, 83, 62, 64, 66),
               "Funds: Excess"=filter(VO_Funds, `District` %in% input$districtDTH & (`Excess ICF?`=="Yes" | `Excess RF?`=="Yes" | `Excess FSF?`=="Yes") & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select(`District`, `Block`, 8, 14, 82, 83, 56, 58, 60),
               
               "Performance: ICF Loans"=filter(VO_Recoveries, `District` %in% input$districtDTH & Loan=="ICF" & (`VO Disbursed`-`VO Recovered`>0) & (period>3| `VO Repayment`<90))%>%select(`District`, `Block`, 9, 12, 27, 28, 22, 23, 24, 25, 26),
               "Performance: ICF Loans @ SHG"=filter(SHG_Recoveries, `District` %in% input$districtDTH & Loan=="ICF" & (`SHG Borrowed`-`SHG Repaid`>0) & (period>3| `SHG Repayment`<90))%>%select(`District`, `Block`, 9, 12, 31, 33, 18, 26, 27, 28, 29, 30),
               
               "Performance: HRF Loans"=filter(VO_Recoveries, `District` %in% input$districtDTH & Loan=="HRF" & (`VO Disbursed`-`VO Recovered`>0) & period>3)%>%select(`District`, `Block`, 9, 12, 27, 28, 22, 23, 24, 25),
               "Performance: HRF Loans @ SHG"=filter(SHG_Recoveries, `District` %in% input$districtDTH & Loan=="HRF" & (`SHG Borrowed`-`SHG Repaid`>0) & period>3)%>%select(`District`, `Block`, 9, 12, 31, 33, 18, 26, 27, 28, 29),
               
               "Performance: FSF Loans"=filter(VO_Recoveries, `District` %in% input$districtDTH & Loan=="FSF" & (`VO Disbursed`-`VO Recovered`>0) & period>3)%>%select(`District`, `Block`, 9, 12, 27, 28, 22, 23, 24, 25),
               "Performance: FSF Loans @ SHG"=filter(SHG_Recoveries, `District` %in% input$districtDTH & Loan=="FSF" & (`SHG Borrowed`-`SHG Repaid`>0) & period>3)%>%select(`District`, `Block`, 9, 12, 31, 33, 18, 26, 27, 28, 29),
               
               "Performance: GENERAL Loans"=filter(VO_Recoveries, `District` %in% input$districtDTH & Loan=="GENERAL" & (`VO Disbursed`-`VO Recovered`>0) & period>3)%>%select(`District`, `Block`, 9, 12, 27, 28, 22, 23, 24, 25),
               "Performance: GENERAL Loans @ SHG"=filter(SHG_Recoveries, `District` %in% input$districtDTH & Loan=="GENERAL" & (`SHG Borrowed`-`SHG Repaid`>0) & period>3)%>%select(`District`, `Block`, 9, 12, 31, 33, 18, 26, 27, 28, 29),
               
               "Dormancy @ SHG"=filter(SHG_Vouchers, `District` %in% input$districtDTH & Alert %in% c("Dormant 3 months", "No Transactions"))%>%select(`District`, `Block`, 13, 15, 39, 36, 21, 40)
        )
      } else{
        if(!is.null(input$blockDTH) & !is.null(input$districtDTH) & is.null(input$clfDTH))
          switch(input$topicDTH,
                 "Extreme VO Monitoring"=filter(VO_Funds, `District` %in% input$districtDTH & `Block` %in% input$blockDTH & (Alert1!="" | Alert2!="" | Alert3!="" | Alert4!="" | Alert5!="" | Alert6!=""| Alert7!=""))%>%select(`Block`, `CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `Alert1`, `Alert2`, `Alert3`, `Alert4`, `Alert5`, `Alert6`, `Alert7`),
                 "Extreme VO Monitoring @ CLF"=filter(CLF_Level, `District` %in% input$districtDTH & `Block` %in% input$blockDTH)%>%select(`Block`, `CLF`, `CLF: Ready`, `VOs Federated`, `VOs: Allowed`, `VOs: Extreme`),
                 
                 "Fund Available"=filter(VO_Funds, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH &  `Funds in VO`>=200000 & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select(`Block`, 8, 14, 84),
                 
                 "Funds: Not Delivered"=filter(VO_Funds, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH & (`ICF Received?`=="No" | `FSF Received?`=="No" | `HRF Received?`=="No") & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select(`Block`, 8, 14, 82, 83, 50, 52, 54),
                 "Funds: Not Delivered @ SHG"=filter(SHG_Vouchers, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH &  (`ICF Received?`=="No" | `FSF Received?`=="No" | `RF Received?`=="No" | `GL Received?`=="No"))%>%select(`Block`, 13, 15, 39, 21, 38, 40, 29, 31, 33, 35),
                 
                 "Funds: Diverted"=filter(VO_Funds, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH & (`Fund Diversion: ICF?`=="Yes" | `Fund Diversion: HRF?`=="Yes" | `Fund Diversion: FSF?`=="Yes") & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select(`Block`, 8, 14, 82, 83, 62, 64, 66),
                 "Funds: Excess"=filter(VO_Funds, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH & (`Excess ICF?`=="Yes" | `Excess RF?`=="Yes" | `Excess FSF?`=="Yes") & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select(`Block`, 8, 14, 82, 83, 56, 58, 60),
                 
                 "Performance: ICF Loans"=filter(VO_Recoveries, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH & Loan=="ICF" & (`VO Disbursed`-`VO Recovered`>0) & (period>3| `VO Repayment`<90))%>%select(`Block`, 9, 12, 27, 28, 22, 23, 24, 25, 26),
                 "Performance: ICF Loans @ SHG"=filter(SHG_Recoveries, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH & Loan=="ICF" & (`SHG Borrowed`-`SHG Repaid`>0) & (period>3| `SHG Repayment`<90))%>%select(`Block`, 9, 12, 31, 33, 18, 26, 27, 28, 29, 30),
                 
                 "Performance: HRF Loans"=filter(VO_Recoveries, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH & Loan=="HRF" & (`VO Disbursed`-`VO Recovered`>0) & period>3)%>%select(`Block`, 9, 12, 27, 28, 22, 23, 24, 25),
                 "Performance: HRF Loans @ SHG"=filter(SHG_Recoveries, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH & Loan=="HRF" & (`SHG Borrowed`-`SHG Repaid`>0) & period>3)%>%select(`Block`, 9, 12, 31, 33, 18, 26, 27, 28, 29),
                 
                 "Performance: FSF Loans"=filter(VO_Recoveries, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH & Loan=="FSF" & (`VO Disbursed`-`VO Recovered`>0) & period>3)%>%select(`Block`, 9, 12, 27, 28, 22, 23, 24, 25),
                 "Performance: FSF Loans @ SHG"=filter(SHG_Recoveries, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH & Loan=="FSF" & (`SHG Borrowed`-`SHG Repaid`>0) & period>3)%>%select(`Block`, 9, 12, 31, 33, 18, 26, 27, 28, 29),
                 
                 "Performance: GENERAL Loans"=filter(VO_Recoveries, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH & Loan=="GENERAL" & (`VO Disbursed`-`VO Recovered`>0) & period>3)%>%select(`Block`, 9, 12, 27, 28, 22, 23, 24, 25),
                 "Performance: GENERAL Loans @ SHG"=filter(SHG_Recoveries, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH & Loan=="GENERAL" & (`SHG Borrowed`-`SHG Repaid`>0) & period>3)%>%select(`Block`, 9, 12, 31, 33, 18, 26, 27, 28, 29),
                 
                 "Dormancy @ SHG"=filter(SHG_Vouchers, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH & Alert %in% c("Dormant 3 months", "No Transactions"))%>%select(`Block`, 13, 15, 39, 36, 21, 40)
          )
      else(
        switch(input$topicDTH,
               "Extreme VO Monitoring"=filter(VO_Funds, `District` %in% input$districtDTH & `Block` %in% input$blockDTH  & `CLF` %in% input$clfDTH & (Alert1!="" | Alert2!="" | Alert3!="" | Alert4!="" | Alert5!="" | Alert6!=""| Alert7!=""))%>%select(`CLF`, `VO`, `VO DoF`, `Last Voucher VO`, `Alert1`, `Alert2`, `Alert3`, `Alert4`, `Alert5`, `Alert6`, `Alert7`),
               "Extreme VO Monitoring @ CLF"=filter(CLF_Level, `District` %in% input$districtDTH & `Block` %in% input$blockDTH & `CLF` %in% input$clfDTH)%>%select(`CLF`, `CLF: Ready`, `VOs Federated`, `VOs: Allowed`, `VOs: Extreme`),
               
               "Fund Available"=filter(VO_Funds, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH  & `CLF` %in% input$clfDTH &  `Funds in VO`>=200000 & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select(8, 14, 84),
               
               "Funds: Not Delivered"=filter(VO_Funds, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH  & `CLF` %in% input$clfDTH & (`ICF Received?`=="No" | `FSF Received?`=="No" | `HRF Received?`=="No") & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select(8, 14, 82, 83, 50, 52, 54),
               "Funds: Not Delivered @ SHG"=filter(SHG_Vouchers, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH  & `CLF` %in% input$clfDTH & (`ICF Received?`=="No" | `FSF Received?`=="No" | `RF Received?`=="No" | `GL Received?`=="No"))%>%select(13, 15, 39, 21, 38, 40, 29, 31, 33, 35),
               
               "Funds: Diverted"=filter(VO_Funds, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH  & `CLF` %in% input$clfDTH & (`Fund Diversion: ICF?`=="Yes" | `Fund Diversion: HRF?`=="Yes" | `Fund Diversion: FSF?`=="Yes") & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select(8, 14, 82, 83, 62, 64, 66),
               "Funds: Excess"=filter(VO_Funds, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH  & `CLF` %in% input$clfDTH & (`Excess ICF?`=="Yes" | `Excess RF?`=="Yes" | `Excess FSF?`=="Yes") & Alert1=="" & Alert2=="" & Alert3=="" & Alert4=="" & Alert5=="" & Alert6=="" & Alert7=="")%>%select(8, 14, 82, 83, 56, 58, 60),
               
               "Performance: ICF Loans"=filter(VO_Recoveries, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH  & `CLF` %in% input$clfDTH & Loan=="ICF" & (`VO Disbursed`-`VO Recovered`>0) & (period>3| `VO Repayment`<90))%>%select(9, 12, 27, 28, 22, 23, 24, 25, 26),
               "Performance: ICF Loans @ SHG"=filter(SHG_Recoveries, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH  & `CLF` %in% input$clfDTH & Loan=="ICF" & (`SHG Borrowed`-`SHG Repaid`>0) & (period>3| `SHG Repayment`<90))%>%select(9, 12, 31, 33, 18, 26, 27, 28, 29, 30),
               
               "Performance: HRF Loans"=filter(VO_Recoveries, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH  & `CLF` %in% input$clfDTH & Loan=="HRF" & (`VO Disbursed`-`VO Recovered`>0) & period>3)%>%select(9, 12, 27, 28, 22, 23, 24, 25),
               "Performance: HRF Loans @ SHG"=filter(SHG_Recoveries, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH  & `CLF` %in% input$clfDTH & Loan=="HRF" & (`SHG Borrowed`-`SHG Repaid`>0) & period>3)%>%select(9, 12, 31, 33, 18, 26, 27, 28, 29),
               
               "Performance: FSF Loans"=filter(VO_Recoveries, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH  & `CLF` %in% input$clfDTH & Loan=="FSF" & (`VO Disbursed`-`VO Recovered`>0) & period>3)%>%select(9, 12, 27, 28, 22, 23, 24, 25),
               "Performance: FSF Loans @ SHG"=filter(SHG_Recoveries, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH  & `CLF` %in% input$clfDTH & Loan=="FSF" & (`SHG Borrowed`-`SHG Repaid`>0) & period>3)%>%select(9, 12, 31, 33, 18, 26, 27, 28, 29),
               
               "Performance: GENERAL Loans"=filter(VO_Recoveries, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH  & `CLF` %in% input$clfDTH & Loan=="GENERAL" & (`VO Disbursed`-`VO Recovered`>0) & period>3)%>%select(9, 12, 27, 28, 22, 23, 24, 25),
               "Performance: GENERAL Loans @ SHG"=filter(SHG_Recoveries, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH  & `CLF` %in% input$clfDTH & Loan=="GENERAL" & (`SHG Borrowed`-`SHG Repaid`>0) & period>3)%>%select(9, 12, 31, 33, 18, 26, 27, 28, 29),
               
               "Dormancy @ SHG"=filter(SHG_Vouchers, `District` %in% input$districtDTH &  `Block` %in% input$blockDTH  & `CLF` %in% input$clfDTH & Alert %in% c("Dormant 3 months", "No Transactions"))%>%select(13, 15, 39, 36, 21, 40)
          )
        )
      }
    }
  })
  
  observe({
    input$triggerDTH
    isolate({
      datasetH<-databaseH()
      output$tableH<-DT::renderDataTable(datasetH, filter="top", escape=TRUE, style="bootstrap")
      
      output$downloadDTH<-downloadHandler(filename=function(){
        paste(input$topicDTH, " @ ", Sys.Date(), ".xlsx", sep=" ")},
        content = function(file) {
          export(datasetH, format="xlsx", file)
        }
      )
    })
  }) 
  
  #######################################################################################################################################33  
  
}


shinyApp(ui = ui, server = server)
