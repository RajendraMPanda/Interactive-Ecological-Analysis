#Load packages
library(shiny)
library(ggplot2)
library(dplyr)
library(mgcv)
library(corrplot)
library(GGally)
library(raster)
library(rgdal)
library(reshape)
library(scales)
#library(ReporteRs)
#create a random dataset with 300 rows or records
#The mountain example
id <- c(1:300)
HERB <- sample(5:50, 300, replace = T)
SHRUB <- sample(0:19, 300, replace = T)
TREE <- sample(0:9, 300, replace = T)
SR <- HERB+SHRUB+TREE
Vegetation <- rep(c("Subtropical Forest","Tectona","Shorea","Tropical Dry Deciduous",
"Sub Apline", "Dry Evergreen Scrub", "Cedrus", "Grassland", "Orchard", "Dry Alpine Pasture", 
"Pine", "Wet Grasslands"), times = 25)
ELV <- sample(300:4000, 300, replace=T) 
SLP <- sample(1.5:22.5, 300, replace=T)
MAT <- sample(-1:25, 300, replace=T) 
TMX <- sample(12:30, 300, replace=T) 
TMN <- sample(-7:12, 300, replace=T) 
PAN <- sample(250:2500, 300, replace=T)
PDR <- sample(100:400, 300, replace=T) 
PET <- sample(500:1500, 300, replace=T)
PS <- sample(30:120, 300, replace=T) 
TS <- sample(450:750, 300, replace=T)
#Abbreviations
#SR = Species richness; SLP = Slope; MAT = Mean Annual Temperature; TMN = Minimum Temperature; TMX = Maximum Temperature; PAN = Mean Annual Precipitation; 
#PS = Precipitation seasonality; TS = Temperature seasonality; PDR = Precipitation of driest quarter;
#ELV = Elevation; PET = Potential evapotranspiration;
#A hypothetical data for mountain studies
MTData <- data.frame(Vegetation, SR, HERB, SHRUB, TREE, ELV, SLP, MAT, TMN, TMX, PAN, PDR, PET, PS, TS)

ui <- fluidPage(
titlePanel(h1("Interactive Ecological Analysis")),
titlePanel("The Mountain Example"),
titlePanel(h3("A tool guide to students, ecologists and instructors")),
sidebarLayout(
sidebarPanel(
    #fileInput("file1", "1. Upload the file", multiple = F,
              # accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    sliderInput("ELV", "2. Elevation Range", 0, 4500, c(500,3000), post = "m"),
    selectInput("Vegetation", "3. Select Vegetation Type", sort(unique(MTData$Vegetation)),
                selected = "Tropical Dry Deciduous"),
    selectInput(inputId = "Xvar", label = "4. Select the X variable", choices = colnames(MTData[,c(6:15)]), selected = "PET"),
    selectInput(inputId = "Yvar", label = "5. Select the Y variable", choices = colnames(MTData[,c(2:5)]), selected = "Herb"),
    sliderInput("bin", "6. Adjust Histogram Binwidth:",
                min=20, max=100, value=40, step=10),
    radioButtons("color", "7. Select Colour of Histogram",
                choices = c("blue", "green", "red"), selected = "green"),
    radioButtons(inputId = "plot",  label ="8.Select Plot Type",
                choices = c("png", "pdf", "jpeg")),
    radioButtons(inputId = "type",  label ="9.Select File Type",
                choices = c("csv", "txt", "docx")),
    # adding the new div tag to the sidebar            
    tags$div(class="header", checked=NA,
            tags$b(("10. Related References")),
            tags$a(href="https://onlinelibrary.wiley.com/doi/pdf/10.1002/ece3.3569", "Click Here!")),
    uiOutput("Vegetation")),
  
mainPanel(
    tabsetPanel(type="tab",
            tabPanel("Plot1", plotOutput("plot1")),
            tabPanel("Plot2", plotOutput("plot2"),
                downloadButton(outputId = "ScPlot", label = "Download Scatter Plot")),
            tabPanel("Plot3", plotOutput("plot3"),
                downloadButton(outputId = "CorPlot", label = "Download Correlation Plot")),
            tabPanel("Plot4", plotOutput("plot4"),
                downloadButton(outputId = "RegPlot", label = "Download Regession Plot")),
            tabPanel("Summary", verbatimTextOutput("summary"), 
                downloadButton(outputId = "SumTable", label = "Download Summary Table")), 
            tabPanel("CorrTable", tableOutput("correl"), 
                downloadButton(outputId = "CorTable", label = "Download Correlation Table")),
                tabPanel("Data", tableOutput("metadata")
            )))))

server <- function(input, output){
    output$Vegetation <- renderUI({})
      filteredData <- reactive({
      if (is.null(input$Vegetation)){
      return()
    }    
    MTData%>%
      filter(ELV >= input$ELV[1],
             ELV <= input$ELV[2],
      Vegetation == input$Vegetation)
  })
  # x contains all the observations of the x variable selected by the user. X is a reactive function
  x <- reactive({
    MTData[,as.character(input$Xvar)]
  })
  # x contains all the observations of the y variable selected by the user. Y is a reactive function
  y <- reactive({
    MTData[,as.character(input$Yvar)]
  })
  # xl contains the x variable or column name of the dataset
  xl <- reactive({
    names(MTData[as.character(input$Xvar)])
  })
  # yl contains the y variable or column name of the dataset
  yl <- reactive({
    names(MTData[as.character(input$Yvar)])
  })
  
  # Display the plot in the mainPanel
  output$plot2 <- renderPlot({
    if (is.null(filteredData())) {
      return()
    }
  plot(x=x(), y=y(), main = "Scatterplot", xlab = xl(), ylab = yl(), col="blue", pch=20, cex=.9)
  })
  
  output$plot1 <- renderPlot({
    if (is.null(filteredData())) {
      return()
    }
  #Adjust the bin of histogram
  mybin <- input$bin
  # Plot using ggplot2
  ggplot(filteredData(), aes(SR)) + ggtitle("Histogram") +
      #geom_histogram(colour="red", fill=("yellow"))
      geom_histogram(col=input$color, fill=("yellow"), bins = mybin)+ 
      theme(axis.text.x = element_text(colour="black",size=14,angle=0,hjust=0.5,vjust=0.5,face="plain"),
          axis.text.y = element_text(colour="black",size=14,angle=0,hjust=0.5,vjust=0.5,face="plain"),  
          axis.title.x = element_text(colour="black",size=14,angle=0,hjust=.5,vjust=0.5,face="plain"),
          axis.title.y = element_text(colour="black",size=14,angle=90,hjust=0.5,vjust=0.5,face="plain"),
          axis.line.x = element_line(color="grey20", size = 0.5),
          axis.line.y = element_line(color="grey20", size = 0.5),
          axis.ticks = element_line(colour = "grey20", size = 0.5),
          axis.ticks.length = unit(0.25, "cm"),
          #plot.title = element_text(family="Ariel", face="bold.italic", colour="Black", size=14),
          panel.background = element_rect(fill = "white"))
  })
  
  output$plot3 <- renderPlot({
    if (is.null(filteredData())) {
      return()
      }
     ggcorr(MTData[,c(7:15)], low = "steelblue", mid = "grey80",high = "orangered", legend.position = "bottom", legend.size = 16, label_size = 8,
           label = TRUE, label_alpha = FALSE, size=6, angle=0, nbreaks = 4)+ ggtitle("Correlation Plot")
    #+theme(plot.title = element_text(family="Ariel", face="bold.italic", colour="Black", size=14))
  })
  
  output$plot4<- renderPlot({
    if (is.null(filteredData())) {
      return()
      }
  ggplot(filteredData(), aes(x=PET, y=SR)) + ggtitle("Regression Plot") +
      geom_point(aes(x=PET, y=SR, color = "red" )) +
      geom_smooth(method = "lm", formula = y~poly(x,2), se = T, aes(col="red"), lty=1, lwd=1.25, show.legend=T)+  
      geom_smooth(method="gam", formula=y~s(x), se = T, lty=1,lwd=1.25, aes(col="blue"), show.legend=T)+
      scale_colour_manual("", labels=c("GAM", "Linear"), values=c("blue","red"))+
      scale_fill_manual(values=c("blue","red"))+
      theme(legend.position="top", 
          axis.text.x = element_text(colour="black",size=14,angle=0,hjust=0.5,vjust=0.5,face="plain"),
          axis.text.y = element_text(colour="black",size=14,angle=0,hjust=0.5,vjust=0.5,face="plain"),  
          axis.title.x = element_text(colour="black",size=14,angle=0,hjust=0.5,vjust=0.5,face="plain"),
          axis.title.y = element_text(colour="black",size=14,angle=90,hjust=0.5,vjust=0.5,face="plain"),
          axis.line.x = element_line(color="grey20", size = 0.5),
          axis.line.y = element_line(color="grey20", size = 0.5),
          axis.ticks = element_line(colour = "grey20", size = 0.5),
          axis.ticks.length = unit(0.25, "cm"))
          # plot.title = element_text(family="Ariel", face="bold.italic", colour="Black", size=14),
          # panel.background = element_rect(fill = "cornsilk"))
    })
  
 # Show the first six observations
  output$metadata <- renderTable({head(filteredData())
    })
 # Show correlation table without the first six columns 
  output$correl <- renderTable({cor(MTData[,-c(1:6)])
    })
  
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    summary(MTData[,-c(1:6)])
    })
  
  # Use 'downloadHandler' fun to download outputs
  plotext <- reactive({
    switch(input$plot,
           png = "png", pdf = "pdf", jpeg = "jpeg")
  }) 
  output$ScPlot <- downloadHandler(
    filename= function(){
    paste("scatr", plotext(), sep=".")},
    content = function(file){
    if(input$plot == "png")
    png(file)
    else if (input$plot == "pdf")
    pdf(file)
    else
    jpeg(file)
    print(plot(x=x(), y=y(), main = "Scatter Plot", xlab = xl(), ylab = yl(), col="blue", pch=20, cex=.9))
    dev.off()  # turn the device off
  })
  
  output$CorPlot <- downloadHandler(
    filename= function(){
      paste("corplot", plotext(), sep=".")},
    content = function(file){
      if(input$plot == "png")
        png(file)
      else
        jpeg(file)
      
  print(ggcorr(MTData[,c(7:15)], low = "steelblue", mid = "grey80", high = "orangered", legend.position = "bottom", 
          legend.size = 16, label_size = 6, label = TRUE, label_alpha = FALSE, size=6, angle=0, nbreaks = 4)+ ggtitle("Correlation Plot")
       # +theme(plot.title = element_text(family="Ariel", face="bold.italic", colour="Black", size=14)
       )
      dev.off()
  })     
    
  output$RegPlot <- downloadHandler(
    filename= function(){
      paste("regplot", plotext(), sep=".")},
    content = function(file){
      if(input$plot == "png")
        png(file)
      else if (input$plot == "pdf")
        pdf(file)
      else
        jpeg(file)
      
   print(ggplot(filteredData(), aes(x=PET, y=SR)) + ggtitle("Regression Plot") +
        geom_point(aes(x=PET, y=SR, color = "red" )) +
        geom_smooth(method = "lm", formula = y~poly(x,2), se = T, aes(col="red"), lty=1, lwd=1.5, show.legend=T)+  
        geom_smooth(method="gam", formula=y~s(x), se = T, lty=1,lwd=1.5, aes(col="blue"), show.legend=T)+
        scale_colour_manual("", labels=c("GAM", "Linear"), values=c("blue","red"))+
        scale_fill_manual(values=c("blue","red"))+
        theme(legend.position="top", 
            axis.text.x = element_text(colour="black",size=14,angle=0,hjust=0.5,vjust=.5,face="plain"),
            axis.text.y = element_text(colour="black",size=14,angle=0,hjust=0.5,vjust=.5,face="plain"),  
            axis.title.x = element_text(colour="black",size=14,angle=0,hjust=.5,vjust=0.5,face="plain"),
            axis.title.y = element_text(colour="black",size=14,angle=90,hjust=.5,vjust=.5,face="plain"),
            axis.line.x = element_line(color="grey20", size = 0.5),
            axis.line.y = element_line(color="grey20", size = 0.5),
            axis.ticks = element_line(colour = "grey20", size = 0.5),
            axis.ticks.length = unit(0.25, "cm")))
            # plot.title = element_text(family="Ariel", face="bold.italic", colour="Black", size=14),
            # panel.background = element_rect(fill = "cornsilk")))
       dev.off()  # turn the device off
  })
  
  filetext <- reactive({
        switch(input$type,
        csv = "csv", txt = "txt",  doc = "docx")
  })
  
  output$CorTable <- downloadHandler(
        filename = function() {
        paste("cor", filetext(), sep=".")},
        content = function(file){
          if(input$type == "csv")
            write.csv(file)
          else if (input$type == "txt")
            write.table(file)
          else
            writeDoc(file)
          print(cor(MTData[,-c(1:6)]))
  })
  output$SumTable <- downloadHandler(
        filename = function() {
        paste("sum", filetext(), sep=".")
      },
        content = function(file){
        if(input$type == "csv")
          write.csv(file)
        else if (input$type == "txt")
          write.table(file)
        else
          writeDoc(file)
        print(summary(MTData[,-c(1:6)]))
  })
  }
  shinyApp(ui = ui, server = server)
