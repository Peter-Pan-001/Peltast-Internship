#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
# Define UI for application that draws a histogram
library(shinydashboard)
library(leaflet)
world = rgdal::readOGR(dsn = "ne_50m_admin_0_countries",layer = 'ne_50m_admin_0_countries',encoding = 'UTF-8')
import <- read.csv("Final_Dataset_Location.csv")
world0 <- world
colnames(import)[16]<-"NAME_LONG"

HS <- c(84241090000, 40091100000, 38130000000, 40081190000, 29033990000)
product_names <- c('Fire Extinguisher', 'Hose', 'Foam and Powder', 'Insulation Rubber Sheet', 'Refrigerant')
names(HS) <- product_names

header <- dashboardHeader(
    title = "Annual Product Importation"
 )

body <- dashboardBody(
    fluidRow(
        column(width = 8,
              box(width = NULL, solidHeader = TRUE,
                  leafletOutput("map", height = 500)
                 
              )
        ),
        column(width = 4,
               box(width = NULL, status = "warning",
                   sliderInput("Start_Date",
                               "Start Date",
                               min = as.Date("2016-01-01","%Y-%m-%d"),
                               max = as.Date("2019-06-30","%Y-%m-%d"),
                               value = as.Date("2016-01-01"),
                               timeFormat="%Y-%m-%d")
               ),
               box(width = NULL, status = "warning",
                   sliderInput("End_Date",
                               "End Date",
                               min = as.Date("2016-01-01","%Y-%m-%d"),
                               max = as.Date("2019-06-30","%Y-%m-%d"),
                               value = as.Date("2019-06-30"),
                               timeFormat="%Y-%m-%d")
               ),
               box(width = NULL, status = "warning",
                   selectInput(inputId = "Product", "Product", product_names, selected = 'Fire Extinguisher'
                   )
               )
        )
    ),
    
    fluidRow(
      column(width = 8,
             uiOutput("describe"),
             box(width = NULL, solidHeader = TRUE,
                 #plotOutput("country")
                 plotlyOutput("country")
             )
      ),
      column(width = 4,
             box(width = NULL, status = "warning",
                 selectInput(inputId = "Y", "Scale", c("Gross_Mass", "Customs_Value", "Unit_Price"), selected = "Gross_Mass"
                 )
             ),
             box(width = NULL, status = "warning",
                 selectInput(inputId = "Currency", "Currency", c("USD", "PHP"), selected = "USD"
                 )
             ),
             box(width = NULL, status = "warning",
                 selectInput(inputId = "Group", "Group", c("Importer", "Exporter", "Country_Origin"), selected = "Country_Origin"
                 )
             )
      )
    ),
    
    fluidRow(
      column(width = 8,
             box(width = NULL, solidHeader = TRUE,
                 #plotOutput("time", height = 500)
                 plotlyOutput("time", height = 500)
                 
             )
      ),
      column(width = 4,
             box(width = NULL, status = "warning",
                 selectInput(inputId = "Timeseries_Comparison_Group", "Timeseries Comparison Group", c("Importer", "Exporter", "Country_Origin"), selected = "Country_Origin"
                 )
             ),
             box(width = NULL, status = "warning",
                 selectInput(inputId = "Comparison_Object_1", "Comparison Object 1", sort(unique(import$NAME_LONG)), selected = "AE"
                 )
             ),
             box(width = NULL, status = "warning",
                 selectInput(inputId = "Comparison_Object_2", "Comparison Object 2", sort(unique(import$NAME_LONG)), selected = "AL"
                 )
             ),
             box(width = NULL, status = "warning",
                 selectInput(inputId = "Timeseries_Currency", "Currency", c("USD", "PHP"), selected = "USD"
                 )
             ),
             box(width = NULL, status = "warning",
                 selectInput(inputId = "Timeseries_Comparison_Y", "Timeseries Comparison Scale", c("Gross_Mass", "Customs_Value", "Unit_Price"), selected = "Gross_Mass"
                 )
             ),
             checkboxInput("Line", "Line", FALSE),
             checkboxInput("Smooth", "Smooth", FALSE)
      )
    )
 )

ui <- dashboardPage(
    header,
    dashboardSidebar(disable = TRUE),
    body,
    skin = "black"
 )



# Define server logic required to draw a histogram
pal <- colorNumeric("Blues",NULL)
server <- function(input, output, session) {
  
    # dynamically change the choices of Comparison Object 1 and Comparison Object 2
    
    observe({
      if (input$Start_Date < input$End_Date){
        
        import$Date = as.Date(import$Date)
        choose_data <- import[which((import$HS_Code == HS[[input$Product]]) & (import$Date >= input$Start_Date) & (import$Date <= input$End_Date)),]
        comparison_group <- input$Timeseries_Comparison_Group
      
        if (comparison_group == "Country_Origin"){
          candidates = sort(unique(choose_data$NAME_LONG))
          updateSelectInput(session, "Comparison_Object_1",
                            label = "Country 1",
                            choices = candidates,
                            selected = head(candidates, 1))
          updateSelectInput(session, "Comparison_Object_2",
                            label = "Country 2",
                            choices = candidates,
                            selected = tail(candidates, 1))
        
        } else if (comparison_group == "Importer"){
          candidates = sort(unique(choose_data$Consignee))
          updateSelectInput(session, "Comparison_Object_1",
                            label = "Importer 1",
                            choices = candidates,
                            selected = head(candidates, 1))
          updateSelectInput(session, "Comparison_Object_2",
                            label = "Importer 2",
                            choices = candidates,
                            selected = tail(candidates, 1))
        
        } else if (comparison_group == "Exporter"){
          candidates = sort(unique(choose_data$Exporter))
          updateSelectInput(session, "Comparison_Object_1",
                            label = "Exporter 1",
                            choices = candidates,
                            selected = head(candidates, 1))
          updateSelectInput(session, "Comparison_Object_2",
                            label = "Exporter 2",
                            choices = candidates,
                            selected = tail(candidates, 1))
        }
      }
    })
  
    # end
    
    # start of map
  
    output$map <- renderLeaflet({
      
      if (input$Start_Date < input$End_Date){
        
        import$Date = as.Date(import$Date)
        mydata <- import[which((import$HS_Code == HS[[input$Product]]) & (import$Date >= input$Start_Date) & (import$Date <= input$End_Date)),]
        
        if (input$Currency == "PHP"){
          mydata$Customs_Value <- mydata$Customs_Value_PHP
          means = "PHP"
        } else{
          mydata$Customs_Value <- mydata$Customs_Value_USD
          means = "USD"
        }
        
        if (input$Y == "Gross_Mass"){
          
            mydata <- mydata%>%
              group_by(NAME_LONG)%>%
              summarise(Total_Mass=sum(Gross_Mass_KGS))
            datafiltered <- mydata[, c("NAME_LONG", "Total_Mass")]
            world@data <- plyr::join(x=world0@data, y=datafiltered, by="NAME_LONG", type = "left")
            leaflet(world) %>%
              addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                          opacity = 1.0, fillOpacity = 0.5,
                          #fillColor = ~colorQuantile("YlOrRd", Total_Value)(Total_Value),
                          fillColor = ~colorQuantile("Blues", Total_Mass)(Total_Mass),
                          label = ~paste0(NAME_LONG, ": ", prettyNum(round(Total_Mass, 1),big.mark=",",scientific=FALSE), " KGS"),
                          highlightOptions = highlightOptions(color = "white", weight = 2,
                                                              bringToFront = TRUE))%>%
              addLegend(pal = pal , values = ~(Total_Mass), opacity = 1.0, title = "Gross_Mass(kg)")
            
        }else if (input$Y == "Customs_Value"){
          
            mydata <- mydata%>%
              group_by(NAME_LONG)%>%
              summarise(Total_Customs_Value=sum(Customs_Value))
            datafiltered <- mydata[, c("NAME_LONG", "Total_Customs_Value")]
            
            world@data <- plyr::join(x=world0@data, y=datafiltered, by="NAME_LONG", type = "left")
            leaflet(world) %>%
              addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                          opacity = 1.0, fillOpacity = 0.5,
                          #fillColor = ~colorQuantile("YlOrRd", Total_Value)(Total_Value),
                          fillColor = ~colorQuantile("Blues", Total_Customs_Value)(Total_Customs_Value),
                          label = ~paste0(NAME_LONG, ": ", prettyNum(round(Total_Customs_Value, 1),big.mark=",",scientific=FALSE), " ", means),
                          highlightOptions = highlightOptions(color = "white", weight = 2,
                                                              bringToFront = TRUE))%>%
              addLegend(pal = pal , values = ~(Total_Customs_Value), opacity = 1.0, title = ~paste0("Total Value"," ",means))
            
         } 
          
         else if (input$Y == "Unit_Price"){
           
            mydata <- mydata%>%
              group_by(NAME_LONG)%>%
              summarise(Unit_Price=sum(Customs_Value)/sum(Gross_Mass_KGS))
            datafiltered <- mydata[, c("NAME_LONG", "Unit_Price")]
            
            world@data <- plyr::join(x=world0@data, y=datafiltered, by="NAME_LONG", type = "left")
            leaflet(world) %>%
              addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                          opacity = 1.0, fillOpacity = 0.5,
                          #fillColor = ~colorQuantile("YlOrRd", Total_Value)(Total_Value),
                          fillColor = ~colorQuantile("Blues", Unit_Price)(Unit_Price),
                          label = ~paste0(NAME_LONG, ": ", prettyNum(round(Unit_Price, 1),big.mark=",",scientific=FALSE), " ", means, "/KG"),
                          highlightOptions = highlightOptions(color = "white", weight = 2,
                                                              bringToFront = TRUE))%>%
              addLegend(pal = pal , values = ~(Unit_Price), opacity = 1.0, title = ~paste0("Unit Price"," ", means,"/KG"))
         
        }
      }
    })
    
    # end of map
    
    # start of printing the statistics
    
    output$describe <- renderUI({
      
      if (input$Start_Date < input$End_Date){
        
        import$Date = as.Date(import$Date)
        display_data <- import[which((import$HS_Code == HS[[input$Product]]) & (import$Date >= input$Start_Date) & (import$Date <= input$End_Date)),]
        
        if (input$Currency == "PHP"){
          display_data$Customs_Value <- display_data$Customs_Value_PHP
        } else{
          display_data$Customs_Value <- display_data$Customs_Value_USD
        }
        
        if (input$Y == "Gross_Mass"){
          
          if (input$Group == "Importer"){
            
            display_data <- display_data%>%
              group_by(Consignee)%>%
              summarise(Total_Mass=sum(Gross_Mass_KGS))%>%
              arrange(desc(Total_Mass))%>%
              top_n(50)
            
            HTML(paste(
              p(strong("Average:"), HTML('&nbsp;'), prettyNum(round(mean(display_data$Total_Mass), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Median:"), HTML('&nbsp;'), prettyNum(round(median(display_data$Total_Mass), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Max:"), HTML('&nbsp;'), prettyNum(round(max(display_data$Total_Mass), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Min:"), HTML('&nbsp;'), prettyNum(round(min(display_data$Total_Mass), 1),big.mark=",",scientific=FALSE)
              )
            )
            )
            
          } else if (input$Group == "Exporter"){
            
            display_data <- display_data%>%
              group_by(Exporter)%>%
              summarise(Total_Mass=sum(Gross_Mass_KGS))%>%
              arrange(desc(Total_Mass))%>%
              top_n(50)
            
            HTML(paste(
              p(strong("Average:"), HTML('&nbsp;'), prettyNum(round(mean(display_data$Total_Mass), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Median:"), HTML('&nbsp;'), prettyNum(round(median(display_data$Total_Mass), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Max:"), HTML('&nbsp;'), prettyNum(round(max(display_data$Total_Mass), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Min:"), HTML('&nbsp;'), prettyNum(round(min(display_data$Total_Mass), 1),big.mark=",",scientific=FALSE)
              )
            )
            )
            
          } else if (input$Group == "Country_Origin") {
            
            display_data <- display_data%>%
              group_by(NAME_LONG)%>%
              summarise(Total_Mass=sum(Gross_Mass_KGS))%>%
              arrange(desc(Total_Mass))
            
            HTML(paste(
              p(strong("Average:"), HTML('&nbsp;'), prettyNum(round(mean(display_data$Total_Mass), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Median:"), HTML('&nbsp;'), prettyNum(round(median(display_data$Total_Mass), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Max:"), HTML('&nbsp;'), prettyNum(round(max(display_data$Total_Mass), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Min:"), HTML('&nbsp;'), prettyNum(round(min(display_data$Total_Mass), 1),big.mark=",",scientific=FALSE)
              )
            )
            )
            
          }
          
        } else if (input$Y == "Customs_Value"){
          
          if (input$Group == "Importer"){
            
            display_data <- display_data%>%
              group_by(Consignee)%>%
              summarise(Total_Customs_Value=sum(Customs_Value))%>%
              arrange(desc(Total_Customs_Value))%>%
              top_n(50)
            
            HTML(paste(
              p(strong("Average:"), HTML('&nbsp;'), prettyNum(round(mean(display_data$Total_Customs_Value), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Median:"), HTML('&nbsp;'), prettyNum(round(median(display_data$Total_Customs_Value), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Max:"), HTML('&nbsp;'), prettyNum(round(max(display_data$Total_Customs_Value), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Min:"), HTML('&nbsp;'), prettyNum(round(min(display_data$Total_Customs_Value), 1),big.mark=",",scientific=FALSE)
              )
            )
            )
            
          } else if (input$Group == "Exporter"){
            
            display_data <- display_data%>%
              group_by(Exporter)%>%
              summarise(Total_Customs_Value=sum(Customs_Value))%>%
              arrange(desc(Total_Customs_Value))%>%
              top_n(50)
            
            HTML(paste(
              p(strong("Average:"), HTML('&nbsp;'), prettyNum(round(mean(display_data$Total_Customs_Value), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Median:"), HTML('&nbsp;'), prettyNum(round(median(display_data$Total_Customs_Value), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Max:"), HTML('&nbsp;'), prettyNum(round(max(display_data$Total_Customs_Value), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Min:"), HTML('&nbsp;'), prettyNum(round(min(display_data$Total_Customs_Value), 1),big.mark=",",scientific=FALSE)
              )
            )
            )
            
          } else if (input$Group == "Country_Origin") {
            
            display_data <- display_data%>%
              group_by(NAME_LONG)%>%
              summarise(Total_Customs_Value=sum(Customs_Value))%>%
              arrange(desc(Total_Customs_Value))
            
            HTML(paste(
              p(strong("Average:"), HTML('&nbsp;'), prettyNum(round(mean(display_data$Total_Customs_Value), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Median:"), HTML('&nbsp;'), prettyNum(round(median(display_data$Total_Customs_Value), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Max:"), HTML('&nbsp;'), prettyNum(round(max(display_data$Total_Customs_Value), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Min:"), HTML('&nbsp;'), prettyNum(round(min(display_data$Total_Customs_Value), 1),big.mark=",",scientific=FALSE)
              )
            )
            )
            
          }
          
        } else if (input$Y == "Unit_Price"){
          
          if (input$Group == "Importer"){
            
            display_data <- display_data%>%
              group_by(Consignee)%>%
              summarise(Unit_Price=sum(Customs_Value)/sum(Gross_Mass_KGS))%>%
              arrange(desc(Unit_Price))%>%
              top_n(50)
            
            HTML(paste(
              p(strong("Average:"), HTML('&nbsp;'), prettyNum(round(mean(display_data$Unit_Price), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Median:"), HTML('&nbsp;'), prettyNum(round(median(display_data$Unit_Price), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Max:"), HTML('&nbsp;'), prettyNum(round(max(display_data$Unit_Price), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Min:"), HTML('&nbsp;'), prettyNum(round(min(display_data$Unit_Price), 1),big.mark=",",scientific=FALSE)
              )
            )
            )
            
          } else if (input$Group == "Exporter"){
            
            display_data <- display_data%>%
              group_by(Exporter)%>%
              summarise(Unit_Price=sum(Customs_Value)/sum(Gross_Mass_KGS))%>%
              arrange(desc(Unit_Price))%>%
              top_n(50)
            
            HTML(paste(
              p(strong("Average:"), HTML('&nbsp;'), prettyNum(round(mean(display_data$Unit_Price), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Median:"), HTML('&nbsp;'), prettyNum(round(median(display_data$Unit_Price), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Max:"), HTML('&nbsp;'), prettyNum(round(max(display_data$Unit_Price), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Min:"), HTML('&nbsp;'), prettyNum(round(min(display_data$Unit_Price), 1),big.mark=",",scientific=FALSE)
              )
            )
            )
            
          } else if (input$Group == "Country_Origin") {
            
            display_data <- display_data%>%
              group_by(NAME_LONG)%>%
              summarise(Unit_Price=sum(Customs_Value)/sum(Gross_Mass_KGS))%>%
              arrange(desc(Unit_Price))
            
            HTML(paste(
              p(strong("Average:"), HTML('&nbsp;'), prettyNum(round(mean(display_data$Unit_Price), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Median:"), HTML('&nbsp;'), prettyNum(round(median(display_data$Unit_Price), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Max:"), HTML('&nbsp;'), prettyNum(round(max(display_data$Unit_Price), 1),big.mark=",",scientific=FALSE), HTML('&nbsp;'), HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'),
                strong("Min:"), HTML('&nbsp;'), prettyNum(round(min(display_data$Unit_Price), 1),big.mark=",",scientific=FALSE)
              )
            )
            )
            
          }
        }
      }
    })
    
    # end of printing statistics
    
    # start of bar plot
    output$country <- renderPlotly({
        if (input$Start_Date < input$End_Date){
          
          import$Date = as.Date(import$Date)
          plot_data <- import[which((import$HS_Code == HS[[input$Product]]) & (import$Date >= input$Start_Date) & (import$Date <= input$End_Date)),]
          
          if (input$Currency == "PHP"){
            plot_data$Customs_Value <- plot_data$Customs_Value_PHP
            means = "PHP"
          } else{
            plot_data$Customs_Value <- plot_data$Customs_Value_USD
            means = "USD"
          }
          
          if (input$Y == "Gross_Mass"){
            
            if (input$Group == "Importer"){
              
              plot_data <- plot_data%>%
                group_by(Consignee)%>%
                summarise(Total_Mass=sum(Gross_Mass_KGS))%>%
                arrange(desc(Total_Mass))%>%
                top_n(50)
              
              p <- ggplot(plot_data, aes(x=reorder(Consignee, -Total_Mass), y=Total_Mass))+
                geom_bar(stat="identity", aes(text=sprintf("Importer: %s<br>Gross Mass: %s KG", Consignee, prettyNum(round(Total_Mass, 1),big.mark=",",scientific=FALSE))), 
                         col="white", fill="light blue") +
                  labs(x="Importer", y="Gross Mass(KG)") + 
                    theme(axis.text.x = element_text(angle = 45, size = 5, color = 'blue'), 
                          axis.text.y = element_text(color = "blue",size = 5)) + 
                scale_y_continuous(label=scales::comma)
              
              ggplotly(p, tooltip="text")
              
            } else if (input$Group == "Exporter"){
              
              plot_data <- plot_data%>%
                group_by(Exporter)%>%
                summarise(Total_Mass=sum(Gross_Mass_KGS))%>%
                arrange(desc(Total_Mass))%>%
                top_n(50)
              
              p <- ggplot(plot_data, aes(x=reorder(Exporter, -Total_Mass), y=Total_Mass))+
                geom_bar(stat="identity", aes(text=sprintf("Exporter: %s<br>Gross Mass: %s KG", Exporter, prettyNum(round(Total_Mass, 1),big.mark=",",scientific=FALSE))),
                         col="white", fill="light blue") +
                  labs(x="Exporter", y="Gross Mass(KG)") + 
                    theme(axis.text.x = element_text(angle = 45, size = 5, color = 'blue'), 
                          axis.text.y = element_text(color = "blue",size = 5)) + 
                scale_y_continuous(label=scales::comma)
              
              ggplotly(p, tooltip="text")
              
            } else if (input$Group == "Country_Origin") {
              
              plot_data <- plot_data%>%
                group_by(NAME_LONG)%>%
                summarise(Total_Mass=sum(Gross_Mass_KGS))%>%
                arrange(desc(Total_Mass))
              
              p <- ggplot(plot_data, aes(x=reorder(NAME_LONG, -Total_Mass), y=Total_Mass))+
                geom_bar(stat="identity", aes(text=sprintf("Country: %s<br>Gross Mass: %s KG", NAME_LONG, prettyNum(round(Total_Mass, 1),big.mark=",",scientific=FALSE))), 
                         col="white", fill="light blue") +
                  labs(x="Country", y="Gross Mass(KG)") + 
                    theme(axis.text.x = element_text(angle = 45, size = 5, color = 'blue'), 
                          axis.text.y = element_text(color = "blue",size = 5)) + 
                    scale_y_continuous(label=scales::comma)
              
              ggplotly(p, tooltip="text")
            }
            
          } else if (input$Y == "Customs_Value"){
            
            if (input$Group == "Importer"){
              
              plot_data <- plot_data%>%
                group_by(Consignee)%>%
                summarise(Total_Customs_Value=sum(Customs_Value))%>%
                arrange(desc(Total_Customs_Value))%>%
                top_n(50)
              
              p <- ggplot(plot_data, aes(x=reorder(Consignee, -Total_Customs_Value), y=Total_Customs_Value))+
                geom_bar(stat="identity", aes(text=sprintf("Importer: %s<br>Customs Value: %s %s", Consignee, prettyNum(round(Total_Customs_Value, 1),big.mark=",",scientific=FALSE), means)),
                         col="white", fill="light blue") +
                  labs(x="Importer", y="Total Customs Value") + 
                    theme(axis.text.x = element_text(angle = 45, size = 5, color = 'blue'), 
                          axis.text.y = element_text(color = "blue",size = 5)) + 
                scale_y_continuous(label=scales::comma)
              
              ggplotly(p, tooltip="text")
              
            } else if (input$Group == "Exporter"){
              
              plot_data <- plot_data%>%
                group_by(Exporter)%>%
                summarise(Total_Customs_Value=sum(Customs_Value))%>%
                arrange(desc(Total_Customs_Value))%>%
                top_n(50)
              
              p <- ggplot(plot_data, aes(x=reorder(Exporter, -Total_Customs_Value), y=Total_Customs_Value))+
                geom_bar(stat="identity", aes(text=sprintf("Exporter: %s<br>Customs Value: %s %s", Exporter, prettyNum(round(Total_Customs_Value, 1),big.mark=",",scientific=FALSE), means)),
                         col="white", fill="light blue") +
                  labs(x="Exporter", y="Total Customs Value") + 
                    theme(axis.text.x = element_text(angle = 45, size = 5, color = 'blue'), 
                          axis.text.y = element_text(color = "blue",size = 5)) + 
                scale_y_continuous(label=scales::comma)
              
              ggplotly(p, tooltip="text")
              
            } else if (input$Group == "Country_Origin") {
              
              plot_data <- plot_data%>%
                group_by(NAME_LONG)%>%
                summarise(Total_Customs_Value=sum(Customs_Value))%>%
                arrange(desc(Total_Customs_Value))
              
              p <- ggplot(plot_data, aes(x=reorder(NAME_LONG, -Total_Customs_Value), y=Total_Customs_Value))+
                geom_bar(stat="identity", aes(text=sprintf("Country: %s<br>Customs Value: %s %s", NAME_LONG, prettyNum(round(Total_Customs_Value, 1),big.mark=",",scientific=FALSE), means)),
                         col="white", fill="light blue") +
                  labs(x="Country", y="Total Customs Value") + 
                    theme(axis.text.x = element_text(angle = 45, size = 5, color = 'blue'), 
                          axis.text.y = element_text(color = "blue",size = 5)) + 
                scale_y_continuous(label=scales::comma)
              
              ggplotly(p, tooltip="text")
            }
            
          } else if (input$Y == "Unit_Price"){
            
            if (input$Group == "Importer"){
              
              plot_data <- plot_data%>%
                group_by(Consignee)%>%
                summarise(Unit_Price=sum(Customs_Value)/sum(Gross_Mass_KGS))%>%
                arrange(desc(Unit_Price))%>%
                top_n(50)
              
              p <- ggplot(plot_data, aes(x=reorder(Consignee, -Unit_Price), y=Unit_Price))+
                geom_bar(stat="identity", aes(text=sprintf("Importer: %s<br>Unit Price: %s %s/KG", Consignee, prettyNum(round(Unit_Price, 1),big.mark=",",scientific=FALSE), means)),
                         col="white", fill="light blue") +
                  labs(x="Importer", y="Unit Price") + 
                    theme(axis.text.x = element_text(angle = 45, size = 5, color = 'blue'), 
                          axis.text.y = element_text(color = "blue",size = 5)) + 
                scale_y_continuous(label=scales::comma)
              
              ggplotly(p, tooltip="text")
              
            } else if (input$Group == "Exporter"){
              
              plot_data <- plot_data%>%
                group_by(Exporter)%>%
                summarise(Unit_Price=sum(Customs_Value)/sum(Gross_Mass_KGS))%>%
                arrange(desc(Unit_Price))%>%
                top_n(50)
              
              p <- ggplot(plot_data, aes(x=reorder(Exporter, -Unit_Price), y=Unit_Price))+
                geom_bar(stat="identity", aes(text=sprintf("Exporter: %s<br>Unit Price: %s %s/KG", Exporter, prettyNum(round(Unit_Price, 1),big.mark=",",scientific=FALSE), means)),
                         col="white", fill="light blue") +
                  labs(x="Exporter", y="Unit Price") + 
                    theme(axis.text.x = element_text(angle = 45, size = 5, color = 'blue'), 
                          axis.text.y = element_text(color = "blue",size = 5)) + 
                scale_y_continuous(label=scales::comma)
              
              ggplotly(p, tooltip="text")
              
            } else if (input$Group == "Country_Origin") {
              
              plot_data <- plot_data%>%
                group_by(NAME_LONG)%>%
                summarise(Unit_Price=sum(Customs_Value)/sum(Gross_Mass_KGS))%>%
                arrange(desc(Unit_Price))
              
              p <- ggplot(plot_data, aes(x=reorder(NAME_LONG, -Unit_Price), y=Unit_Price))+
                geom_bar(stat="identity", aes(text=sprintf("Country: %s<br>Unit Price: %s %s/KG", NAME_LONG, prettyNum(round(Unit_Price, 1),big.mark=",",scientific=FALSE), means)),
                         col="white", fill="light blue") +
                  labs(x="Country", y="Unit Price") + 
                    theme(axis.text.x = element_text(angle = 45, size = 5, color = 'blue'), 
                          axis.text.y = element_text(color = "blue",size = 5)) + 
                scale_y_continuous(label=scales::comma)
              
              ggplotly(p, tooltip="text")
            }
          }
        }
    })
    
    # end of bar plot
    
    
    # start of point plot
    
    output$time <- renderPlotly({
      if (input$Start_Date < input$End_Date){
        
        import$Date = as.Date(import$Date)
        time_data <- import[which((import$HS_Code == HS[[input$Product]]) & (import$Date >= input$Start_Date) & (import$Date <= input$End_Date)),]
        
        if (input$Timeseries_Currency == "PHP"){
          time_data$Customs_Value <- time_data$Customs_Value_PHP
          means = "PHP"
        } else{
          time_data$Customs_Value <- time_data$Customs_Value_USD
          means = "USD"
        }
        
        if (input$Timeseries_Comparison_Y == "Gross_Mass"){
          
          if (input$Timeseries_Comparison_Group == "Importer"){
            
            time_data <- time_data[which(time_data$Consignee == input$Comparison_Object_1 | time_data$Consignee == input$Comparison_Object_2),]
            time_data <- time_data%>%
              group_by(Consignee, Date)%>%
              summarise(Total_Mass=sum(Gross_Mass_KGS))
            
            p <- ggplot(time_data, aes(x = Date, y = Total_Mass)) + 
                   geom_point(aes(color = Consignee, text=sprintf("Importer: %s<br>Total Mass: %s KG", Consignee, 
                                                                  prettyNum(round(Total_Mass, 1),big.mark=",",scientific=FALSE)))) +
                     labs(x="Date", y="Gross Mass(KG)") + 
                       scale_x_date(date_labels = "%b/%Y") + 
                        scale_y_continuous(label=scales::comma) + 
                          theme(legend.title = element_blank())
            
            if (input$Line){
              p <- p + geom_line(aes(color = Consignee))
            }
            
            if (input$Smooth){
              p <- p + geom_smooth(method = "loess", aes(color = Consignee))
            }
            
            ggplotly(p, tooltip="text")
            
          } else if (input$Timeseries_Comparison_Group == "Exporter"){
            
            time_data <- time_data[which(time_data$Exporter == input$Comparison_Object_1 | time_data$Exporter == input$Comparison_Object_2),]
            time_data <- time_data%>%
              group_by(Exporter, Date)%>%
              summarise(Total_Mass=sum(Gross_Mass_KGS))
            
            p <- ggplot(time_data, aes(x = Date, y = Total_Mass)) + 
                  geom_point(aes(color = Exporter, text=sprintf("Exporter: %s<br>Total Mass(KG): %s KG", Exporter, 
                                                             prettyNum(round(Total_Mass, 1),big.mark=",",scientific=FALSE)))) +
                     labs(x="Date", y="Gross Mass(KG)") +
                       scale_x_date(date_labels = "%b/%Y") + 
                        scale_y_continuous(label=scales::comma) + 
                          theme(legend.title = element_blank())
            
            if (input$Line){
              p <- p + geom_line(aes(color = Exporter))
            }
            
            if (input$Smooth){
              p <- p + geom_smooth(method = "loess", aes(color = Exporter))
            }
            
            ggplotly(p, tooltip="text")
            
          } else if (input$Timeseries_Comparison_Group == "Country_Origin") {
            
            time_data <- time_data[which(time_data$NAME_LONG == input$Comparison_Object_1 | time_data$NAME_LONG == input$Comparison_Object_2),]
            time_data <- time_data%>%
              group_by(NAME_LONG, Date)%>%
              summarise(Total_Mass=sum(Gross_Mass_KGS))
            
            p <- ggplot(time_data, aes(x = Date, y = Total_Mass)) + 
                  geom_point(aes(color = NAME_LONG, text=sprintf("Country: %s<br>Total Mass(KG): %s KG", NAME_LONG, 
                                                             prettyNum(round(Total_Mass, 1),big.mark=",",scientific=FALSE)))) +
                     labs(x="Date", y="Gross Mass(KG)") +
                       scale_x_date(date_labels = "%b/%Y") + 
                        scale_y_continuous(label=scales::comma) + 
                          theme(legend.title = element_blank())
            
            if (input$Line){
              p <- p + geom_line(aes(color = NAME_LONG))
            }
            
            if (input$Smooth){
              p <- p + geom_smooth(method = "loess", aes(color = NAME_LONG))
            }
            
            ggplotly(p, tooltip="text")
          }
          
        } else if (input$Timeseries_Comparison_Y == "Customs_Value"){
          
          if (input$Timeseries_Comparison_Group == "Importer"){
            
            time_data <- time_data[which(time_data$Consignee == input$Comparison_Object_1 | time_data$Consignee == input$Comparison_Object_2),]
            time_data <- time_data%>%
              group_by(Consignee, Date)%>%
              summarise(Total_Customs_Value=sum(Customs_Value))
            
            p <- ggplot(time_data, aes(x = Date, y = Total_Customs_Value)) + 
                  geom_point(aes(color = Consignee, text=sprintf("Importer: %s<br>Total Custom Value: %s %s", Consignee, 
                                                             prettyNum(round(Total_Customs_Value, 1),big.mark=",",scientific=FALSE), means))) +
                     labs(x="Date", y="Total Customs Value") + 
                       scale_x_date(date_labels = "%b/%Y") + 
                        scale_y_continuous(label=scales::comma) + 
                          theme(legend.title = element_blank())
            
            if (input$Line){
              p <- p + geom_line(aes(color = Consignee))
            }
            
            if (input$Smooth){
              p <- p + geom_smooth(method = "loess", aes(color = Consignee))
            }
            
            ggplotly(p, tooltip="text")
            
          } else if (input$Timeseries_Comparison_Group == "Exporter"){
            
            time_data <- time_data[which(time_data$Exporter == input$Comparison_Object_1 | time_data$Exporter == input$Comparison_Object_2),]
            time_data <- time_data%>%
              group_by(Exporter, Date)%>%
              summarise(Total_Customs_Value=sum(Customs_Value))
            
            p <- ggplot(time_data, aes(x = Date, y = Total_Customs_Value)) + 
                  geom_point(aes(color = Exporter, text=sprintf("Exporter: %s<br>Total Custom Value: %s %s", Exporter, 
                                                             prettyNum(round(Total_Customs_Value, 1),big.mark=",",scientific=FALSE), means))) +
                     labs(x="Date", y="Total Customs Value") + 
                       scale_x_date(date_labels = "%b/%Y") + 
                        scale_y_continuous(label=scales::comma) + 
                          theme(legend.title = element_blank())
            
            if (input$Line){
              p <- p + geom_line(aes(color = Exporter))
            }
            
            if (input$Smooth){
              p <- p + geom_smooth(method = "loess", aes(color = Exporter))
            }
            
            ggplotly(p, tooltip="text")
            
          } else if (input$Timeseries_Comparison_Group == "Country_Origin") {
            
            time_data <- time_data[which(time_data$NAME_LONG == input$Comparison_Object_1 | time_data$NAME_LONG == input$Comparison_Object_2),]
            time_data <- time_data%>%
              group_by(NAME_LONG, Date)%>%
              summarise(Total_Customs_Value=sum(Customs_Value))
            
            p <- ggplot(time_data, aes(x = Date, y = Total_Customs_Value)) + 
              geom_point(aes(color = NAME_LONG, text=sprintf("Country: %s<br>Total Custom Value: %s %s", NAME_LONG, 
                                                             prettyNum(round(Total_Customs_Value, 1),big.mark=",",scientific=FALSE), means))) +
              labs(x="Date", y="Total Customs Value") + 
              scale_x_date(date_labels = "%b/%Y") + 
              scale_y_continuous(label=scales::comma) +
              theme(legend.title = element_blank())
            
            if (input$Line){
              p <- p + geom_line(aes(color = NAME_LONG))
            }
            
            if (input$Smooth){
              p <- p + geom_smooth(method = "loess", aes(color = NAME_LONG))
            }
            
            ggplotly(p, tooltip="text")
          }
          
        } else if (input$Timeseries_Comparison_Y == "Unit_Price"){
          
          if (input$Timeseries_Comparison_Group == "Importer"){
            
            time_data <- time_data[which(time_data$Consignee == input$Comparison_Object_1 | time_data$Consignee == input$Comparison_Object_2),]
            time_data <- time_data%>%
              group_by(Consignee, Date)%>%
              summarise(Unit_Price=sum(Customs_Value)/sum(Gross_Mass_KGS))
            
            p <- ggplot(time_data, aes(x = Date, y = Unit_Price)) + 
              geom_point(aes(color = Consignee, text=sprintf("Importer: %s<br>Unit_Price: %s %s/KG", Consignee, 
                                                             prettyNum(round(Unit_Price, 1),big.mark=",",scientific=FALSE), means))) +
              labs(x="Date", y="Unit Price") + 
              scale_x_date(date_labels = "%b/%Y") + 
              scale_y_continuous(label=scales::comma) + 
              theme(legend.title = element_blank())
            
            if (input$Line){
              p <- p + geom_line(aes(color = Consignee))
            }
            
            if (input$Smooth){
              p <- p + geom_smooth(method = "loess", aes(color = Consignee))
            }
            
            ggplotly(p, tooltip="text")
            
          } else if (input$Timeseries_Comparison_Group == "Exporter"){
            
            time_data <- time_data[which(time_data$Exporter == input$Comparison_Object_1 | time_data$Exporter == input$Comparison_Object_2),]
            time_data <- time_data%>%
              group_by(Exporter, Date)%>%
              summarise(Unit_Price=sum(Customs_Value)/sum(Gross_Mass_KGS))
            
            p <- ggplot(time_data, aes(x = Date, y = Unit_Price)) + 
              geom_point(aes(color = Exporter, text=sprintf("Exporter: %s<br>Unit Price: %s %s/KG", Exporter, 
                                                             prettyNum(round(Unit_Price, 1),big.mark=",",scientific=FALSE), means))) +
              labs(x="Date", y="Unit Price") + 
              scale_x_date(date_labels = "%b/%Y") + 
              scale_y_continuous(label=scales::comma) + 
              theme(legend.title = element_blank())
            
            if (input$Line){
              p <- p + geom_line(aes(color = Exporter))
            }
            
            if (input$Smooth){
              p <- p + geom_smooth(method = "loess", aes(color = Exporter))
            }
            
            ggplotly(p, tooltip="text")
            
          } else if (input$Timeseries_Comparison_Group == "Country_Origin") {
            
            time_data <- time_data[which(time_data$NAME_LONG == input$Comparison_Object_1 | time_data$NAME_LONG == input$Comparison_Object_2),]
            time_data <- time_data%>%
              group_by(NAME_LONG, Date)%>%
              summarise(Unit_Price=sum(Customs_Value)/sum(Gross_Mass_KGS))
            
            p <- ggplot(time_data, aes(x = Date, y = Unit_Price)) + 
              geom_point(aes(color = NAME_LONG, text=sprintf("Country: %s<br>Unit Price: %s %s/KG", NAME_LONG, 
                                                             prettyNum(round(Unit_Price, 1),big.mark=",",scientific=FALSE), means))) +
              labs(x="Date", y="Unit Price") + 
              scale_x_date(date_labels = "%b/%Y") + 
              scale_y_continuous(label=scales::comma) + 
              theme(legend.title = element_blank())
            
            if (input$Line){
              p <- p + geom_line(aes(color = NAME_LONG))
            }
            
            if (input$Smooth){
              p <- p + geom_smooth(method = "loess", aes(color = NAME_LONG))
            }
            
            ggplotly(p, tooltip="text")
          }
        }
      }
    })
    
    # end of point plot
}

# Run the application 
shinyApp(ui = ui, server = server)