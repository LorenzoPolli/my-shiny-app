library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(extrafont)

data = read.csv("data/Superstore Sales Dataset.csv")
data$Order.Date = as.Date(data$Order.Date,format = '%d/%m/%Y')


ui <- fluidPage(
  titlePanel(
    strong("A US Superstore Sales Analysis"),
),

h4("A project by Lorenzo Polli"),
br(),

sidebarLayout(
  sidebarPanel(
    helpText("Choose the segment whose sales data you want to visualize."),
    selectInput(inputId = "selectSegment",
                label = "Select a segment",
                selected = "Corporate",
                choices = c("Consumer","Corporate","Home Office")
                ),
    helpText("Type in the US region where the products are sold:"),
    helpText(" - Central"),
    helpText(" - East"),
    helpText(" - South"),
    helpText(" - West"),
    textInput(inputId = "selectRegion", 
              label = ("Select a Region"), 
              value = "Central"
              ),
    helpText("Choose a date range for your data analysis."),
    dateRangeInput(inputId = "dateRange", 
                   label = "Date range:",
                   start = min(data$Order.Date), 
                   end = max(data$Order.Date),
                   format = "yyyy-mm-dd"
                   ),
    
    br(),
    br(),
    strong("A few Analytic Results..."),

    br(),
    textOutput(outputId = "TotRegionSales"),
    br(),
    h5("The 3 States which got the highest revenues are: "),
    textOutput(outputId = "top3States_1"),
    textOutput(outputId = "top3States_2"),
    textOutput(outputId = "top3States_3"),
    br(),
    textOutput(outputId = "maxRevenue")
    ),
   

  mainPanel(
    textOutput(outputId = "Selected_segment"),
    textOutput(outputId = "Selected_region"),
    textOutput(outputId = "Selected_dates"),
    br(),
    plotOutput(outputId = "sum_sales_plot"),
    plotOutput(outputId = "Time_graph")
  )
 )
)

server <- function(input, output){
  
  output$Selected_segment <- renderText({
    paste("You have selected the segment", input$selectSegment)
  })
  
  output$Selected_dates <- renderText({
    paste("You have chosen a period that goes from ",input$dateRange[1]," to ",input$dateRange[2])
  })
  
  output$Selected_region <- renderText({
    paste("Region selected: ",input$selectRegion)
  })
  
  obtainDataFiltered <- reactive({
    data_filtered <- filter(data, Segment == input$selectSegment,
                            Region == input$selectRegion)
    return(data_filtered)
  })
  
  # This reactive function filter the dataset based on the inputs given. At the end, it groups Sales per each State and sum them  
  
  obtainSalesByState = reactive({
    selection = filter(data, Region == input$selectRegion & Segment == input$selectSegment)
    new_selection = subset(selection, Order.Date > as.Date(input$dateRange[1]) & Order.Date < as.Date(input$dateRange[2]))
    groupSalesByState =  new_selection %>%
      group_by(State) %>%
      summarise(Total = sum(Sales, na.rm = TRUE))
    return(groupSalesByState)
  })
  
  # BAR CHART
  
  output$sum_sales_plot <- renderPlot({
    groupSales <- obtainSalesByState()
    ggplot(groupSales) +
      ggtitle("Aggregate sales by US State for the chosen segment (in USD)") +
      theme(plot.title = element_text(color="#00008b",size = 17, face="bold", family = "Segoe UI Semibold"),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title.x = element_text(size=13, face = "bold", family = "Segoe UI Semibold"),
            axis.title.y = element_text(size=13, face = "bold", family = "Segoe UI Semibold"),
            legend.title = element_text(family = "Segoe UI Semibold")) +
      aes(x=State,y=Total,fill=State) + 
      geom_bar(stat = "identity") + 
      labs(x="US State", y="Total Sales") +
      scale_fill_hue(c = 55)
  })  
  
  # This reactive function filter the dataset based on the inputs given. At the end, it groups Sales per each Order.Date,
  # in all the regions, and sum the Sales values 
  
    obtainSalesByDate = reactive({
    selection = filter(data, Region == input$selectRegion & Segment == input$selectSegment)
    new_selection = subset(selection, Order.Date > as.Date(input$dateRange[1]) & Order.Date < as.Date(input$dateRange[2]))
    groupSalesByDate =  new_selection %>%
      group_by(Order.Date) %>%
      summarise(Total = sum(Sales, na.rm = TRUE))
    return(groupSalesByDate)
  })
  

  # LINE GRAPH
    
  output$Time_graph <- renderPlot({
    dataSegmentRegion <- obtainSalesByDate()
    ggplot(dataSegmentRegion) +
      ggtitle("Aggregate regional sales by date (in USD)") +
      theme(plot.title = element_text(color="#00008b",size = 17, face="bold", family = "Segoe UI Semibold"),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title.x = element_text(size=13, face = "bold", family = "Segoe UI Semibold"),
            axis.title.y = element_text(size=13, face = "bold", family = "Segoe UI Semibold")) +
      aes(x=Order.Date, y=Total) +
      geom_line(color="#03AC13") +
      labs(x="Order Date", y="Total Sales")
  })
  
  # AN.RESULTS N.1) Obtain and display the OVERALL REVENUES made by the selected Region 
  
  sumRegionSales <- reactive({
    selection = filter(data, Region == input$selectRegion)
    selection = sum(selection$Sales)
    return (selection)
  })
  
  output$TotRegionSales <- renderText({
    RegionSales <- sumRegionSales()
    NewRegionSales <- format(round(as.numeric(RegionSales),2), big.mark=",")
    paste("The current region overall sales value is of ", NewRegionSales, "USD")
  })
  
  # AN.RESULTS N.2) Find and display THE TOP 3 STATES by revenues in the selected date range.
  # Then, find the % SALES REVENUE with respect to the region of the segment products sold by the 3 top States in the date range selected
  
  infoTopStates <- reactive({
    selection = filter(data, Region == input$selectRegion & Segment == input$selectSegment)
    new_selection = subset(selection, (Order.Date > input$dateRange[1]) & (Order.Date < input$dateRange[2]))
    sumByState <- aggregate(new_selection$Sales, by=list(State=new_selection$State), FUN=sum)
    sumByStateOrderded = sumByState[order(-sumByState$x),]
    totalRegionalRevenue = sum(sumByStateOrderded$x)
    percRevenueState_1 = format(round(sumByStateOrderded$x[1] / totalRegionalRevenue * 100,2), big.mark=",")
    percRevenueState_2 = format(round(sumByStateOrderded$x[2] / totalRegionalRevenue * 100,2), big.mark=",")
    percRevenueState_3 = format(round(sumByStateOrderded$x[3] / totalRegionalRevenue * 100,2), big.mark=",")
    top3States = head(sumByStateOrderded, n=3)
    top3States$percRevenue = c(percRevenueState_1,percRevenueState_2,percRevenueState_3)
    return (top3States)
  })
  
  # State N.1
  output$top3States_1 <- renderText({
    top3States <- infoTopStates()
    State_1_name <- top3States$State[1]
    State_1_sales <- format(round(as.numeric(top3States$x[1]),2), big.mark=",")
    State_1_perc <- top3States$percRevenue[1]
    paste("1) ", State_1_name, "(", State_1_sales, "USD) - ", State_1_perc, "% of the regional sales value")
  })
  
  #State N.2
  output$top3States_2 <- renderText({
    top3States <- infoTopStates()
    State_2_name <- top3States$State[2]
    State_2_sales <- format(round(as.numeric(top3States$x[2]),2), big.mark=",")
    State_2_perc <- top3States$percRevenue[2]
    paste("2) ", State_2_name, "(", State_2_sales, "USD) - ", State_2_perc, "% of the regional sales value")
  })

  #State N.3
 output$top3States_3 <- renderText({
    top3States <- infoTopStates()
    State_3_name <- top3States$State[3]
    State_3_sales <- format(round(as.numeric(top3States$x[3]),2), big.mark=",")
    State_3_perc <- top3States$percRevenue[3]
    paste("3) ", State_3_name, "(", State_3_sales, "USD) - ", State_3_perc, "% of the regional sales value")
  })


 # AN.RESULTS N.3) Display as text THE DAY IN WHICH THE COMPANY HIT THE HIGHEST REGIONAL REVENUE (by Order Date) 
 output$maxRevenue <- renderText({
   SalesByOrderDate <- obtainSalesByDate()
   maxRevenue = subset(SalesByOrderDate, Total == max(SalesByOrderDate$Total))
   format(round(as.numeric(maxRevenue$Total),2), big.mark=",")
   paste("The highest revenue related to the selected region was registered on the date ", maxRevenue$Order.Date, "and is of ", format(round(maxRevenue$Total,2), big.mark = ","),"USD")
})
 
 
}

shinyApp(ui = ui, server = server)


