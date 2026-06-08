library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(randomForest)
library(caret)

data <- read.csv(
  "data/smartcart_transactions_clean.csv",
  stringsAsFactors = FALSE
)

ui <- dashboardPage(
  dashboardHeader(title = "SmartCart Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Customer Behavior", tabName = "behavior", icon = icon("users")),
      menuItem("Demographics", tabName = "demographics", icon = icon("user")),
      menuItem("Churn Prediction", tabName = "churn", icon = icon("chart-line")),
      menuItem("Products", tabName = "products", icon = icon("shopping-cart")),
      menuItem("Payments", tabName = "payments", icon = icon("credit-card")),
      menuItem("Sales Trends", tabName = "sales", icon = icon("line-chart")),
      menuItem("Correlation", tabName = "correlation", icon = icon("project-diagram")),
      
      hr(),
      
      selectInput("year_filter","Purchase Year",
                  choices=c("All", sort(unique(data$purchase_year)))),
      selectInput("gender_filter","Gender",
                  choices=c("All", unique(data$gender))),
      selectInput("category_filter","Category",
                  choices=c("All", unique(data$product_category)))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(
        tabName="overview",
        fluidRow(
          valueBoxOutput("customers"),
          valueBoxOutput("transactions"),
          valueBoxOutput("revenue")
        ),
        fluidRow(
          valueBoxOutput("avgpurchase"),
          valueBoxOutput("churnrate"),
          valueBoxOutput("returnrate")
        ),
        fluidRow(
          box(width=12,title="Monthly Revenue Trend",
              plotlyOutput("monthlyTrend"))
        ) ,
        fluidRow(
          valueBoxOutput("bestCategory"),
          valueBoxOutput("bestPayment"),
          valueBoxOutput("bestYear")
        ),
        fluidRow(
          box(width=12,title="Reports",
              downloadButton("downloadData","Download Filtered Data"),
              br(),br(),
              downloadButton("downloadSummary","Download Executive Summary"))
        )
      ),
      
      tabItem(
        tabName="behavior",
        fluidRow(
          valueBoxOutput("topCategory"),
          valueBoxOutput("highestRevenueCategory")
        ),
        fluidRow(
          box(width=6,title="Revenue by Product Category",
              plotlyOutput("categoryRevenue")),
          box(width=6,title="Product Category Frequency",
              plotlyOutput("categoryFrequency"))
        ),
        fluidRow(
          box(width=6,title="Revenue by Gender",
              plotlyOutput("genderRevenue")),
          box(width=6,title="Purchase Amount Distribution",
              plotlyOutput("purchaseDistribution"))
        )
      ),
      
      tabItem(
        tabName="demographics",
        fluidRow(
          box(width=6,title="Age Distribution",
              plotlyOutput("ageDistribution")),
          box(width=6,title="Gender Distribution",
              plotlyOutput("genderDistribution"))
        ),
        fluidRow(
          box(width=6,title="Purchase Amount by Gender",
              plotlyOutput("purchaseByGender")),
          box(width=6,title="Purchase Amount by Age Group",
              plotlyOutput("purchaseByAge"))
        ),
        fluidRow(
          box(width=12,title="Age vs Spending",
              plotlyOutput("ageVsSpending"))
        )
      ),
      
      tabItem(
        tabName="churn",
        fluidRow(
          valueBoxOutput("modelAccuracy"),
          valueBoxOutput("churnCustomers"),
          valueBoxOutput("retainedCustomers")
        ),
        fluidRow(
          box(width=6,title="Churn Distribution",
              plotlyOutput("churnDistribution")),
          box(width=6,title="Feature Importance",
              plotlyOutput("featureImportance"))
        ),
        fluidRow(
          box(width=12,title="Customer Churn Risk Simulator",
              numericInput("pred_age","Age",35),
              numericInput("pred_price","Product Price",200),
              numericInput("pred_quantity","Quantity",2),
              selectInput("pred_returns","Returns",c(0,1)),
              actionButton("predict_btn","Predict Churn Risk"),
              br(),br(),
              h3(textOutput("predictionText"))
          )
        )
      ),
      
      tabItem(
        tabName="products",
        fluidRow(
          box(width=6,title="Revenue by Category",
              plotlyOutput("productRevenue")),
          box(width=6,title="Quantity Sold by Category",
              plotlyOutput("productQuantity"))
        )
      ),
      
      tabItem(
        tabName="payments",
        fluidRow(
          box(width=6,title="Payment Method Usage",
              plotlyOutput("paymentUsage")),
          box(width=6,title="Revenue by Payment Method",
              plotlyOutput("paymentRevenue"))
        )
      ),
      
      tabItem(
        tabName="sales",
        fluidRow(
          box(width=6,title="Monthly Revenue",
              plotlyOutput("salesMonthly")),
          box(width=6,title="Quarterly Revenue",
              plotlyOutput("salesQuarterly"))
        ),
        fluidRow(
          box(width=12,title="Yearly Revenue",
              plotlyOutput("salesYearly"))
        )
      ),
      
      tabItem(
        tabName="correlation",
        fluidRow(
          box(width=12,title="Correlation Heatmap",
              plotlyOutput("correlationPlot"))
        ),
        fluidRow(
          box(width=12,title="Correlation Matrix",
              tableOutput("correlationTable"))
        )
      )
    )
  )
)

server <- function(input, output){
  
  filtered_data <- reactive({
    df <- data
    if(input$year_filter != "All") df <- df %>% filter(purchase_year == as.numeric(input$year_filter))
    if(input$gender_filter != "All") df <- df %>% filter(gender == input$gender_filter)
    if(input$category_filter != "All") df <- df %>% filter(product_category == input$category_filter)
    df
  })
  
  model_data <- reactive({
    df <- data
    df$churn <- as.factor(df$churn)
    df
  })
  
  rf_model <- reactive({
    set.seed(123)
    randomForest(churn ~ age + product_price + quantity + returns,
                 data=model_data(), ntree=100)
  })
  
  output$customers <- renderValueBox({
    valueBox(format(length(unique(filtered_data()$customer_id)), big.mark=","),"Customers", icon=icon("users"), color="aqua")
  })
  output$transactions <- renderValueBox({
    valueBox(format(nrow(filtered_data()), big.mark=","),"Transactions", icon=icon("shopping-cart"), color="green")
  })
  output$revenue <- renderValueBox({
    valueBox(paste0("$", format(sum(filtered_data()$total_purchase_amount), big.mark=",")),"Revenue", icon=icon("dollar-sign"), color="yellow")
  })
  output$avgpurchase <- renderValueBox({
    valueBox(round(mean(filtered_data()$total_purchase_amount),2),"Average Purchase", icon=icon("calculator"), color="purple")
  })
  output$churnrate <- renderValueBox({
    valueBox(paste0(round(mean(filtered_data()$churn)*100,2),"%"),"Churn Rate", icon=icon("user-minus"), color="red")
  })
  output$returnrate <- renderValueBox({
    valueBox(paste0(round(mean(filtered_data()$returns)*100,2),"%"),"Return Rate", icon=icon("undo"), color="maroon")
  })
  
  output$monthlyTrend <- renderPlotly({
    monthly <- filtered_data() %>% group_by(purchase_year,purchase_month) %>% summarise(revenue=sum(total_purchase_amount), .groups="drop")
    plot_ly(monthly,x=~purchase_month,y=~revenue,color=~as.factor(purchase_year),type="scatter",mode="lines+markers")
  })
  
  output$categoryRevenue <- renderPlotly({
    df <- filtered_data() %>% group_by(product_category) %>% summarise(revenue=sum(total_purchase_amount), .groups="drop")
    plot_ly(df,x=~product_category,y=~revenue,type="bar")
  })
  output$categoryFrequency <- renderPlotly({
    df <- filtered_data() %>% count(product_category)
    plot_ly(df,labels=~product_category,values=~n,type="pie")
  })
  output$genderRevenue <- renderPlotly({
    df <- filtered_data() %>% group_by(gender) %>% summarise(revenue=sum(total_purchase_amount), .groups="drop")
    plot_ly(df,x=~gender,y=~revenue,type="bar")
  })
  output$purchaseDistribution <- renderPlotly({
    ggplotly(ggplot(filtered_data(), aes(total_purchase_amount))+geom_histogram(bins=30))
  })
  
  output$topCategory <- renderValueBox({
    x <- filtered_data() %>% count(product_category) %>% arrange(desc(n)) %>% slice(1)
    valueBox(x$product_category,"Most Purchased Category",icon=icon("shopping-bag"),color="green")
  })
  output$highestRevenueCategory <- renderValueBox({
    x <- filtered_data() %>% group_by(product_category) %>% summarise(revenue=sum(total_purchase_amount), .groups="drop") %>% arrange(desc(revenue)) %>% slice(1)
    valueBox(x$product_category,"Highest Revenue Category",icon=icon("dollar-sign"),color="yellow")
  })
  
  output$ageDistribution <- renderPlotly({
    ggplotly(ggplot(filtered_data(), aes(age))+geom_histogram(bins=20))
  })
  output$genderDistribution <- renderPlotly({
    df <- filtered_data() %>% count(gender)
    plot_ly(df,labels=~gender,values=~n,type="pie")
  })
  output$purchaseByGender <- renderPlotly({
    ggplotly(ggplot(filtered_data(), aes(gender,total_purchase_amount))+geom_boxplot())
  })
  output$purchaseByAge <- renderPlotly({
    df <- filtered_data()
    df$age_group <- cut(df$age, breaks=c(18,25,35,45,55,65,75), include.lowest=TRUE)
    ggplotly(ggplot(df, aes(age_group,total_purchase_amount))+geom_boxplot())
  })
  output$ageVsSpending <- renderPlotly({
    plot_ly(filtered_data(),x=~age,y=~total_purchase_amount,type="scatter",mode="markers")
  })
  
  output$modelAccuracy <- renderValueBox({
    preds <- predict(rf_model(), model_data())
    acc <- mean(preds == model_data()$churn)
    valueBox(paste0(round(acc*100,2),"%"),"Model Accuracy",icon=icon("bullseye"),color="green")
  })
  output$churnCustomers <- renderValueBox({
    valueBox(sum(filtered_data()$churn==1),"Churned Customers",icon=icon("user-times"),color="red")
  })
  output$retainedCustomers <- renderValueBox({
    valueBox(sum(filtered_data()$churn==0),"Retained Customers",icon=icon("user-check"),color="aqua")
  })
  output$churnDistribution <- renderPlotly({
    df <- filtered_data() %>% count(churn)
    df$Status <- ifelse(df$churn==1,"Churn","Retained")
    plot_ly(df,labels=~Status,values=~n,type="pie")
  })
  output$featureImportance <- renderPlotly({
    imp <- importance(rf_model())
    imp_df <- data.frame(Feature=rownames(imp), Importance=imp[,1])
    plot_ly(imp_df,x=~Feature,y=~Importance,type="bar")
  })
  
  observeEvent(input$predict_btn,{
    new_customer <- data.frame(
      age=input$pred_age,
      product_price=input$pred_price,
      quantity=input$pred_quantity,
      returns=as.numeric(input$pred_returns)
    )
    pred <- predict(rf_model(), new_customer, type="class")
    output$predictionText <- renderText(if(pred=="1") "HIGH RISK OF CHURN" else "LOW RISK OF CHURN")
  })
  
  output$productRevenue <- renderPlotly({
    df <- filtered_data() %>% group_by(product_category) %>% summarise(revenue=sum(total_purchase_amount), .groups="drop")
    plot_ly(df,x=~product_category,y=~revenue,type="bar")
  })
  output$productQuantity <- renderPlotly({
    df <- filtered_data() %>% group_by(product_category) %>% summarise(quantity=sum(quantity), .groups="drop")
    plot_ly(df,x=~product_category,y=~quantity,type="bar")
  })
  output$paymentUsage <- renderPlotly({
    df <- filtered_data() %>% count(payment_method)
    plot_ly(df,labels=~payment_method,values=~n,type="pie")
  })
  output$paymentRevenue <- renderPlotly({
    df <- filtered_data() %>% group_by(payment_method) %>% summarise(revenue=sum(total_purchase_amount), .groups="drop")
    plot_ly(df,x=~payment_method,y=~revenue,type="bar")
  })
  output$salesMonthly <- renderPlotly({
    df <- filtered_data() %>% group_by(purchase_month) %>% summarise(revenue=sum(total_purchase_amount), .groups="drop")
    plot_ly(df,x=~purchase_month,y=~revenue,type="scatter",mode="lines+markers")
  })
  output$salesQuarterly <- renderPlotly({
    df <- filtered_data() %>% group_by(purchase_quarter) %>% summarise(revenue=sum(total_purchase_amount), .groups="drop")
    plot_ly(df,x=~purchase_quarter,y=~revenue,type="bar")
  })
  output$salesYearly <- renderPlotly({
    df <- filtered_data() %>% group_by(purchase_year) %>% summarise(revenue=sum(total_purchase_amount), .groups="drop")
    plot_ly(df,x=~purchase_year,y=~revenue,type="bar")
  })
  
  output$correlationTable <- renderTable({
    corr_df <- filtered_data() %>%
      dplyr::select(age, product_price, quantity,
                    total_purchase_amount, returns, churn)
    round(cor(corr_df), 3)
  })
  
  output$correlationPlot <- renderPlotly({
    corr_df <- filtered_data() %>%
      dplyr::select(age, product_price, quantity,
                    total_purchase_amount, returns, churn)
    corr_matrix <- round(cor(corr_df), 3)
    plot_ly(
      x = colnames(corr_matrix),
      y = rownames(corr_matrix),
      z = corr_matrix,
      type = "heatmap"
    )
  })
  
  output$bestCategory <- renderValueBox({
    x <- filtered_data() %>%
      group_by(product_category) %>%
      summarise(revenue=sum(total_purchase_amount), .groups="drop") %>%
      arrange(desc(revenue)) %>%
      slice(1)
    valueBox(x$product_category,"Top Category",
             icon=icon("award"), color="green")
  })
  
  output$bestPayment <- renderValueBox({
    x <- filtered_data() %>%
      group_by(payment_method) %>%
      summarise(revenue=sum(total_purchase_amount), .groups="drop") %>%
      arrange(desc(revenue)) %>%
      slice(1)
    valueBox(x$payment_method,"Top Payment Method",
             icon=icon("credit-card"), color="yellow")
  })
  
  output$bestYear <- renderValueBox({
    x <- filtered_data() %>%
      group_by(purchase_year) %>%
      summarise(revenue=sum(total_purchase_amount), .groups="drop") %>%
      arrange(desc(revenue)) %>%
      slice(1)
    valueBox(x$purchase_year,"Best Sales Year",
             icon=icon("calendar"), color="aqua")
  })
  
  output$downloadData <- downloadHandler(
    filename=function(){
      paste0("smartcart_filtered_", Sys.Date(), ".csv")
    },
    content=function(file){
      write.csv(filtered_data(), file, row.names=FALSE)
    }
  )
  
  output$downloadSummary <- downloadHandler(
    filename=function(){
      paste0("smartcart_summary_", Sys.Date(), ".csv")
    },
    content=function(file){
      summary_df <- data.frame(
        Customers=length(unique(filtered_data()$customer_id)),
        Transactions=nrow(filtered_data()),
        Revenue=sum(filtered_data()$total_purchase_amount),
        ChurnRate=mean(filtered_data()$churn)
      )
      write.csv(summary_df, file, row.names=FALSE)
    }
  )
  
}

shinyApp(ui, server)
