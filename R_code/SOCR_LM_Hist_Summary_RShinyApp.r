library(plotly)
library(tidyverse)

# available to each user session
dataset <- txhousing

# Imputation of categorical variables using Mode
getmode <- function(v) {
  v = v[nchar(as.character(v))>0]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Imputation
medianModeImputation <- function (df) {
  for (cols in colnames(df)) {
    if (cols %in% names(df[,sapply(df, is.numeric)]))  ## Numeric variables first, then the Categorical
      df<-df %>% mutate(!!cols := replace(!!rlang::sym(cols), is.na(!!rlang::sym(cols)), mean(!!rlang::sym(cols), na.rm=TRUE)))
    else df <- df %>% mutate(!!cols := replace(!!rlang::sym(cols), !!rlang::sym(cols)=="", getmode(!!rlang::sym(cols))))
  }
  return(df)
}

fitCurrent <- NULL

dataset <- medianModeImputation(dataset)

# named list of features
namedListOfFeatures <- function () {
  namedList         <- as.list(colnames(dataset))
  names (namedList) <- colnames(dataset)
  return(namedList)
}


### UI
ui <- fluidPage(
  titlePanel("Regression Model (Texas Housing Data)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("outcome", label = h3("Outcome (y)"),
                  choices = unique(namedListOfFeatures()), selected = namedListOfFeatures()[5]),
      selectInput("indepvar", label = h3("Explanatory variable (x)"),
                  choices = unique(namedListOfFeatures()), selected = namedListOfFeatures()[6])),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Data", DT::dataTableOutput('tbl')), # Data as datatable
                  tabPanel("Scatterplot", plotlyOutput("scatterplot"),
                           br(), hr(), 
                           withMathJax(
                             paste0("Least Squares Linear Model Estimates"),
                             br(),
                             paste0("Slope: \\(\\hat{\\beta}_1 = \\dfrac{\\big(\\sum^n_{i=1} x_i y_i \\big) - n \\bar{x} 
                                    \\bar{y}}{\\sum^n_{i=1} (x_i- \\bar{x})^2} \\) =",
                                    "\\(\\dfrac{ n \\big(\\sum^n_{i=1} x_i y_i \\big) - 
                                    \\big(\\sum^n_{i=1} x_i \\big) \\big(\\sum^n_{i=1} y_i \\big) }
                                    {n \\sum^n_{i=1} x_i^2 - \\big(\\sum^n_{i=1} x_i \\big)^2} \\)"),
                             br(),
                             paste0("Intercept: \\(\\hat{\\beta}_0 = \\bar{y} - \\hat{\\beta}_1 \\bar{x} \\) "),
                             br(),
                             paste0("Prediction: \\( \\hat{y} = \\hat{\\beta}_0 + \\hat{\\beta}_1 x \\) ")
                           )
                  ), # Scatter Plot
                  tabPanel("Distribution", # Plots of pair of sample histograms (distributions)
                           fluidRow(
                             column(6, plotlyOutput("distribution1")),
                             column(6, plotlyOutput("distribution2")))),
                  tabPanel("Model Summary", verbatimTextOutput("summary")) # Regression output
      )
    )
  ))

### SERVER
server <- function(input, output, session) {
  # Regression output
  output$summary <- renderPrint({
    fit <- lm(unlist(dataset[,input$outcome]) ~ unlist(dataset[,input$indepvar]))
    names(fit$coefficients) <- c("Intercept", input$var2)
    fitCurrent <- fit
    summary(fit)
  })
  
  # Data output
  output$tbl = DT::renderDataTable({
    DT::datatable(dataset, options = list(lengthChange = FALSE))
  })
  
  # Scatterplot output
  output$scatterplot <- renderPlotly({
    plot_ly(x=~unlist(dataset[,input$indepvar]), y=~unlist(dataset[,input$outcome]), 
            type="scatter", mode="markers", name="Data") %>%
               add_lines(x = ~unlist(dataset[,input$indepvar]), 
                         y = ~(lm(unlist(dataset[,input$outcome]) ~ unlist(dataset[,input$indepvar]))$fitted.values), 
                         mode = "lines", name="Linear Model") %>%
               add_lines(x = ~lowess(unlist(dataset[,input$indepvar]), unlist(dataset[,input$outcome]))$x,
                         y = ~lowess(unlist(dataset[,input$indepvar]), unlist(dataset[,input$outcome]))$y,
                         mode = "lines", name="LOESS") %>%
              add_markers(x = mean(unlist(dataset[,input$indepvar])), y = mean(unlist(dataset[,input$outcome])), 
                  name="Center Point", marker=list(size=20, color='green',line=list(color='yellow', width=2))) %>%
              layout(title=paste0("lm(", input$outcome, " ~ ", input$indepvar,
                                   "), Cor(", input$indepvar, ",", input$outcome, ") = ",
                                   round(cor(unlist(dataset[,input$indepvar]), unlist(dataset[,input$outcome])),3)),
                      xaxis=list(title=input$indepvar), yaxis=list(title=input$outcome))})
  
  # Histogram output var 1
  output$distribution1 <- renderPlotly({
    plot_ly(x = ~unlist(dataset[,input$outcome]), type="histogram") %>%
      layout(title=paste0("Distribution"), xaxis=list(title=input$outcome), bargap=0.1)})

  # Histogram output var 2
  output$distribution2 <- renderPlotly({
    plot_ly(x = ~unlist(dataset[,input$indepvar]), type="histogram") %>%
      layout(title=paste0("Distribution"), xaxis=list(title=input$indepvar), bargap=0.1)})
}

### App
shinyApp(ui = ui, server = server)
