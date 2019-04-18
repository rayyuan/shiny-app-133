
library(shiny)
library(ggplot2)
library(reshape)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Saving/Investing Scenarios"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
     column(4,
            
              sliderInput("init",
                          "Initial Amount",
                          min = 1,
                          max = 100000,
                          pre = "$",
                          sep = ",",
                          value = 1000)
            
     )
     ,
     column(4,
            
            sliderInput("ret_rate",
                        "Return Rate(in %)",
                        min = 0,
                        max = 20,
                        value = 5)
            
     ),
     column(4,
            
            sliderInput("years",
                        "Years",
                        min = 0,
                        max = 50,
                        value = 10)
            
     ),
     column(4,
            
            sliderInput("ann_contrib",
                        "Annual Contribution",
                        min = 0,
                        max = 50000,
                        pre = "$",
                        sep = ",",
                        value = 2000)
            
     ),
     column(4,
            
            sliderInput("growth",
                        "Growth Rate(in %)",
                        min = 0,
                        max = 20,
                        value = 2)
            
     ),
     column(4,
            selectInput("facet",
                        "Facet?",
                        c("No",
                          "Yes"))
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       h3("Timelines"),
       plotOutput("timeline"),
       h3("Balances"),
       verbatimTextOutput("tbl")
     )
   )
)

#' @title Future Value
#' @description This function calculates the expected return over a certain amount of years given a starting principle and an annual rate
#' @param amount Starting Principle Amount (numeric)
#' @param rate Annual rate of return (numeric)
#' @param years Amount of years to calculate interest for (numeric)
#' @return Expected total value after a set number of years compounding at a set rate (numeric)
future_value <- function(amount, rate, years) {
  return (amount * (1 + rate)^years)
}

#' @title Future Value of Annuity
#' @description Calculates the expected future value of annuity given an annual contribution along with the annual rate of return and the amount of years to calculate
#' @param contrib Amount deposited at the end of each year (numeric)
#' @param rate Annual rate of return (numeric)
#' @param years Amount of years to calculate future value of annuity for (numeric)
#' @return Expected total value of annuity (numeric)
annuity <- function(contrib, rate, years) {
  return(contrib * (((1 + rate)^years - 1) / rate))
}

#' @title Future Value of Growing Annuity
#' @description Calculates the expected future value of annuity given a growing annual contribution along with the annual rate of return and the amount of years to calculate
#' @param contrib Amount deposited at the end of each year (numeric)
#' @param rate Annual rate of return (numeric)
#' @param growth Annual rate of growth of annual contribution (numeric)
#' @param years Amount of years to calculate future value of growing annuity for (numeric)
#' @return Expected total value of growing annuity (numeric)
growing_annuity <- function(contrib, rate, growth, years) {
  growth <- ((1 + rate)^years - (1 + growth)^years) / (rate - growth)
  return(contrib * growth)
}


# Define server logic required to draw a histogram
server <- function(input, output) {
  
   output$tbl <- renderPrint({
     none1 <- c()
     none2 <- c()
     fixed <- c()
     growing <- c()
     
     
     for (i in 0:input$years) {
       n <- future_value(input$init, input$ret_rate/100, i)
       f <- n + annuity(input$ann_contrib, input$ret_rate/100, i)
       none1 <- c(none1, n)
       fixed <- c(fixed, f)
     }
     for (i in 0:input$years) {
       n <- future_value(input$init, input$ret_rate/100, i)
       g <- n + growing_annuity(input$ann_contrib, input$ret_rate/100, input$growth/100, i)
       none2 <- c(none2, n)
       growing <- c(growing, g)
     }
     
     modalities <- data.frame(years = 0:input$years, no_contrib = none2, fixed_contrib = fixed, growing_contrib = growing)
     print(modalities)
   })
   
   output$timeline <- renderPlot({
     
     none1 <- c()
     none2 <- c()
     fixed <- c()
     growing <- c()
     
     
     for (i in 0:input$years) {
       n <- future_value(input$init, input$ret_rate/100, i)
       f <- n + annuity(input$ann_contrib, input$ret_rate/100, i)
       none1 <- c(none1, n)
       fixed <- c(fixed, f)
     }
     for (i in 0:input$years) {
       n <- future_value(input$init, input$ret_rate/100, i)
       g <- n + growing_annuity(input$ann_contrib, input$ret_rate/100, input$growth/100, i)
       none2 <- c(none2, n)
       growing <- c(growing, g)
     }
     
     modalities <- data.frame(years = 0:input$years, no_contrib = none2, fixed_contrib = fixed, growing_contrib = growing)
     
     faceted_list <- melt(modalities,id=c("years"))
     
     print(modalities)
     
     if(input$facet == "No"){
       plot(0:input$years, modalities$no_contrib, type="l", col="red", xlab="years", ylab="Value ($)", ylim = c(0,max(faceted_list$value, na.rm = TRUE)), main = "Growth of Principle Using Differing Amounts of Annual Contribution") + lines(0:input$years, modalities$fixed_contrib, col="green") + lines(0:input$years, modalities$growing_contrib ,col="blue")
       legend("topleft", c("No Contrib", "Fixed Contrib", "Growing Contrib"), col = c("red", "green", "blue"), lwd = 2, bty = "n")
     } else if(input$facet == "Yes"){
       

       
       ggplot(data = faceted_list,aes(x = years, y = value, color = variable,fill = variable,group=variable) ) +
         geom_area(alpha = 0.5) +
         facet_wrap(~ variable) + xlab("year") + ylab("balance") + ggtitle("Three Modes of Investing") 
     }
     
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

