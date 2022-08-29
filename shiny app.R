library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("united"),
  # page header
  headerPanel ("Estimating the price of your dream planter"),
  
  # Input values
  sidebarPanel( # for input
    HTML("<h3> Select characters of the planter you want to buy"),
    selectInput("make", label = "Make:",
                choices = list("JohnDeer" = "johndeer", "CaseIH" = "caseih", "Kinze" = "kinze","Other" = 'other')),
    sliderInput(inputId = "produce_year", label = "Produced year", min = 1980, max = 2022, value = 1980, step = 1),
    selectInput("condition", label = "Planter condition",
                choices = list("General Condition" = "general condition", "Good Condition" = "good condition", "Excellent Condition" = "excellent condition")),
    actionButton("submitbutton", "Submit", class = "btn btn-primary")
  ),
  
  mainPanel( # for output
    tags$label(h3("Estimated price:")),
    verbatimTextOutput("contents"),
    tableOutput('tabledata')
  )
)

server <- function(input, output, session) {
  
  #Input data
  datasetInput <- reactive({
    df <- data.frame(JohnDeer = ifelse(input$make == "johndeer", 1, 0),
                     CaseIH = ifelse(input$make == "caseih", 1, 0),
                     Kinze = ifelse(input$make == "kinze", 1, 0),
                     
                     Pro_years = 1 + (2022 - input$produce_year),
                     
                     Cond_good = ifelse(input$condition == "good condition", 1, 0),
                     Cond_ex = ifelse(input$condition == "excellent condition", 1, 0)
                     )
    
   df$Price <- (0.432*df$JohnDeer) + (0.116*df$CaseIH) + (0.116*df$Kinze)+
               (-0.12*df$Pro_years) + (0.0001*(df$Pro_years)^2)  +
               (0.343*df$Cond_good) + (0.471*df$Cond_ex)
   
   df$CI_low <- ((0.432-1.96*0.06)*df$JohnDeer) + ((0.116-1.96*0.07)*df$CaseIH) + ((0.116-1.96*0.067)*df$Kinze)+
                ((-0.12-1.96*0.007)*df$Pro_years) + (0.0001*(df$Pro_years)^2)  +
                ((0.343-1.96*0.112)*df$Cond_good) + ((0.471-1.96*0.122)*df$Cond_ex)
   
   df$CI_hig <- ((0.432+1.96*0.06)*df$JohnDeer) + ((0.116+1.96*0.07)*df$CaseIH) + ((0.116+1.96*0.067)*df$Kinze)+
     ((-0.12+1.96*0.007)*df$Pro_years) + (0.0001*(df$Pro_years)^2)  +
     ((0.343+1.96*0.112)*df$Cond_good) + ((0.471+1.96*0.122)*df$Cond_ex)
   df$CI <- paste("(",round(df$CI_low,2), ",",round(df$CI_hig,2), ")",sep = "")
   OUTPUT <- df[,c("Price","CI")]
   #write.table(df, "df.csv", sep=",")
   
   #test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
   print(OUTPUT)
  })
  
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

shinyApp(ui = ui, server = server)

