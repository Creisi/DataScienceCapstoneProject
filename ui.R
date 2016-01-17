shinyUI(fluidPage(
   titlePanel("Data Science Capstone Project"),
   h3("Text Prediction App"),
   
   sidebarLayout(
      sidebarPanel(
         submitButton("Predict"),
         h5(""),
         textInput("input", label = ("Type text here to have the next word predicted"), 
                   value = "Enter text...") ,
         textInput("answer", label = ("Type the next word in your text from above to compare to the answers the algorithm provides"), value = "Enter answer...") ,
         radioButtons("freq", label = ("Find Associations Calculation"),
                      choices = list("On" = TRUE, "Off" = FALSE), selected = FALSE),
         h5("Turning Find Associations ON may improve the accuracy of the algorithm, however, this will increase the time it takes to work"),
         h5("It would be worth a try once or twice if the standard algorithm doesn't give you the right answer.")
         # put a submit buttton some where),
      ),
      mainPanel(
         verbatimTextOutput("PredictFUN"),
         plotOutput("trigramPlot")
         )
   )
))
